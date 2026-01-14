open Base
open Faraday

(* This is currently written with little to no concern about the efficiency of the
   serializer. If performance is necessary, we would want to keep a closer eye on the
   internal buffer and flush occasionally.
*)

let rec dispatch t : Message.t -> unit = function
  | Nil -> write_nil t
  | Int i -> write_int t i
  | Uint64 i -> write_uint64 t i
  | Int64 i -> write_signed_integer t i
  | Bool b -> write_bool t b
  | Float f -> write_float t f
  | Array vs -> write_array t vs
  | Map kvs -> write_map t kvs
  | String s -> write_strval t s
  | Binary bs -> write_bin t bs
  | Ext ext -> write_ext t ext

and write_nil t = write_char t Constants.nil

and write_bool t b =
  match b with
  | true -> write_char t Constants.true_
  | false -> write_char t Constants.false_

and write_int t i =
  if i >= 0
  then write_nonnegative_integer t (Int64.of_int i)
  else write_signed_integer t (Int64.of_int i)

and write_uint64 t i =
  write_char t Constants.uint64_header;
  BE.write_uint64 t i

and write_nonnegative_integer t i =
  let open Int64.O in
  if i <= (2L ** 7L) - 1L
  then
    (* This fits into 8 bits, and we must have the top bit unset here *)
    write_uint8 t (Int64.to_int_exn i)
  else if i <= 0xFFL
  then (
    write_char t Constants.uint8_header;
    write_uint8 t (Int64.to_int_exn i))
  else if i <= 0xFFFFL
  then (
    write_char t Constants.uint16_header;
    BE.write_uint16 t (Int64.to_int_exn i))
  else if i <= 0xFFFF_FFFFL
  then (
    write_char t Constants.uint32_header;
    let v = if i < 2L ** 31L then i else i - (2L ** 32L) in
    BE.write_uint32 t (Int64.to_int32_exn v))
  else (
    write_char t Constants.uint64_header;
    BE.write_uint64 t i)

and write_signed_integer t i =
  let open Int64.O in
  if i >= 0L
  then write_nonnegative_integer t i
  else if -32L <= i
  then write_uint8 t (Int.( lor ) (Int64.to_int_exn i) Constants.negative_fixint_mask)
  else if -(2L ** 7L) <= i
  then (
    write_char t Constants.int8_header;
    (* This is correct by properties of 2s complement arithmetic *)
    write_uint8 t (Int64.to_int_exn i))
  else if -(2L ** 15L) <= i
  then (
    write_char t Constants.int16_header;
    BE.write_uint16 t (Int64.to_int_exn i))
  else if -(2L ** 31L) <= i
  then (
    write_char t Constants.int32_header;
    BE.write_uint32 t (Int64.to_int32_exn i))
  else (
    write_char t Constants.int64_header;
    BE.write_uint64 t i)

and write_float t f =
  write_char t Constants.float64_header;
  BE.write_double t f

(* Don't shadow [Faraday.write_string]. *)
and write_strval t s =
  let open Int.O in
  let len = String.length s in
  if len <= 31
  then write_uint8 t (len lor Constants.fixstr_mask)
  else if len <= (2 ** 8) - 1
  then (
    write_char t Constants.str8_header;
    write_uint8 t len)
  else if len <= (2 ** 16) - 1
  then (
    write_char t Constants.str16_header;
    BE.write_uint16 t len)
  else (
    write_char t Constants.str32_header;
    BE.write_uint32 t (Int32.of_int_exn len));
  write_string t s

and write_bin t b =
  let open Int.O in
  let len = Bytes.length b in
  if len <= (2 ** 8) - 1
  then (
    write_char t Constants.bin8_header;
    write_uint8 t len)
  else if len <= (2 ** 16) - 1
  then (
    write_char t Constants.bin16_header;
    BE.write_uint16 t len)
  else (
    write_char t Constants.bin32_header;
    BE.write_uint32 t (Int32.of_int_exn len));
  write_bytes t b

and write_array t vs =
  let open Int.O in
  let len = List.length vs in
  if len <= 15
  then write_uint8 t (len lor Constants.fixarray_mask)
  else if len <= (2 ** 16) - 1
  then (
    write_char t Constants.array16_header;
    BE.write_uint16 t len)
  else (
    write_char t Constants.array32_header;
    BE.write_uint32 t (Int32.of_int_exn len));
  List.iter ~f:(dispatch t) vs

and write_map t kvs =
  let open Int.O in
  let len = List.length kvs in
  if len <= 15
  then write_uint8 t (len lor Constants.fixmap_mask)
  else if len <= (2 ** 16) - 1
  then (
    write_char t Constants.map16_header;
    BE.write_uint16 t len)
  else (
    write_char t Constants.map32_header;
    BE.write_uint32 t (Int32.of_int_exn len));
  List.iter
    ~f:(fun (k, v) ->
      dispatch t k;
      dispatch t v)
    kvs

(* The spec does not actually say what to do in the case that an extension type takes a
   number of bytes that falls between two of the fixed sizes. Here, we choose to represent
   that as an 8-bit chunk rather than padding with 0s, because those 0s may be meaningful
   to the end-user.
*)
and write_ext t { type_id; data } =
  let open Int.O in
  let len = Bytes.length data in
  if len = 1
  then write_char t Constants.fixext1_header
  else if len = 2
  then write_char t Constants.fixext2_header
  else if len = 4
  then write_char t Constants.fixext4_header
  else if len = 8
  then write_char t Constants.fixext8_header
  else if len = 16
  then write_char t Constants.fixext16_header
  else if len <= (2 ** 8) - 1
  then (
    write_char t Constants.ext8_header;
    write_uint8 t len)
  else if len <= (2 ** 16) - 1
  then (
    write_char t Constants.ext16_header;
    BE.write_uint16 t len)
  else if len <= (2 ** 32) - 1
  then (
    write_char t Constants.ext32_header;
    BE.write_uint32 t (Int32.of_int_exn len))
  else failwith "Ext data too large for messagepack format!";
  write_uint8 t type_id;
  write_bytes t data
;;

let message_to_string_exn ?(bufsize = 256) msg =
  let t = create bufsize in
  dispatch t msg;
  serialize_to_string t
;;
