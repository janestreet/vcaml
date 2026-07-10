open Base
open Angstrom
open Message

(* Angstrom does not expose [Let_syntax]. *)
module Let_syntax = struct
  (* Suppress ``unused value'' warnings *)
  [@@@ocaml.warning "-32"]

  let return = return
  let bind x ~f = x >>= f
  let map x ~f = x >>| f
  let both x y = return (fun x -> x, y) <*> return x
end

let with_header_byte c next =
  let%bind (_ : char) = char c in
  next
;;

let nil = with_header_byte Constants.nil (return Nil)
let true_ = with_header_byte Constants.true_ (return (Bool true))
let false_ = with_header_byte Constants.false_ (return (Bool false))

(* See [Constants] for why these are separate functions.

   Also, see https://github.com/msgpack/msgpack/blob/master/spec.md#formats
*)
let apply_unmask ~unmask ~value = value land unmask
let unmask ~mask ~value = value land lnot mask

let check_mask ~mask ~value =
  let open Int.O in
  value land mask = mask
;;

let positive_fixint =
  let open Int.O in
  let%map c =
    satisfy (fun c ->
      apply_unmask ~unmask:Constants.positive_fixint_unmask ~value:(Char.to_int c)
      = Char.to_int c)
  in
  Char.to_int c
;;

let negative_fixint =
  let%map c =
    satisfy (fun c ->
      check_mask ~mask:Constants.negative_fixint_mask ~value:(Char.to_int c))
  in
  let n = unmask ~mask:Constants.negative_fixint_mask ~value:(Char.to_int c) in
  (* A negative fixint is stored as a 5-bit two's complement integer. To bit-extend this
     to be the correct real integer, we set all bits besides the bottom 5.

     The reason that we use the literal -32 is so that the mask works on 32-bit and 64-bit
     architectures.

     32-bit computer -32: 11111111_11111111_11111111_11100000 64-bit computer -32:
     11111111_11111111_11111111_11111111_11111111_11111111_11111111_11100000
  *)
  let top_bits_mask = -32 in
  n lor top_bits_mask
;;

let uint8 = with_header_byte Constants.uint8_header any_uint8
let uint16 = with_header_byte Constants.uint16_header BE.any_uint16

(* [Angstrom.BE] does not expose uint32 or uint64... *)
let uint32 =
  let%bind (_ : char) = char Constants.uint32_header in
  let%map bs = count 2 BE.any_uint16 in
  (* Because we can't tell angstrom to automatically parse an unsigned 32-bit integer, we
     instead need to roll our own. A number 0xYYZZ is equal to the sum 0xYY << 16 + 0xZZ
     and is stored in big endian as YY ZZ.
  *)
  List.fold ~f:(fun acc v -> (acc lsl 16) + v) ~init:0 bs
;;

let uint64 =
  let%map v = with_header_byte Constants.uint64_header BE.any_int64 in
  Uint64 v
;;

let int8 = with_header_byte Constants.int8_header any_int8
let int16 = with_header_byte Constants.int16_header BE.any_int16

let int32 =
  let%bind result = with_header_byte Constants.int32_header BE.any_int32 in
  match Int32.to_int result with
  | Some i -> return i
  (* This will technically fail back to the outlying choice combinators, but that's fine,
     because the spec is designed such that each of those will fail as well.
  *)
  | None -> fail "int32 value too big for native integers!"
;;

let int64 =
  let%map result = with_header_byte Constants.int64_header BE.any_int64 in
  Int64 result
;;

let float = with_header_byte Constants.float32_header BE.any_float
let double = with_header_byte Constants.float64_header BE.any_double

let fixstr =
  (* We can't really use [check_mask] here, because the bit pattern for [fixstr] is
     [101XXXXX], which will also accept [111XXXXX] if we mask naively.
  *)
  let%bind c =
    satisfy (function
      | '\xa0' .. '\xbf' -> true
      | _ -> false)
  in
  let len = unmask ~mask:Constants.fixstr_mask ~value:(Char.to_int c) in
  take len
;;

let str8 =
  let%bind len = with_header_byte Constants.str8_header any_uint8 in
  take len
;;

let str16 =
  let%bind len = with_header_byte Constants.str16_header BE.any_uint16 in
  take len
;;

let str32 =
  let%bind len = with_header_byte Constants.str32_header BE.any_int32 in
  match Int32.to_int len with
  | Some i -> take i
  | None -> fail "string value is too long!"
;;

let bin8 =
  let%bind len = with_header_byte Constants.bin8_header any_uint8 in
  let%map s = take len in
  Bytes.of_string s
;;

let bin16 =
  let%bind len = with_header_byte Constants.bin16_header BE.any_uint16 in
  let%map s = take len in
  Bytes.of_string s
;;

let bin32 =
  let%bind len = with_header_byte Constants.bin32_header BE.any_int32 in
  match Int32.to_int len with
  | None -> fail "bytes value is too long!"
  | Some i -> take i >>| Bytes.of_string
;;

let fixarray obj_parser =
  let%bind c =
    satisfy (function
      | '\x90' .. '\x9f' -> true
      | _ -> false)
  in
  let len = unmask ~mask:Constants.fixarray_mask ~value:(Char.to_int c) in
  count len obj_parser
;;

let array16 obj_parser =
  let%bind len = with_header_byte Constants.array16_header BE.any_uint16 in
  count len obj_parser
;;

let array32 obj_parser =
  let%bind len = with_header_byte Constants.array32_header BE.any_int32 in
  match Int32.to_int len with
  | None -> fail "array value is too long!"
  | Some i -> count i obj_parser
;;

let pair parser =
  let%bind a = parser in
  let%map b = parser in
  a, b
;;

let fixmap obj_parser =
  let%bind c =
    satisfy (function
      | '\x80' .. '\x8f' -> true
      | _ -> false)
  in
  let len = unmask ~mask:Constants.fixmap_mask ~value:(Char.to_int c) in
  count len (pair obj_parser)
;;

let map16 obj_parser =
  let%bind len = with_header_byte Constants.map16_header BE.any_uint16 in
  count len (pair obj_parser)
;;

let map32 obj_parser =
  let%bind len = with_header_byte Constants.map32_header BE.any_int32 in
  match Int32.to_int len with
  | None -> fail "map value is too long!"
  | Some i -> count i (pair obj_parser)
;;

let create_custom ~type_id ~data = return { Custom.type_id; data = Bytes.of_string data }

let make_fixext_parser ~header ~len =
  let%bind type_id = with_header_byte header any_int8 in
  let%bind data = take len in
  create_custom ~type_id ~data
;;

let fixext1 = make_fixext_parser ~header:Constants.fixext1_header ~len:1
let fixext2 = make_fixext_parser ~header:Constants.fixext2_header ~len:2
let fixext4 = make_fixext_parser ~header:Constants.fixext4_header ~len:4
let fixext8 = make_fixext_parser ~header:Constants.fixext8_header ~len:8
let fixext16 = make_fixext_parser ~header:Constants.fixext16_header ~len:16

let ext8 =
  let%bind len = with_header_byte Constants.ext8_header any_uint8 in
  let%bind type_id = any_int8 in
  let%bind data = take len in
  create_custom ~type_id ~data
;;

let ext16 =
  let%bind len = with_header_byte Constants.ext16_header BE.any_uint16 in
  let%bind type_id = any_int8 in
  let%bind data = take len in
  create_custom ~type_id ~data
;;

let ext32 =
  let%bind len = with_header_byte Constants.ext32_header BE.any_int32 in
  let%bind type_id = any_int8 in
  match Int32.to_int len with
  | None -> fail "map value is too long!"
  | Some i ->
    let%bind data = take i in
    create_custom ~type_id ~data
;;

(* Instead of using [choice] to linearly try each sub-parser (which backtracks on each
   failure), we peek the first byte to determine the msgpack format type upfront and
   dispatch directly to the correct parser. This turns the O(n) search through parsers
   into an O(1) dispatch.

   The [parse_by_tag] function below is generated by cinaps from
   [Msgpack.Internal.Constants], so that the hex patterns here stay in lock-step with the
   actual tag-byte constants rather than being duplicated by hand. *)

(*$ Msgpack_cinaps.print_parser_match () *)
let parse_by_tag msg c =
  match c with
  | '\x00' .. '\x7f' ->
    let%map v = positive_fixint in
    Int v
  | '\x80' .. '\x8f' ->
    let%map v = fixmap msg in
    Map v
  | '\x90' .. '\x9f' ->
    let%map v = fixarray msg in
    Array v
  | '\xa0' .. '\xbf' ->
    let%map v = fixstr in
    String v
  | '\xc0' -> nil
  | '\xc1' -> fail "msgpack: reserved byte 0xc1"
  | '\xc2' -> false_
  | '\xc3' -> true_
  | '\xc4' ->
    let%map v = bin8 in
    Binary v
  | '\xc5' ->
    let%map v = bin16 in
    Binary v
  | '\xc6' ->
    let%map v = bin32 in
    Binary v
  | '\xc7' ->
    let%map v = ext8 in
    Ext v
  | '\xc8' ->
    let%map v = ext16 in
    Ext v
  | '\xc9' ->
    let%map v = ext32 in
    Ext v
  | '\xca' ->
    let%map v = float in
    Float v
  | '\xcb' ->
    let%map v = double in
    Float v
  | '\xcc' ->
    let%map v = uint8 in
    Int v
  | '\xcd' ->
    let%map v = uint16 in
    Int v
  | '\xce' ->
    let%map v = uint32 in
    Int v
  | '\xcf' -> uint64
  | '\xd0' ->
    let%map v = int8 in
    Int v
  | '\xd1' ->
    let%map v = int16 in
    Int v
  | '\xd2' ->
    let%map v = int32 in
    Int v
  | '\xd3' -> int64
  | '\xd4' ->
    let%map v = fixext1 in
    Ext v
  | '\xd5' ->
    let%map v = fixext2 in
    Ext v
  | '\xd6' ->
    let%map v = fixext4 in
    Ext v
  | '\xd7' ->
    let%map v = fixext8 in
    Ext v
  | '\xd8' ->
    let%map v = fixext16 in
    Ext v
  | '\xd9' ->
    let%map v = str8 in
    String v
  | '\xda' ->
    let%map v = str16 in
    String v
  | '\xdb' ->
    let%map v = str32 in
    String v
  | '\xdc' ->
    let%map v = array16 msg in
    Array v
  | '\xdd' ->
    let%map v = array32 msg in
    Array v
  | '\xde' ->
    let%map v = map16 msg in
    Map v
  | '\xdf' ->
    let%map v = map32 msg in
    Map v
  | '\xe0' .. '\xff' ->
    let%map v = negative_fixint in
    Int v
;;

(*$*)

let msg =
  fix (fun msg ->
    let%bind c = peek_char_fail in
    parse_by_tag msg c)
;;

let parse s =
  parse_string ~consume:Prefix msg s |> Result.map_error ~f:(fun s -> Error.of_string s)
;;
