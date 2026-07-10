open! Core
module Constants = Msgpack.Internal.Constants

module Pattern = struct
  (* The tag-byte pattern this kind matches. [Range] is used for the fix-* kinds and the
     two fixint ranges; everything else is a single byte. *)
  type t =
    | Single of int
    | Range of int * int
end

module Format = struct
  type t =
    | Positive_fixint
    | Fixmap
    | Fixarray
    | Fixstr
    | Nil
    (* 0xc1 is marked "(never used)" in the msgpack spec, i.e. reserved for future use.
       See https://github.com/msgpack/msgpack/blob/master/spec.md#overview . The parser
       fails when it sees this byte; the variant exists so the byte-range partition check
       below stays exhaustive. *)
    | Reserved_c1
    | False_
    | True_
    | Bin8
    | Bin16
    | Bin32
    | Ext8
    | Ext16
    | Ext32
    | Float32
    | Float64
    | Uint8
    | Uint16
    | Uint32
    | Uint64
    | Int8
    | Int16
    | Int32
    | Int64
    | Fixext1
    | Fixext2
    | Fixext4
    | Fixext8
    | Fixext16
    | Str8
    | Str16
    | Str32
    | Array16
    | Array32
    | Map16
    | Map32
    | Negative_fixint
  [@@deriving enumerate]

  let pattern = function
    | Positive_fixint -> Pattern.Range (0x00, Constants.positive_fixint_unmask)
    | Fixmap -> Range (Constants.fixmap_mask, Constants.fixmap_mask lor 0xf)
    | Fixarray -> Range (Constants.fixarray_mask, Constants.fixarray_mask lor 0xf)
    | Fixstr -> Range (Constants.fixstr_mask, Constants.fixstr_mask lor 0x1f)
    | Nil -> Single (Char.to_int Constants.nil)
    | Reserved_c1 -> Single 0xc1
    | False_ -> Single (Char.to_int Constants.false_)
    | True_ -> Single (Char.to_int Constants.true_)
    | Bin8 -> Single (Char.to_int Constants.bin8_header)
    | Bin16 -> Single (Char.to_int Constants.bin16_header)
    | Bin32 -> Single (Char.to_int Constants.bin32_header)
    | Ext8 -> Single (Char.to_int Constants.ext8_header)
    | Ext16 -> Single (Char.to_int Constants.ext16_header)
    | Ext32 -> Single (Char.to_int Constants.ext32_header)
    | Float32 -> Single (Char.to_int Constants.float32_header)
    | Float64 -> Single (Char.to_int Constants.float64_header)
    | Uint8 -> Single (Char.to_int Constants.uint8_header)
    | Uint16 -> Single (Char.to_int Constants.uint16_header)
    | Uint32 -> Single (Char.to_int Constants.uint32_header)
    | Uint64 -> Single (Char.to_int Constants.uint64_header)
    | Int8 -> Single (Char.to_int Constants.int8_header)
    | Int16 -> Single (Char.to_int Constants.int16_header)
    | Int32 -> Single (Char.to_int Constants.int32_header)
    | Int64 -> Single (Char.to_int Constants.int64_header)
    | Fixext1 -> Single (Char.to_int Constants.fixext1_header)
    | Fixext2 -> Single (Char.to_int Constants.fixext2_header)
    | Fixext4 -> Single (Char.to_int Constants.fixext4_header)
    | Fixext8 -> Single (Char.to_int Constants.fixext8_header)
    | Fixext16 -> Single (Char.to_int Constants.fixext16_header)
    | Str8 -> Single (Char.to_int Constants.str8_header)
    | Str16 -> Single (Char.to_int Constants.str16_header)
    | Str32 -> Single (Char.to_int Constants.str32_header)
    | Array16 -> Single (Char.to_int Constants.array16_header)
    | Array32 -> Single (Char.to_int Constants.array32_header)
    | Map16 -> Single (Char.to_int Constants.map16_header)
    | Map32 -> Single (Char.to_int Constants.map32_header)
    | Negative_fixint -> Range (Constants.negative_fixint_mask, 0xff)
  ;;

  (* The starting byte of this kind's pattern. Used to sort the match arms in ascending
     tag-byte order. *)
  let header t =
    match pattern t with
    | Single b -> b
    | Range (lo, _) -> lo
  ;;

  (* The parser sub-expression to invoke in this kind's match arm.

     For most kinds this is an identifier like ["bin8"] that returns some raw OCaml value
     to be wrapped by [constructor_name]. For the kinds whose parser already returns a
     [Message.t] (like [nil], [uint64]) or a raw [fail] ([Reserved_c1]), this is the full
     arm body and [constructor_name] is [None]. *)
  let parser_name = function
    | Positive_fixint -> "positive_fixint"
    | Fixmap -> "fixmap msg"
    | Fixarray -> "fixarray msg"
    | Fixstr -> "fixstr"
    | Nil -> "nil"
    | Reserved_c1 -> {|fail "msgpack: reserved byte 0xc1"|}
    | False_ -> "false_"
    | True_ -> "true_"
    | Bin8 -> "bin8"
    | Bin16 -> "bin16"
    | Bin32 -> "bin32"
    | Ext8 -> "ext8"
    | Ext16 -> "ext16"
    | Ext32 -> "ext32"
    | Float32 -> "float"
    | Float64 -> "double"
    | Uint8 -> "uint8"
    | Uint16 -> "uint16"
    | Uint32 -> "uint32"
    | Uint64 -> "uint64"
    | Int8 -> "int8"
    | Int16 -> "int16"
    | Int32 -> "int32"
    | Int64 -> "int64"
    | Fixext1 -> "fixext1"
    | Fixext2 -> "fixext2"
    | Fixext4 -> "fixext4"
    | Fixext8 -> "fixext8"
    | Fixext16 -> "fixext16"
    | Str8 -> "str8"
    | Str16 -> "str16"
    | Str32 -> "str32"
    | Array16 -> "array16 msg"
    | Array32 -> "array32 msg"
    | Map16 -> "map16 msg"
    | Map32 -> "map32 msg"
    | Negative_fixint -> "negative_fixint"
  ;;

  (* The [Message.t] constructor used to wrap [parser_name]'s result. [None] means the arm
     body is just [parser_name] as-is (the parser already returns a [Message.t], or is a
     raw [fail]). *)
  let constructor_name = function
    | Nil | Reserved_c1 | False_ | True_ | Uint64 | Int64 -> None
    | Positive_fixint | Uint8 | Uint16 | Uint32 | Int8 | Int16 | Int32 | Negative_fixint
      -> Some "Int"
    | Float32 | Float64 -> Some "Float"
    | Fixmap | Map16 | Map32 -> Some "Map"
    | Fixarray | Array16 | Array32 -> Some "Array"
    | Fixstr | Str8 | Str16 | Str32 -> Some "String"
    | Bin8 | Bin16 | Bin32 -> Some "Binary"
    | Ext8 | Ext16 | Ext32 | Fixext1 | Fixext2 | Fixext4 | Fixext8 | Fixext16 ->
      Some "Ext"
  ;;
end

let pattern_to_string : Pattern.t -> string =
  let hex b = Printf.sprintf "'\\x%02x'" b in
  function
  | Single b -> hex b
  | Range (lo, hi) -> [%string "%{hex lo} .. %{hex hi}"]
;;

(* All formats sorted in ascending tag-byte order. This is the order we want the match
   arms in. *)
let sorted_formats =
  lazy (List.sort Format.all ~compare:(Comparable.lift Int.compare ~f:Format.header))
;;

let%expect_test "Sanity check that there are no gaps and overlaps in the parser ranges" =
  let covers =
    List.fold (force sorted_formats) ~init:0 ~f:(fun next kind ->
      let lo, hi =
        match Format.pattern kind with
        | Single b -> b, b
        | Range (lo, hi) -> lo, hi
      in
      match Int.equal next lo with
      | false ->
        raise_s
          [%message
            "Msgpack_cinaps: kinds do not partition the byte range"
              ~expected_next_byte:(next : int)
              ~got:(lo : int)
              ~pattern:(pattern_to_string (Format.pattern kind) : string)]
      | true -> hi + 1)
  in
  match Int.equal covers 0x100 with
  | true -> ()
  | false ->
    raise_s
      [%message "Msgpack_cinaps: kinds do not cover all 256 tag bytes" (covers : int)]
;;

let print_parser_match () =
  (* We print the whole function definition rather than just the match arms so the cinaps
     block can live at column 0 (cinaps does not indent the generated output, so nesting
     the block would leave each arm at the wrong column). The initial and trailing blank
     lines separate the output from the surrounding [(*$ ... *)] and [(*$*)] markers,
     matching how ocamlformat likes to format around them. *)
  print_endline "";
  print_endline "let parse_by_tag msg c =";
  print_endline "  match c with";
  List.iter (force sorted_formats) ~f:(fun kind ->
    let pat = pattern_to_string (Format.pattern kind) in
    let parser_name = Format.parser_name kind in
    match Format.constructor_name kind with
    | None -> print_endline [%string "  | %{pat} -> %{parser_name}"]
    | Some constructor ->
      print_endline [%string "  | %{pat} ->"];
      print_endline [%string "    let%map v = %{parser_name} in"];
      print_endline [%string "    %{constructor} v"]);
  print_endline ";;";
  print_endline ""
;;
