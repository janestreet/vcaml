(* tests automatically generated from a subset of
   https://github.com/vsergeev/u-msgpack-python/blob/master/test_umsgpack.py *)

open Core
open Msgpack

let print_hex = String.iter ~f:(fun c -> printf "\\x%02x" (Char.to_int c))

let%expect_test "nil" =
  let obj = Nil in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xc0 |}]
;;

let%expect_test "bool false" =
  let obj = Bool false in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xc2 |}]
;;

let%expect_test "bool true" =
  let obj = Bool true in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xc3 |}]
;;

let%expect_test "7-bit uint0" =
  let obj = Int 0 in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \x00 |}]
;;

let%expect_test "7-bit uint1" =
  let obj = Int 16 in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \x10 |}]
;;

let%expect_test "7-bit uint2" =
  let obj = Int 127 in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \x7f |}]
;;

let%expect_test "5-bit sint0" =
  let obj = Int (-1) in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xff |}]
;;

let%expect_test "5-bit sint1" =
  let obj = Int (-16) in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xf0 |}]
;;

let%expect_test "5-bit sint2" =
  let obj = Int (-32) in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xe0 |}]
;;

let%expect_test "8-bit uint0" =
  let obj = Int 128 in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xcc\x80 |}]
;;

let%expect_test "8-bit uint1" =
  let obj = Int 240 in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xcc\xf0 |}]
;;

let%expect_test "8-bit uint2" =
  let obj = Int 255 in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xcc\xff |}]
;;

let%expect_test "16-bit uint0" =
  let obj = Int 256 in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xcd\x01\x00 |}]
;;

let%expect_test "16-bit uint1" =
  let obj = Int 8192 in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xcd\x20\x00 |}]
;;

let%expect_test "16-bit uint2" =
  let obj = Int 65535 in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xcd\xff\xff |}]
;;

let%expect_test "32-bit uint0" =
  let obj = Int 65536 in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xce\x00\x01\x00\x00 |}]
;;

let%expect_test "32-bit uint1" =
  let obj = Int 2097152 in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xce\x00\x20\x00\x00 |}]
;;

let%expect_test "32-bit uint2" =
  let obj = Int64 4294967295L in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xce\xff\xff\xff\xff |}]
;;

let%expect_test "64-bit uint0" =
  let obj = Int64 4294967296L in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xcf\x00\x00\x00\x01\x00\x00\x00\x00 |}]
;;

let%expect_test "64-bit uint1" =
  let obj = Int64 35184372088832L in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xcf\x00\x00\x20\x00\x00\x00\x00\x00 |}]
;;

let%expect_test "64-bit uint2" =
  let obj = Uint64 Int64.minus_one in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xcf\xff\xff\xff\xff\xff\xff\xff\xff |}]
;;

let%expect_test "8-bit int0" =
  let obj = Int (-33) in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xd0\xdf |}]
;;

let%expect_test "8-bit int1" =
  let obj = Int (-100) in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xd0\x9c |}]
;;

let%expect_test "8-bit int2" =
  let obj = Int (-128) in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xd0\x80 |}]
;;

let%expect_test "16-bit int0" =
  let obj = Int (-129) in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xd1\xff\x7f |}]
;;

let%expect_test "16-bit int1" =
  let obj = Int (-2000) in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xd1\xf8\x30 |}]
;;

let%expect_test "16-bit int2" =
  let obj = Int (-32768) in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xd1\x80\x00 |}]
;;

let%expect_test "32-bit int0" =
  let obj = Int (-32769) in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xd2\xff\xff\x7f\xff |}]
;;

let%expect_test "32-bit int1" =
  let obj = Int (-1000000000) in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xd2\xc4\x65\x36\x00 |}]
;;

let%expect_test "32-bit int2" =
  let obj = Int64 (-2147483648L) in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xd2\x80\x00\x00\x00 |}]
;;

let%expect_test "64-bit int0" =
  let obj = Int64 (-2147483649L) in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xd3\xff\xff\xff\xff\x7f\xff\xff\xff |}]
;;

let%expect_test "64-bit int1" =
  let obj = Int64 (-1000000000000000002L) in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xd3\xf2\x1f\x49\x4c\x58\x9b\xff\xfe |}]
;;

let%expect_test "64-bit int2" =
  let obj = Int64 (-9223372036854775808L) in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xd3\x80\x00\x00\x00\x00\x00\x00\x00 |}]
;;

let%expect_test "64-bit float0" =
  let obj = Float 0.0 in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xcb\x00\x00\x00\x00\x00\x00\x00\x00 |}]
;;

let%expect_test "64-bit float1" =
  let obj = Float 2.5 in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xcb\x40\x04\x00\x00\x00\x00\x00\x00 |}]
;;

let%expect_test "64-bit float2" =
  let obj = Float 1e+35 in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xcb\x47\x33\x42\x61\x72\xc7\x4d\x82 |}]
;;

let%expect_test "fix string0" =
  let obj = String "" in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xa0 |}]
;;

let%expect_test "fix string1" =
  let obj = String "a" in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xa1\x61 |}]
;;

let%expect_test "fix string2" =
  let obj = String "abc" in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xa3\x61\x62\x63 |}]
;;

let%expect_test "wide char string0" =
  let obj = String "Allagbé" in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xa8\x41\x6c\x6c\x61\x67\x62\xc3\xa9 |}]
;;

let%expect_test "wide char string1" =
  let obj = String "По оживлённым берегам" in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect
    {| \xd9\x28\xd0\x9f\xd0\xbe\x20\xd0\xbe\xd0\xb6\xd0\xb8\xd0\xb2\xd0\xbb\xd1\x91\xd0\xbd\xd0\xbd\xd1\x8b\xd0\xbc\x20\xd0\xb1\xd0\xb5\xd1\x80\xd0\xb5\xd0\xb3\xd0\xb0\xd0\xbc |}]
;;

let%expect_test "fixext1" =
  let obj = Ext { type_id = 0x5; data = Bytes.init 1 ~f:(fun _ -> '\x80') } in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xd4\x05\x80 |}]
;;

let%expect_test "fixext2" =
  let obj = Ext { type_id = 0x5; data = Bytes.init 2 ~f:(fun _ -> '\x80') } in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xd5\x05\x80\x80 |}]
;;

let%expect_test "fixext4" =
  let obj = Ext { type_id = 0x5; data = Bytes.init 4 ~f:(fun _ -> '\x80') } in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xd6\x05\x80\x80\x80\x80 |}]
;;

let%expect_test "fixext8" =
  let obj = Ext { type_id = 0x5; data = Bytes.init 8 ~f:(fun _ -> '\x80') } in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xd7\x05\x80\x80\x80\x80\x80\x80\x80\x80 |}]
;;

let%expect_test "fixext16" =
  let obj = Ext { type_id = 0x5; data = Bytes.init 16 ~f:(fun _ -> '\x80') } in
  let s = string_of_t_exn obj in
  print_hex s;
  [%expect {| \xd8\x05\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80 |}]
;;
