open Core
open Msgpack

let equal_int a b =
  match a, b with
  | Int a, Int b -> Int.( = ) a b
  | Int64 a, Int64 b -> Int64.( = ) a b
  | Int64 a, Int b -> Int64.( = ) a (Int64.of_int b)
  | Int a, Int64 b -> Int64.( = ) (Int64.of_int a) b
  | Uint64 a, Uint64 b -> Int64.( = ) a b
  | Uint64 a, Int b -> Int64.( = ) a (Int64.of_int b)
  | Int a, Uint64 b -> Int64.( = ) (Int64.of_int a) b
  | _ -> false
;;

let%test "nil" =
  let obj = "\xc0" in
  let s = t_of_string_exn obj in
  let s' = Nil in
  equal s s'
;;

let%test "bool false" =
  let obj = "\xc2" in
  let s = t_of_string_exn obj in
  let s' = Bool false in
  equal s s'
;;

let%test "bool true" =
  let obj = "\xc3" in
  let s = t_of_string_exn obj in
  let s' = Bool true in
  equal s s'
;;

let%test "7-bit uint0" =
  let obj = "\x00" in
  let s = t_of_string_exn obj in
  let s' = Int 0 in
  equal s s'
;;

let%test "7-bit uint1" =
  let obj = "\x10" in
  let s = t_of_string_exn obj in
  let s' = Int 16 in
  equal s s'
;;

let%test "7-bit uint2" =
  let obj = "\x7f" in
  let s = t_of_string_exn obj in
  let s' = Int 127 in
  equal s s'
;;

let%test "5-bit sint0" =
  let obj = "\xff" in
  let s = t_of_string_exn obj in
  let s' = Int (-1) in
  equal s s'
;;

let%test "5-bit sint1" =
  let obj = "\xf0" in
  let s = t_of_string_exn obj in
  let s' = Int (-16) in
  equal s s'
;;

let%test "5-bit sint2" =
  let obj = "\xe0" in
  let s = t_of_string_exn obj in
  let s' = Int (-32) in
  equal s s'
;;

let%test "8-bit uint0" =
  let obj = "\xcc\x80" in
  let s = t_of_string_exn obj in
  let s' = Int 128 in
  equal s s'
;;

let%test "8-bit uint1" =
  let obj = "\xcc\xf0" in
  let s = t_of_string_exn obj in
  let s' = Int 240 in
  equal s s'
;;

let%test "8-bit uint2" =
  let obj = "\xcc\xff" in
  let s = t_of_string_exn obj in
  let s' = Int 255 in
  equal s s'
;;

let%test "16-bit uint0" =
  let obj = "\xcd\x01\x00" in
  let s = t_of_string_exn obj in
  let s' = Int 256 in
  equal s s'
;;

let%test "16-bit uint1" =
  let obj = "\xcd\x20\x00" in
  let s = t_of_string_exn obj in
  let s' = Int 8192 in
  equal s s'
;;

let%test "16-bit uint2" =
  let obj = "\xcd\xff\xff" in
  let s = t_of_string_exn obj in
  let s' = Int 65535 in
  equal s s'
;;

let%test "32-bit uint0" =
  let obj = "\xce\x00\x01\x00\x00" in
  let s = t_of_string_exn obj in
  let s' = Int 65536 in
  equal s s'
;;

let%test "32-bit uint1" =
  let obj = "\xce\x00\x20\x00\x00" in
  let s = t_of_string_exn obj in
  let s' = Int 2097152 in
  equal s s'
;;

let%test "32-bit uint2" =
  let obj = "\xce\xff\xff\xff\xff" in
  let s = t_of_string_exn obj in
  let s' = Int64 4294967295L in
  equal_int s s'
;;

let%test "64-bit uint0" =
  let obj = "\xcf\x00\x00\x00\x01\x00\x00\x00\x00" in
  let s = t_of_string_exn obj in
  let s' = Uint64 4294967296L in
  equal_int s s'
;;

let%test "64-bit uint1" =
  let obj = "\xcf\x00\x00\x20\x00\x00\x00\x00\x00" in
  let s = t_of_string_exn obj in
  let s' = Uint64 35184372088832L in
  equal_int s s'
;;

let%test "64-bit uint2" =
  let obj = "\xcf\xff\xff\xff\xff\xff\xff\xff\xff" in
  let s = t_of_string_exn obj in
  let s' = Uint64 Int64.minus_one in
  equal_int s s'
;;

let%test "8-bit int0" =
  let obj = "\xd0\xdf" in
  let s = t_of_string_exn obj in
  let s' = Int (-33) in
  equal s s'
;;

let%test "8-bit int1" =
  let obj = "\xd0\x9c" in
  let s = t_of_string_exn obj in
  let s' = Int (-100) in
  equal s s'
;;

let%test "8-bit int2" =
  let obj = "\xd0\x80" in
  let s = t_of_string_exn obj in
  let s' = Int (-128) in
  equal s s'
;;

let%test "16-bit int0" =
  let obj = "\xd1\xff\x7f" in
  let s = t_of_string_exn obj in
  let s' = Int (-129) in
  equal s s'
;;

let%test "16-bit int1" =
  let obj = "\xd1\xf8\x30" in
  let s = t_of_string_exn obj in
  let s' = Int (-2000) in
  equal s s'
;;

let%test "16-bit int2" =
  let obj = "\xd1\x80\x00" in
  let s = t_of_string_exn obj in
  let s' = Int (-32768) in
  equal s s'
;;

let%test "32-bit int0" =
  let obj = "\xd2\xff\xff\x7f\xff" in
  let s = t_of_string_exn obj in
  let s' = Int (-32769) in
  equal s s'
;;

let%test "32-bit int1" =
  let obj = "\xd2\xc4\x65\x36\x00" in
  let s = t_of_string_exn obj in
  let s' = Int (-1000000000) in
  equal s s'
;;

let%test "32-bit int2" =
  let obj = "\xd2\x80\x00\x00\x00" in
  let s = t_of_string_exn obj in
  let s' = Int64 (-2147483648L) in
  equal_int s s'
;;

let%test "64-bit int0" =
  let obj = "\xd3\xff\xff\xff\xff\x7f\xff\xff\xff" in
  let s = t_of_string_exn obj in
  let s' = Int64 (-2147483649L) in
  equal_int s s'
;;

let%test "64-bit int1" =
  let obj = "\xd3\xf2\x1f\x49\x4c\x58\x9b\xff\xfe" in
  let s = t_of_string_exn obj in
  let s' = Int64 (-1000000000000000002L) in
  equal_int s s'
;;

let%test "64-bit int2" =
  let obj = "\xd3\x80\x00\x00\x00\x00\x00\x00\x00" in
  let s = t_of_string_exn obj in
  let s' = Int64 (-9223372036854775808L) in
  equal_int s s'
;;

let%test "64-bit float0" =
  let obj = "\xcb\x00\x00\x00\x00\x00\x00\x00\x00" in
  let s = t_of_string_exn obj in
  let s' = Float 0.0 in
  equal s s'
;;

let%test "64-bit float1" =
  let obj = "\xcb\x40\x04\x00\x00\x00\x00\x00\x00" in
  let s = t_of_string_exn obj in
  let s' = Float 2.5 in
  equal s s'
;;

let%test "64-bit float2" =
  let obj = "\xcb\x47\x33\x42\x61\x72\xc7\x4d\x82" in
  let s = t_of_string_exn obj in
  let s' = Float 1e+35 in
  equal s s'
;;

let%test "fix string0" =
  let obj = "\xa0" in
  let s = t_of_string_exn obj in
  let s' = String "" in
  equal s s'
;;

let%test "fix string1" =
  let obj = "\xa1\x61" in
  let s = t_of_string_exn obj in
  let s' = String "a" in
  equal s s'
;;

let%test "fix string2" =
  let obj = "\xa3\x61\x62\x63" in
  let s = t_of_string_exn obj in
  let s' = String "abc" in
  equal s s'
;;
