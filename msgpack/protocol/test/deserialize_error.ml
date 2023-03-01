open Core
open Msgpack

let%test "insufficient data 8-bit uint" =
  let obj = "\xcc" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data 16-bit uint" =
  let obj = "\xcd\xff" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data 32-bit uint" =
  let obj = "\xce\xff" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data 64-bit uint" =
  let obj = "\xcf\xff" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data 8-bit int" =
  let obj = "\xd0" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data 16-bit int" =
  let obj = "\xd1\xff" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data 32-bit int" =
  let obj = "\xd2\xff" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data 64-bit int" =
  let obj = "\xd3\xff" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data 32-bit float" =
  let obj = "\xca\xff" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data 64-bit float" =
  let obj = "\xcb\xff" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data fixstr" =
  let obj = "\xa1" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data 8-bit string" =
  let obj = "\xd9" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data 8-bit string" =
  let obj = "\xd9\x01" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data 16-bit string" =
  let obj = "\xda\x01\x00" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data 32-bit string" =
  let obj = "\xdb\x00\x01\x00\x00" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data 8-bit binary" =
  let obj = "\xc4" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data 8-bit binary" =
  let obj = "\xc4\x01" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data 16-bit binary" =
  let obj = "\xc5\x01\x00" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data 32-bit binary" =
  let obj = "\xc6\x00\x01\x00\x00" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data fixarray" =
  let obj = "\x91" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data fixarray" =
  let obj = "\x92\xc2" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data 16-bit array" =
  let obj = "\xdc\x00\xf0\xc2\xc3" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data 32-bit array" =
  let obj = "\xdd\x00\x01\x00\x00\xc2\xc3" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data fixmap" =
  let obj = "\x81" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data fixmap" =
  let obj = "\x82\xc2\xc3" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data 16-bit map" =
  let obj = "\xde\x00\xf0\xc2\xc3" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data 32-bit map" =
  let obj = "\xdf\x00\x01\x00\x00\xc2\xc3" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data fixext 1" =
  let obj = "\xd4" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data fixext 1" =
  let obj = "\xd4\x05" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data fixext 2" =
  let obj = "\xd5\x05\x01" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data fixext 4" =
  let obj = "\xd6\x05\x01\x02\x03" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data fixext 8" =
  let obj = "\xd7\x05\x01\x02\x03" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data fixext 16" =
  let obj = "\xd8\x05\x01\x02\x03" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data ext 8-bit" =
  let obj = "\xc7\x05\x05\x01\x02\x03" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data ext 16-bit" =
  let obj = "\xc8\x01\x00\x05\x01\x02\x03" in
  let s = t_of_string obj in
  Or_error.is_error s
;;

let%test "insufficient data ext 32-bit" =
  let obj = "\xc9\x00\x01\x00\x00\x05\x01\x02\x03" in
  let s = t_of_string obj in
  Or_error.is_error s
;;
