open Core
open Msgpack

let%expect_test "basic array" =
  let msg = Array [ Int 5; String "abc"; Bool true ] in
  let serialized = string_of_t_exn msg in
  let msg' = t_of_string_exn serialized in
  printf !"%{sexp:t}" msg';
  [%expect {| (Array ((Int 5) (String abc) (Bool true))) |}]
;;

let%expect_test "big integer" =
  let msg = Int64 (-3735992885L) in
  let serialized = string_of_t_exn msg in
  let msg' = t_of_string_exn serialized in
  printf !"%{sexp:t}" msg';
  [%expect {| (Int64 -3735992885) |}]
;;

let%expect_test "big integer 2" =
  let msg = Int64 (-6408138865367371111L) in
  let serialized = string_of_t_exn msg in
  let msg' = t_of_string_exn serialized in
  printf !"%{sexp:t}" msg';
  [%expect {| (Int64 -6408138865367371111) |}]
;;

let%expect_test "extension" =
  let msg = Ext { type_id = -48; data = Bytes.of_string "W\xC1I" } in
  let serialized = string_of_t_exn msg in
  let msg' = t_of_string_exn serialized in
  printf !"%{sexp:t}" msg';
  [%expect {| (Ext ((type_id -48) (data "W\193I"))) |}]
;;

let%expect_test "strings" =
  let msg = String "🤔🤔🤔" in
  let serialized = string_of_t_exn msg in
  String.iter ~f:(fun c -> printf "\\x%02x" (Char.to_int c)) serialized;
  [%expect {| \xac\xf0\x9f\xa4\x94\xf0\x9f\xa4\x94\xf0\x9f\xa4\x94 |}]
;;
