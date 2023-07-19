open Core
open Msgpack

let test_string s = string_of_t_exn (String s)

let%test_unit "8-bit string1" =
  [%test_result: string]
    (test_string (String.init 100 ~f:(fun _ -> 'c')))
    ~expect:("\xd9\x64" ^ String.init 100 ~f:(fun _ -> 'c'))
;;

let%test_unit "8-bit string2" =
  [%test_result: string]
    (test_string (String.init 255 ~f:(fun _ -> 'c')))
    ~expect:("\xd9\xff" ^ String.init 255 ~f:(fun _ -> 'c'))
;;

let%test_unit "16-bit string1" =
  [%test_result: string]
    (test_string (String.init 256 ~f:(fun _ -> 'c')))
    ~expect:("\xda\x01\x00" ^ String.init 256 ~f:(fun _ -> 'c'))
;;

let%test_unit "16-bit string2" =
  [%test_result: string]
    (test_string (String.init 0xFFFF ~f:(fun _ -> 'c')))
    ~expect:("\xda\xff\xff" ^ String.init 0xFFFF ~f:(fun _ -> 'c'))
;;

let%test_unit "32-bit string" =
  [%test_result: string]
    (test_string (String.init 65536 ~f:(fun _ -> 'c')))
    ~expect:("\xdb\x00\x01\x00\x00" ^ String.init 65536 ~f:(fun _ -> 'c'))
;;

let test_bytes b = string_of_t_exn (Binary b)

let%test_unit "8-bit binary" =
  [%test_result: string]
    (test_bytes (Bytes.init 255 ~f:(fun _ -> 'c')))
    ~expect:("\xc4\xff" ^ String.init 255 ~f:(fun _ -> 'c'))
;;

let%test_unit "16-bit binary" =
  [%test_result: string]
    (test_bytes (Bytes.init 256 ~f:(fun _ -> 'c')))
    ~expect:("\xc5\x01\x00" ^ String.init 256 ~f:(fun _ -> 'c'))
;;

let%test_unit "32-bit binary" =
  [%test_result: string]
    (test_bytes (Bytes.init 65536 ~f:(fun _ -> 'c')))
    ~expect:("\xc6\x00\x01\x00\x00" ^ String.init 65536 ~f:(fun _ -> 'c'))
;;

let%test_unit "fix array" =
  [%test_result: string]
    (string_of_t_exn (Array [ Int 5; String "abc"; Bool true ]))
    ~expect:"\x93\x05\xa3\x61\x62\x63\xc3"
;;
