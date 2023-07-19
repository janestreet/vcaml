open Core
open Msgpack

let string s = String s

let%test "8-bit string1" =
  let s = "\xd9\x64" ^ String.init 100 ~f:(fun _ -> 'c') |> t_of_string_exn in
  let s' = String (String.init 100 ~f:(fun _ -> 'c')) in
  Msgpack.equal s s'
;;

let%test "8-bit string2" =
  let s = String.init 255 ~f:(fun _ -> 'c') |> string in
  let s' = "\xd9\xff" ^ String.init 255 ~f:(fun _ -> 'c') |> t_of_string_exn in
  Msgpack.equal s s'
;;

let%test "16-bit string1" =
  let s = String.init 256 ~f:(fun _ -> 'c') |> string in
  let s' = "\xda\x01\x00" ^ String.init 256 ~f:(fun _ -> 'c') |> t_of_string_exn in
  Msgpack.equal s s'
;;

let%test "16-bit string2" =
  let s = String.init 0xFFFF ~f:(fun _ -> 'c') |> string in
  let s' = "\xda\xff\xff" ^ String.init 0xFFFF ~f:(fun _ -> 'c') |> t_of_string_exn in
  Msgpack.equal s s'
;;

let%test "32-bit string" =
  let s = String.init 65536 ~f:(fun _ -> 'c') |> string in
  let s' =
    "\xdb\x00\x01\x00\x00" ^ String.init 65536 ~f:(fun _ -> 'c') |> t_of_string_exn
  in
  Msgpack.equal s s'
;;

let bytes b = Binary b

let%test "8-bit binary" =
  let s = bytes (Bytes.init 255 ~f:(fun _ -> 'c')) in
  let s' = t_of_string_exn ("\xc4\xff" ^ String.init 255 ~f:(fun _ -> 'c')) in
  Msgpack.equal s s'
;;

let%test "16-bit binary" =
  let s = bytes (Bytes.init 256 ~f:(fun _ -> 'c')) in
  let s' = t_of_string_exn ("\xc5\x01\x00" ^ String.init 256 ~f:(fun _ -> 'c')) in
  Msgpack.equal s s'
;;

let%test "32-bit binary" =
  let s = bytes (Bytes.init 65536 ~f:(fun _ -> 'c')) in
  let s' =
    t_of_string_exn ("\xc6\x00\x01\x00\x00" ^ String.init 65536 ~f:(fun _ -> 'c'))
  in
  Msgpack.equal s s'
;;

let%test "fix array" =
  let s = Array [ Int 5; String "abc"; Bool true ] in
  let s' = t_of_string_exn "\x93\x05\xa3\x61\x62\x63\xc3" in
  Msgpack.equal s s'
;;

let%test "16-bit array1" =
  let s = Array (List.init 16 ~f:(fun _ -> Int 5)) in
  let s' = "\xdc\x00\x10" ^ String.init ~f:(fun _ -> '\x05') 16 |> t_of_string_exn in
  Msgpack.equal s s'
;;

let%test "16-bit array2" =
  let s = Array (List.init 65535 ~f:(fun _ -> Int 5)) in
  let s' = "\xdc\xff\xff" ^ String.init ~f:(fun _ -> '\x05') 65535 |> t_of_string_exn in
  Msgpack.equal s s'
;;

let%test "32-bit array" =
  let s = Array (List.init 65536 ~f:(fun _ -> Int 5)) in
  let s' =
    "\xdd\x00\x01\x00\x00" ^ String.init ~f:(fun _ -> '\x05') 65536 |> t_of_string_exn
  in
  Msgpack.equal s s'
;;
