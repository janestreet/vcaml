open! Core
open! Msgpack

let%test_unit "parse . serialize == id" =
  Quickcheck.test (Util.generate ()) ~sexp_of:[%sexp_of: Message.t] ~f:(fun msg ->
    [%test_result: bool]
      (Msgpack.equal (t_of_string_exn (string_of_t_exn msg)) msg)
      ~expect:true)
;;
