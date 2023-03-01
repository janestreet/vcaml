open Core
open Msgpack

let%test_unit "parse . serialize == id" =
  Quickcheck.test
    (quickcheck_generator ~only_string_keys:false ~only_finite_floats:false)
    ~sexp_of:[%sexp_of: t]
    ~f:(fun msg ->
      [%test_result: bool]
        (Msgpack.equal (t_of_string_exn (string_of_t_exn msg)) msg)
        ~expect:true)
;;
