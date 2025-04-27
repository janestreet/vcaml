open Core
open Async
open Vcaml
open Vcaml_test_helpers

let%expect_test "create, all_named" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind unnamed = Namespace.create client () in
      let%bind named = Namespace.create client ~name:"foo" () in
      let%map all_named = Namespace.all_named client in
      print_s
        [%message
          (unnamed : Namespace.t)
            (named : Namespace.t)
            (all_named : Namespace.t String.Map.t)])
  in
  [%expect
    {|
    ((unnamed ((id 1) (name ()))) (named ((id 2) (name (foo))))
     (all_named ((foo ((id 2) (name (foo)))))))
    |}];
  return ()
;;
