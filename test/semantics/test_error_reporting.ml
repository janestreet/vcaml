(* The tests in this file demonstrate VCaml's own guarantees about how it behaves. *)
open Core
open Async
open Vcaml
open Vcaml_test_helpers

let%expect_test "Error in the middle of an atomic call is returned correctly" =
  Backtrace.elide := true;
  let%bind () =
    Expect_test_helpers_async.require_does_raise_async [%here] (fun () ->
      with_client (fun client ->
        let open Deferred.Or_error.Let_syntax in
        let get name = Nvim.get_var name ~type_:Boolean in
        let set name value = Nvim.set_var name ~type_:Boolean ~value in
        let%map foo, bar, baz =
          run_join
            [%here]
            client
            (let open Api_call.Or_error.Let_syntax in
             let%map () = set "foo" true
             and () = set "baz" true
             and foo = get "foo"
             and bar = get "bar"
             and baz = get "baz" in
             foo, bar, baz)
        in
        print_s [%message "" (foo : bool) (bar : bool) (baz : bool)]))
  in
  [%expect
    {|
    (("Called from" lib/vcaml/test/semantics/test_error_reporting.ml:LINE:COL)
     (("Vim returned error" "Key not found: bar" (error_type Validation))
      (index 3))) |}];
  Backtrace.elide := false;
  return ()
;;
