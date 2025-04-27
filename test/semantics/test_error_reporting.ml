module Do_not_move = struct
  let line2 = [%here]
  let line3 = [%here]
end

open Core
open Async
open Vcaml
open Vcaml_test_helpers

let%expect_test "Error inside [block_nvim] is displayed with callsite" =
  let open Do_not_move in
  let%bind () =
    with_client (fun client ->
      let%map result =
        block_nvim ~here:line2 client ~f:(fun client ->
          Command.exec ~here:line3 client "DoesNotExist")
      in
      print_s [%sexp (result : unit Or_error.t)];
      Ok ())
  in
  [%expect
    {|
    (Error
     (("Vim returned error" "Command not found: DoesNotExist"
       (error_type Validation))
      (("Called from" lib/vcaml/test/semantics/test_error_reporting.ml:3:14)
       ("Called from" lib/vcaml/test/semantics/test_error_reporting.ml:2:14))))
    |}];
  return ()
;;

let%expect_test "Error in the middle of an atomic call is returned correctly" =
  Dynamic.set_root Backtrace.elide true;
  let%bind () =
    Expect_test_helpers_async.require_does_raise_async (fun () ->
      with_client (fun client ->
        let open Expert in
        let open Deferred.Or_error.Let_syntax in
        let get name = Nvim_internal.nvim_get_var ~name in
        let set name value = Nvim_internal.nvim_set_var ~name ~value in
        match%map
          [ T (set "foo" (Bool true))
          ; T (set "baz" (Bool true))
          ; T (get "foo")
          ; T (get "bar")
          ; T (get "baz")
          ]
          |> Atomic.run client
          |> Deferred.map ~f:(Result.map_error ~f:Atomic.Error.to_error)
        with
        | [ Nil; Nil; Bool foo; Bool bar; Bool baz ] ->
          print_s [%message "" (foo : bool) (bar : bool) (baz : bool)]
        | response -> print_s [%message "Unexpected response" (response : Msgpack.t list)]))
  in
  [%expect
    {|
    (("One of the calls in the nvim_call_atomic batch failed"
      (partial_results (Nil Nil (Bool true)))
      (index_of_failure 3)
      (error_type       Validation)
      "Key not found: bar")
     (("Called from" lib/vcaml/test/semantics/test_error_reporting.ml:LINE:COL)))
    |}];
  Dynamic.set_root Backtrace.elide false;
  return ()
;;

let%expect_test "Failures parsing async requests" =
  Dynamic.set_root Backtrace.elide true;
  let module Notifier = Expert.Notifier in
  let module Args = struct
    type 'a t =
      | [] : unit Deferred.Or_error.t t
      | ( :: ) : 'a * 'b t -> ('a -> 'b) t
  end
  in
  let test (type t) ~(type_ : t Notifier.Func.t) (args : t Args.t) =
    with_ui_client (fun client ui ->
      let open Deferred.Or_error.Let_syntax in
      let call_async_func ~(here : [%call_pos]) client =
        Notifier.notify
          ~here
          client
          ~name:(`Viml "rpcnotify")
          ~type_:Notifier.Func.(Int @-> String @-> type_)
          (Client.channel client)
          "async_func"
      in
      Ocaml_from_nvim.register_request_async
        (Connected client)
        ~name:"async_func"
        ~type_:Ocaml_from_nvim.Async.(Nil @-> unit)
        ~f:(fun ~client:_ () ->
          print_s [%message "Parsing unexpectedly succeeded."];
          Deferred.Or_error.return ());
      let rec call : type t. t -> t Args.t -> unit Deferred.Or_error.t =
        fun f args ->
        match args with
        | [] -> f
        | hd :: tl -> call (f hd) tl
      in
      let%bind () = call (call_async_func client) args in
      let%map screen =
        wait_until_text ui ~f:(String.is_substring ~substring:"argument")
      in
      print_endline screen)
  in
  let%bind () =
    (* Wrong argument type *)
    test ~type_:Notifier.Func.(String @-> unit) [ "bad argument" ]
  in
  [%expect
    {|
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │                                                                                │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │                                                                                │
    │((test-client                                                                   │
    │  (("Failed to parse" (method_name async_func)                                  │
    │    (params ((String "bad argument"))))                                         │
    │   ("Wrong argument type"                                                       │
    │    ("witness does not match message type" (witness Nil)                        │
    │     (msgpack (String "bad argument"))))))                                      │
    │ (("Called from" lib/vcaml/test/semantics/test_error_reporting.ml:LINE:COL)))   │
    │Press ENTER or type command to continue                                         │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  let%bind () =
    (* Too few arguments *)
    test ~type_:Notifier.Func.unit []
  in
  [%expect
    {|
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │                                                                                │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │                                                                                │
    │((test-client                                                                   │
    │  ("Wrong number of arguments" (method_name async_func) (params ())))           │
    │ (("Called from" lib/vcaml/test/semantics/test_error_reporting.ml:LINE:COL)))   │
    │Press ENTER or type command to continue                                         │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  let%bind () =
    (* Too many arguments *)
    test ~type_:Notifier.Func.(Nil @-> Nil @-> unit) [ (); () ]
  in
  [%expect
    {|
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │                                                                                │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │                                                                                │
    │((test-client                                                                   │
    │  ("Wrong number of arguments" (method_name async_func) (params (Nil Nil))))    │
    │ (("Called from" lib/vcaml/test/semantics/test_error_reporting.ml:LINE:COL)))   │
    │Press ENTER or type command to continue                                         │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  Dynamic.set_root Backtrace.elide false;
  return ()
;;

let%expect_test "Error is displayed when Neovim sends a notification for an unknown \
                 method"
  =
  Dynamic.set_root Backtrace.elide true;
  let module Notifier = Expert.Notifier in
  let%bind () =
    with_ui_client (fun client ui ->
      let open Deferred.Or_error.Let_syntax in
      let call_async_func ~(here : [%call_pos]) client =
        Notifier.notify
          ~here
          client
          ~name:(`Viml "rpcnotify")
          ~type_:Notifier.Func.(Int @-> String @-> unit)
          (Client.channel client)
          "async_func"
      in
      let%bind () = call_async_func client in
      let%map screen = wait_until_text ui ~f:(String.is_substring ~substring:"method") in
      print_endline screen)
  in
  [%expect
    {|
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │                                                                                │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │                                                                                │
    │((test-client "Unknown method async_func")                                      │
    │ (("Called from" lib/vcaml/src/client.ml:LINE:COL)))                            │
    │Press ENTER or type command to continue                                         │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  Dynamic.set_root Backtrace.elide false;
  return ()
;;
