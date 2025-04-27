open Core
open Async
open Vcaml
open Vcaml_test_helpers

(* Since we are testing cases where Neovim is expected to exit early, we don't want to
   emit the non-deterministic warning message. We could reconsider if we found a way to
   make the warning deterministic. *)
let with_client = with_client ~warn_if_neovim_exits_early:false

let%expect_test "Sending request with a closed client" =
  Dynamic.set_root Backtrace.elide true;
  let%map () =
    with_client (fun client ->
      let%bind () = Client.close client in
      Expect_test_helpers_async.require_does_raise_async (fun () ->
        Nvim.get_current_buf client >>| ok_exn)
      |> Deferred.ok)
  in
  Dynamic.set_root Backtrace.elide false;
  [%expect
    {|
    (("Failed to send Msgpack RPC message: writer is closed"
      (Array (
        (Int    0)
        (Int    9)
        (String nvim_get_current_buf)
        (Array ()))))
     (("Called from" lib/vcaml/test/semantics/test_closed_client.ml:LINE:COL)))
    |}]
;;

let%expect_test "Sending notification with a closed client" =
  Dynamic.set_root Backtrace.elide true;
  let%map () =
    with_client (fun client ->
      let%bind () = Client.close client in
      Expect_test_helpers_async.require_does_raise_async (fun () ->
        let open Expert.Notifier in
        notify client ~name:(`Viml "nvim_get_current_buf") ~type_:Func.unit >>| ok_exn)
      |> Deferred.ok)
  in
  Dynamic.set_root Backtrace.elide false;
  [%expect
    {|
    (("Failed to send Msgpack RPC message: writer is closed"
      (Array (
        (Int    2)
        (String nvim_call_function)
        (Array ((String nvim_get_current_buf) (Array ()))))))
     (("Called from" lib/vcaml/test/semantics/test_closed_client.ml:LINE:COL)))
    |}]
;;

let%expect_test "Sending response with a closed client" =
  Dynamic.set_root Backtrace.elide true;
  let%map () =
    with_client (fun client ->
      (* We use this low-level hook instead of closing the client inside the RPC because
         we want to make sure we are testing the client closing before sending the
         response message - if we do it inside the RPC, the flush message will be sent
         first. We test closing inside the RPC next. *)
      Private.before_sending_response_hook_for_tests
      := Some (fun () -> Client.close client);
      let function_name = "rpc" in
      let () =
        Ocaml_from_nvim.register_request_blocking
          (Connected client)
          ~name:function_name
          ~type_:Ocaml_from_nvim.Blocking.(return Nil)
          ~f:(fun ~run_in_background:_ ~client:_ -> Deferred.Or_error.return ())
      in
      let channel = Client.channel client in
      Expect_test_helpers_async.require_does_raise_async (fun () ->
        Nvim.call_function
          client
          ~name:(`Viml "rpcrequest")
          ~type_:Nvim.Func.(Int @-> String @-> return Nil)
          channel
          function_name
        >>| ok_exn)
      |> Deferred.ok)
  in
  Private.before_sending_response_hook_for_tests := None;
  Dynamic.set_root Backtrace.elide false;
  [%expect
    {|
    (("Consumer left without responding" (
       request (
         Array (
           (Int    0)
           (Int    8)
           (String nvim_call_function)
           (Array (
             (String rpcrequest)
             (Array (
               (Int    1)
               (String rpc)))))))))
     (("Called from" lib/vcaml/test/semantics/test_closed_client.ml:LINE:COL)))
    |}]
;;

let%expect_test "Client closes inside RPC" =
  Dynamic.set_root Backtrace.elide true;
  let%map () =
    with_client (fun client ->
      let function_name = "rpc" in
      let () =
        Ocaml_from_nvim.register_request_blocking
          (Connected client)
          ~name:function_name
          ~type_:Ocaml_from_nvim.Blocking.(return Nil)
          ~f:(fun ~run_in_background:_ ~client -> Client.close client |> Deferred.ok)
      in
      let channel = Client.channel client in
      Expect_test_helpers_async.require_does_raise_async (fun () ->
        Nvim.call_function
          client
          ~name:(`Viml "rpcrequest")
          ~type_:Nvim.Func.(Int @-> String @-> return Nil)
          channel
          function_name
        >>| ok_exn)
      |> Deferred.ok)
  in
  Dynamic.set_root Backtrace.elide false;
  [%expect
    {|
    (("Consumer left without responding" (
       request (
         Array (
           (Int    0)
           (Int    8)
           (String nvim_call_function)
           (Array (
             (String rpcrequest)
             (Array (
               (Int    1)
               (String rpc)))))))))
     (("Called from" lib/vcaml/test/semantics/test_closed_client.ml:LINE:COL)))
    |}]
;;
