open! Core
open! Async
open! Import
open Vcaml
open Test_client
module Notifier = Vcaml.Expert.Notifier
module Notification = Notifier.Notification

let get_current_channel ~client =
  let%map.Deferred.Or_error channels = Nvim.channels |> run_join [%here] client in
  List.hd_exn channels
;;

let%expect_test "Simple asynchronous notification" =
  let result =
    let result = Ivar.create () in
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind channel =
        let%map channel_info = get_current_channel ~client in
        channel_info.id
      in
      let call_async_func =
        Notification.custom
          ~type_:Notification.Defun.Vim.(Integer @-> String @-> unit)
          ~function_name:"rpcnotify"
          channel
          "async_func"
      in
      register_request_async
        client
        ~name:"async_func"
        ~type_:Defun.Ocaml.Async.unit
        ~f:(fun ~client:_ -> Deferred.Or_error.return (Ivar.fill result "Called!"));
      Notifier.notify client call_async_func;
      Ivar.read result |> Deferred.ok)
  in
  let%bind result = with_timeout (Time_float.Span.of_int_sec 3) result in
  print_s [%sexp (result : [ `Result of string | `Timeout ])];
  [%expect {| (Result Called!) |}];
  return ()
;;

let%expect_test "Bad asynchronous notification" =
  let result =
    let result = Ivar.create () in
    with_client
      ~on_error:
        (`Call
           (fun error ->
              print_s [%sexp (error : Vcaml_error.t)];
              Ivar.fill result "Received asynchronous failure message"))
      (fun client ->
         Notifier.For_testing.send_raw client ~function_name:"" ~params:[];
         Ivar.read result |> Deferred.ok)
  in
  let%bind result = with_timeout (Time_float.Span.of_int_sec 3) result in
  print_s [%sexp (result : [ `Result of string | `Timeout ])];
  [%expect
    {|
    (Nvim_error_event
     (Error (error_type Exception) (message "Invalid method: <empty>")))
    (Result "Received asynchronous failure message") |}];
  return ()
;;
