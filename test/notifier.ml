open! Core
open! Async
open! Import
open Vcaml
open Test_client
module Notifier = Vcaml.Expert.Notifier
module Notification = Notifier.Notification

let get_current_chan ~client =
  let%map.Deferred.Or_error chan_list = Nvim.list_chans |> run_join client in
  List.hd_exn chan_list
;;

let%expect_test "Simple asynchronous notification" =
  let was_called = Ivar.create () in
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind channel =
        let%map channel = get_current_chan ~client in
        channel.id
      in
      let notifier =
        Notifier.create client ~on_error_event:(fun error_type ~message ->
          print_s [%message message (error_type : Notifier.Error_type.t)])
      in
      let call_async_func =
        Notification.custom
          ~type_:Notification.Defun.Vim.(Integer @-> String @-> Nil @-> unit)
          ~function_name:"rpcnotify"
          channel
          "async_func"
      in
      register_request_async
        client
        ~name:"async_func"
        ~type_:Defun.Ocaml.Async.(Nil @-> unit)
        ~f:(fun () -> Deferred.return (Ivar.fill was_called "Called!"));
      Notifier.notify notifier (call_async_func ());
      return ())
  in
  let%bind result = with_timeout (Time.Span.of_int_sec 3) (Ivar.read was_called) in
  print_s [%sexp (result : [ `Result of string | `Timeout ])];
  [%expect {| (Result Called!) |}];
  return ()
;;

let%expect_test "Bad asynchronous notification" =
  let failed = Ivar.create () in
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let notifier =
        Notifier.create client ~on_error_event:(fun error_type ~message ->
          print_s [%message message (error_type : Notifier.Error_type.t)];
          Ivar.fill failed "Received asynchronous failure message.")
      in
      Notifier.For_testing.send_raw notifier ~function_name:"" ~params:(Array []);
      return ())
  in
  let%bind result = with_timeout (Time.Span.of_int_sec 3) (Ivar.read failed) in
  print_s [%sexp (result : [ `Result of string | `Timeout ])];
  [%expect
    {|
    ("Invalid method: <empty>" (error_type Exception))
    (Result "Received asynchronous failure message.") |}];
  return ()
;;
