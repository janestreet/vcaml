open Core
open Async
open Vcaml
open Vcaml_test_helpers
module Notifier = Vcaml.Expert.Notifier

let get_current_channel ~client =
  let%map.Deferred.Or_error channels = Nvim.channels client in
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
      let name = "async_func" in
      Ocaml_from_nvim.register_request_async
        (Connected client)
        ~name
        ~type_:Ocaml_from_nvim.Async.unit
        ~f:(fun ~client:_ -> Deferred.Or_error.return (Ivar.fill_exn result "Called!"));
      let%bind () =
        Notifier.notify
          client
          ~name:(`Viml "rpcnotify")
          ~type_:Notifier.Func.(Int @-> String @-> unit)
          channel
          name
      in
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
            Ivar.fill_exn result "Received asynchronous failure message"))
      (fun client ->
        let%bind.Deferred.Or_error () =
          Notifier.notify client ~name:(`Viml "bad-function") ~type_:Notifier.Func.unit
        in
        Ivar.read result |> Deferred.ok)
  in
  let%bind result = with_timeout (Time_float.Span.of_int_sec 3) result in
  print_s [%sexp (result : [ `Result of string | `Timeout ])];
  [%expect
    {|
    (Nvim_error_event
     ((error_type Exception)
      (message "Vim:E117: Unknown function: bad-function")))
    (Result "Received asynchronous failure message")
    |}];
  return ()
;;
