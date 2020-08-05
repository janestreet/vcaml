open! Core
open! Async
open Vcaml
open Vcaml_greetings

let get_string_or_error msg =
  let open Deferred.Or_error in
  match msg with
  | Msgpack.String x -> return x
  | _ -> error_string "Msgpack response is not a string"
;;

let make_nvim_rpc_call ~client ~chan_id ~name ~payload =
  Vcaml.run_join
    client
    (Client.call_function ~fn:"rpcrequest" ~args:[ Integer chan_id; String name; payload ])
;;

let%expect_test "plugin responds to RPC requests and shuts down" =
  let%bind () =
    Vcaml_test.Test_client.with_client ~f:(fun client ->
      let open Deferred.Or_error.Let_syntax in
      let chan_name = "greetings_test" in
      let terminate_var = Ivar.create () in
      let%bind () =
        Greetings.For_testing.start_plugin_for_testing
          ~client
          ~name:chan_name
          ~terminate_var
      in
      let%bind chan_id =
        Greetings.For_testing.get_rpc_chan_for_testing ~client ~name:chan_name
      in
      let%bind message =
        make_nvim_rpc_call ~client ~chan_id ~name:"greeting" ~payload:(String "Jane")
      in
      let%bind rpc_response = get_string_or_error message in
      print_s [%message (rpc_response : string)];
      print_s [%message (terminate_var : unit Ivar.t)];
      let%bind (_ : Msgpack.t) =
        make_nvim_rpc_call ~client ~chan_id ~name:"shutdown" ~payload:Nil
      in
      print_s [%message (terminate_var : unit Ivar.t)];
      return ())
  in
  [%expect
    {|
      (rpc_response "Hello, Jane!")
      (terminate_var Empty)
      (terminate_var (Full ())) |}]
;;
