open! Core
open! Async
open! Vcaml_greetings

let during_plugin client ~chan_id ~state:() =
  let open Deferred.Or_error.Let_syntax in
  let%bind res =
    Vcaml_plugin.For_testing.run_rpc_call
      ~client
      ~chan_id
      ~name:"greeting"
      ~args:[ Msgpack.String "Jane" ]
  in
  print_s ([%sexp_of: Msgpack.t] res);
  let%bind _ =
    Vcaml_plugin.For_testing.run_rpc_call
      ~client
      ~chan_id
      ~name:"shutdown"
      ~args:[ Msgpack.Nil ]
  in
  print_s (Sexp.Atom "Shutdown success");
  return ()
;;

let%expect_test "plugin responds to RPC requests and shuts down" =
  let%map () =
    Vcaml_plugin.For_testing.with_client (fun client ->
      Greetings.For_testing.run client ~during_plugin:(during_plugin client))
  in
  [%expect {|
  (String "Hello, Jane!")
  "Shutdown success" |}]
;;
