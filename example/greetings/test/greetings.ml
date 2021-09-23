open! Core
open! Async
open! Vcaml
open! Vcaml_greetings

let during_plugin client ~chan_id ~state:() =
  let open Deferred.Or_error.Let_syntax in
  let greeting =
    wrap_viml_function
      ~function_name:"rpcrequest"
      ~type_:Defun.Vim.(Integer @-> String @-> String @-> return String)
      chan_id
      "greeting"
  in
  let shutdown =
    wrap_viml_function
      ~function_name:"rpcrequest"
      ~type_:Defun.Vim.(Integer @-> String @-> Nil @-> return Nil)
      chan_id
      "shutdown"
  in
  let%bind res = run_join [%here] client (greeting "Jane") in
  printf "%s\n" res;
  let%bind () = run_join [%here] client (shutdown ()) in
  printf "Shutdown success.\n";
  return ()
;;

let%expect_test "plugin responds to RPC requests and shuts down" =
  let%map () =
    Vcaml_test.with_client (fun client ->
      Greetings.For_testing.run client ~during_plugin:(during_plugin client))
  in
  [%expect {|
  Hello, Jane!
  Shutdown success. |}]
;;
