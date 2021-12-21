open! Core
open! Async
open! Vcaml
open! Vcaml_greetings

let%expect_test "Plugin responds to RPC request" =
  let%map greeting =
    Vcaml_test.with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let define_on_start =
        {| function! OnGreetingsPluginStart(channel)
           endfunction |}
      in
      let%bind (_ : string) =
        run_join [%here] client (Nvim.source ~code:define_on_start)
      in
      let%bind { plugin_state = (); shutdown; wait_for_shutdown } =
        Greetings.For_testing.start ~client
      in
      let greeting =
        wrap_viml_function
          ~function_name:"rpcrequest"
          ~type_:Defun.Vim.(Integer @-> String @-> String @-> return String)
          (Client.rpc_channel_id client)
          "greeting"
      in
      let%bind greeting = run_join [%here] client (greeting "Jane") in
      shutdown ();
      let%bind () = wait_for_shutdown in
      return greeting)
  in
  print_endline greeting;
  [%expect {| Hello, Jane! |}]
;;
