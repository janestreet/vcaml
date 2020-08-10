open! Core_kernel
open! Async
open Vcaml
open Vcaml_hello_world

let%expect_test "plugin echoes a message" =
  let%bind () =
    Vcaml_test.Test_client.with_client ~f:(fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = Hello_world.For_testing.run_plugin_for_testing ~client in
      let%map message = run_join client (Client.command_output ~command:"2 messages") in
      print_s [%message (message : string)])
  in
  [%expect "(message \"Hello world!\")"]
;;
