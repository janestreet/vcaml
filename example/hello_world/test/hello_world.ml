open! Core_kernel
open! Async
open! Vcaml
open! Vcaml_hello_world

let print_messages client =
  let%map.Deferred.Or_error message =
    run_join client (Client.command_output ~command:"2 messages")
  in
  print_s [%message (message : string)]
;;

let%expect_test "plugin echoes a message" =
  let%map () =
    Vcaml_plugin.For_testing.with_client (fun client ->
      let%bind.Deferred.Or_error () = Hello_world.For_testing.run client in
      print_messages client)
  in
  [%expect "(message \"Hello world!\")"]
;;
