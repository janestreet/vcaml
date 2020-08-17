open! Core_kernel
open! Async
open! Vcaml
open! Vcaml_hello_world

let before_plugin ~client:_ = Deferred.Or_error.return ()

let after_plugin ~client ~state:() =
  let%map.Deferred.Or_error message =
    run_join client (Client.command_output ~command:"2 messages")
  in
  print_s [%message (message : string)]
;;

let%expect_test "plugin echoes a message" =
  let%map () = Hello_world.test ~before_plugin ~after_plugin () in
  [%expect "(message \"Hello world!\")"]
;;
