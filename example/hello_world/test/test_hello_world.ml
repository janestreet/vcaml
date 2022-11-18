open! Core
open Async
open! Vcaml
open Vcaml_hello_world
open Vcaml_test_helpers

let%expect_test "plugin echoes a message" =
  let%map screen =
    with_ui_client (fun client ui ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = Hello_world.For_testing.echo_hello_world client in
      get_screen_contents [%here] ui)
  in
  print_endline screen;
  [%expect
    {|
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │                                                                                │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │[No Name]                                                     0,0-1          All│
    │Hello, world!                                                                   │
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}]
;;
