open! Core
open! Async
open! Vcaml
open! Vcaml_hello_world

let%expect_test "plugin echoes a message" =
  let%map screen =
    Vcaml_test.with_ui_client (fun client ui ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = Hello_world.For_testing.echo_hello_world client in
      Vcaml_test.get_screen_contents [%here] ui)
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
