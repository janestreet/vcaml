open! Core
open Async
open Vcaml
open Vcaml_test_helpers

let%expect_test "plugin echoes a message" =
  let%map screen =
    with_ui_client
      ~links:
        [ "../hello_world.lua", `In_temp_as, "hello_world.lua"
        ; "../bin/main.exe", `In_path_as, "main.exe"
        ]
      (fun client ui ->
        let open Deferred.Or_error.Let_syntax in
        let%bind () = Command.exec client "source" ~args:[ "hello_world.lua" ] in
        let%bind () = Command.exec client "SayHello" ~args:[ "world" ] in
        get_screen_contents ui)
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
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}]
;;
