open Core
open Async
open Vcaml
open Vcaml_test_helpers

let test f =
  with_ui_client
    ~links:
      [ "../hello_world.lua", `In_temp_as, "hello_world.lua"
      ; "../bin/main.exe", `In_path_as, "main.exe"
      ]
    (fun client ui ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = Command.exec client "source" ~args:[ "hello_world.lua" ] in
      let%bind () =
        Deferred.Or_error.repeat_until_finished () (fun () ->
          match%bind
            Nvim.call_function
              client
              ~name:(`Viml "exists")
              ~type_:Nvim.Func.(String @-> return Int)
              ":SayGoodbye"
          with
          | 0 | 1 | 3 ->
            let%map () = Clock_ns.after Time_ns.Span.millisecond |> Deferred.ok in
            `Repeat ()
          | 2 -> return (`Finished ())
          | n ->
            Deferred.Or_error.error_s
              [%message "Unexpected return value from [exists]" ~_:(n : int)])
      in
      f client ui)
;;

let%expect_test "Two hellos, one goodbye" =
  let%bind () =
    test (fun client ui ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = Command.exec client "SayHello" ~args:[ "Alice" ] in
      let%bind screen = wait_until_text ui ~f:(String.is_substring ~substring:"Alice") in
      print_endline screen;
      let%bind () = Command.exec client "SayHello" ~args:[ "Bob" ] in
      let%bind screen = wait_until_text ui ~f:(String.is_substring ~substring:"Bob") in
      print_endline screen;
      let%bind () = Command.exec client "SayGoodbye" in
      let%bind screen =
        wait_until_text ui ~f:(String.is_substring ~substring:"Goodbye")
      in
      print_endline screen;
      return ())
  in
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
    │Hello, Alice!                                                                   │
    ╰────────────────────────────────────────────────────────────────────────────────╯
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
    │Hello, Bob!                                                                     │
    ╰────────────────────────────────────────────────────────────────────────────────╯
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
    │Goodbye, Bob!                                                                   │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  return ()
;;

let%expect_test "Goodbye without Hello" =
  let%bind () =
    test (fun client ui ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = Command.exec client "SayGoodbye" in
      let%bind screen =
        wait_until_text ui ~f:(String.is_substring ~substring:"Goodbye")
      in
      print_endline screen;
      return ())
  in
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
    │Goodbye!                                                                        │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  return ()
;;
