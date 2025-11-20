open Core
open Async
open Vcaml
open Vcaml_test_helpers

let%expect_test "Test buffer clock" =
  let%map () =
    with_ui_client
      ~links:
        [ "../buffer_clock.lua", `In_temp_as, "buffer_clock.lua"
        ; "../bin/main.exe", `In_path_as, "main.exe"
        ]
      (fun client ui ->
        let open Deferred.Or_error.Let_syntax in
        let%bind () = Command.exec client "source" ~args:[ "buffer_clock.lua" ] in
        let%bind buffer_clock_channel =
          Deferred.repeat_until_finished () (fun () ->
            let open Deferred.Let_syntax in
            match%bind Nvim.get_var client "buffer_clock_channel" ~type_:Int with
            | Error _ ->
              let%map () = Clock_ns.after Time_ns.Span.millisecond in
              `Repeat ()
            | Ok channel -> return (`Finished (Ok channel)))
        in
        let%bind buffer_clock_job = Nvim.get_var client "buffer_clock_job" ~type_:Int in
        let%bind screen = get_screen_contents ui in
        print_endline screen;
        let advance_time () =
          Nvim.call_function
            client
            ~name:(`Viml "rpcnotify")
            ~type_:Nvim.Func.(Int @-> String @-> return Int)
            buffer_clock_channel
            "advance-time"
          |> Deferred.Or_error.ignore_m
        in
        let%bind () = advance_time () in
        let%bind screen =
          wait_until_text ui ~f:(String.is_substring ~substring:"00:00:01")
        in
        print_endline screen;
        let%bind () = Command.exec client "only" in
        let%bind () = advance_time () in
        let%bind () =
          (* Give the plugin a chance to try to update the clock and shut down when it
             fails to do so. If we immediately call [jobwait] here, the plugin's buffer
             update will queue behind it. *)
          Clock_ns.after (Time_ns.Span.of_int_ms 10) |> Deferred.ok
        in
        Nvim.call_function
          client
          ~name:(`Viml "jobwait")
          ~type_:Nvim.Func.(Array Int @-> Int @-> return (Array Int))
          [ buffer_clock_job ]
          1000
        >>= function
        | [ -1 ] -> Deferred.Or_error.error_string "Timed out!"
        | [ -2 ] -> Deferred.Or_error.error_string "Interrupted!"
        | [ _ ] -> Deferred.Or_error.return ()
        | statuses ->
          Deferred.Or_error.error_s
            [%message "Unexpected output from [jobwait]" ~_:(statuses : int list)])
  in
  [%expect
    {|
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │1970-01-01 00:00:00                                                             │
    │[Scratch]                                                     1,1            All│
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
    │[No Name]                                                     0,0-1          All│
    │                                                                                │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │1970-01-01 00:00:01                                                             │
    │[Scratch]                                                     1,1            All│
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
    │[No Name]                                                     0,0-1          All│
    │                                                                                │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}]
;;
