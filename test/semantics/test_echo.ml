open Core
open Async
open Vcaml
open Vcaml_test_helpers

let run_echo_tests ~f ~message =
  let tests =
    [ ( "native_echo"
      , fun ~(here : [%call_pos]) client ->
          Command.exec ~here client "echo" ~args:[ [%string "'%{message}'"] ] )
    ; ( "api_out_write"
      , fun ~(here : [%call_pos]) client -> Nvim.out_write ~here client (message ^ "\n")
      )
    ; ( "api_echo"
      , fun ~(here : [%call_pos]) client ->
          Nvim.echo
            ~here
            client
            [ { text = message; hl_group = None } ]
            ~add_to_history:false )
    ; ( "api_notify"
      , fun ~(here : [%call_pos]) client -> Nvim.notify ~here client Info message )
    ]
  in
  Deferred.List.map tests ~how:`Sequential ~f:(fun (name, echo) ->
    let%map output = f ~echo in
    output, name)
  >>| String.Map.of_alist_multi
  >>| Map.iteri ~f:(fun ~key:output ~data:names ->
    let names = String.concat names ~sep:", " in
    print_endline [%string "Output for: %{names}\n%{output}\n"])
;;

let%expect_test "Show echoed content in command line" =
  let%bind () =
    run_echo_tests ~message:"Hello, world!" ~f:(fun ~echo ->
      with_ui_client (fun client ui ->
        let open Deferred.Or_error.Let_syntax in
        let%bind () = echo client in
        get_screen_contents ui))
  in
  [%expect
    {|
    Output for: native_echo, api_out_write, api_echo, api_notify
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
    |}];
  return ()
;;

let%expect_test "Test sending errors" =
  let%bind () =
    with_ui_client (fun client ui ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = Nvim.err_write client "This should not display yet..." in
      let%bind screen = get_screen_contents ui in
      print_endline screen;
      let%bind () = Nvim.err_write client "but now it should!\n" in
      let%bind screen = get_screen_contents ui in
      print_endline screen;
      let%bind () = Nvim.err_writeln client "This should display now." in
      let%bind screen = get_screen_contents ui in
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
    │                                                                                │
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
    │This should not display yet...but now it should!                                │
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
    │This should display now.                                                        │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  return ()
;;

let%expect_test "Naively calling [echo] from inside [rpcrequest] fails" =
  let%bind () =
    run_echo_tests
      ~message:"If this message appears we should get rid of [echo_in_rpcrequest]"
      ~f:(fun ~echo ->
        let open Deferred.Or_error.Let_syntax in
        let rpc ~run_in_background:_ ~client ui =
          let%bind () = echo client in
          get_screen_contents ui
        in
        with_ui_client (fun client ui ->
          let () =
            Ocaml_from_nvim.register_request_blocking
              (Connected client)
              ~name:"rpc"
              ~type_:Ocaml_from_nvim.Blocking.(return String)
              ~f:(rpc ui)
          in
          let channel = Client.channel client in
          Nvim.eval_viml_expression
            client
            (sprintf "rpcrequest(%d, 'rpc')" channel)
            ~result_type:String))
  in
  [%expect
    {|
    Output for: native_echo, api_out_write, api_echo, api_notify
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
    │                                                                                │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  return ()
;;

let%expect_test "[echo_in_rpcrequest] hack" =
  let%bind () =
    let open Deferred.Or_error.Let_syntax in
    let rpc ~run_in_background:_ ~client ui =
      let%bind () =
        Nvim.echo_in_rpcrequest client "Hello, world! 'Quotes' \"work\" too."
      in
      let%bind screen = get_screen_contents ui in
      print_endline screen;
      return ()
    in
    with_ui_client (fun client ui ->
      let () =
        Ocaml_from_nvim.register_request_blocking
          (Connected client)
          ~name:"rpc"
          ~type_:Ocaml_from_nvim.Blocking.(return Nil)
          ~f:(rpc ui)
      in
      let channel = Client.channel client in
      Nvim.exec_viml client [%string "call rpcrequest(%{channel#Int}, 'rpc')"])
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
    │Hello, world! 'Quotes' "work" too.                                              │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  return ()
;;

let%expect_test "notify" =
  let test client ui =
    let open Deferred.Or_error.Let_syntax in
    Deferred.Or_error.List.map ~how:`Sequential Nvim.Log_level.all ~f:(fun log_level ->
      let%bind () = Nvim.notify client log_level "Test message" in
      let%map output = get_screen_contents ui in
      output, log_level)
    >>| String.Map.of_alist_multi
    >>| Map.iteri ~f:(fun ~key:output ~data:names ->
      print_s [%message "Output for" (names : Nvim.Log_level.t list)];
      print_endline output)
  in
  let%bind () = with_ui_client test in
  [%expect
    {|
    ("Output for" (names (Trace Debug Info Warn Error)))
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
    │Test message                                                                    │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  let%bind () =
    with_ui_client (fun client ui ->
      let%bind.Deferred.Or_error () =
        Nvim.exec_lua
          client
          {|
          function vim.notify(msg, level)
            if level == vim.log.levels.ERROR then
              print("ERROR: "..msg)
            elseif level == vim.log.levels.WARN then
              print("WARNING: "..msg)
            end
          end |}
      in
      test client ui)
  in
  [%expect
    {|
    ("Output for" (names (Trace Debug Info)))
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
    │                                                                                │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ("Output for" (names (Error)))
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
    │ERROR: Test message                                                             │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ("Output for" (names (Warn)))
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
    │WARNING: Test message                                                           │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  return ()
;;
