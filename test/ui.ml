open! Core
open! Async
open! Import
open! Vcaml
open Test_client

let%expect_test "Simple test of attach, detach, describe_attached_uis" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind descriptions = Ui.describe_attached_uis |> run_join [%here] client in
      print_s [%sexp (descriptions : Ui.Description.t list)];
      let%bind ui =
        Ui.attach
          [%here]
          client
          ~width:100
          ~height:200
          ~options:
            { ext_cmdline = true
            ; ext_hlstate = false
            ; ext_linegrid = false
            ; ext_messages = false
            ; ext_multigrid = false
            ; ext_popupmenu = false
            ; ext_tabline = true
            ; ext_termcolors = true
            ; ext_wildmenu = false
            ; rgb = true
            }
          ~on_event:ignore
          ~on_parse_error:`Raise
      in
      let%bind descriptions = Ui.describe_attached_uis |> run_join [%here] client in
      print_s [%sexp (descriptions : Ui.Description.t list)];
      let%bind () = Ui.detach ui [%here] in
      let%bind descriptions = Ui.describe_attached_uis |> run_join [%here] client in
      print_s [%sexp (descriptions : Ui.Description.t list)];
      return ())
  in
  [%expect
    {|
    ()
    (((channel (Id 1)) (height 200) (width 100)
      (options
       ((ext_cmdline true) (ext_hlstate false) (ext_linegrid false)
        (ext_messages false) (ext_multigrid false) (ext_popupmenu false)
        (ext_tabline true) (ext_termcolors true) (ext_wildmenu false) (rgb true)))))
    () |}];
  return ()
;;

let%expect_test "get_screen_contents" =
  let%bind () =
    let open Deferred.Or_error.Let_syntax in
    with_ui_client (fun _client ui ->
      let%bind screen = get_screen_contents [%here] ui in
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
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;

let%expect_test "get screen contents multiple times" =
  let%bind () =
    let open Deferred.Or_error.Let_syntax in
    with_ui_client (fun client ui ->
      let%bind screen = get_screen_contents [%here] ui in
      print_endline screen;
      let%bind () =
        Vcaml.Nvim.feedkeys (`Escape_k_special_bytes "ihello world") ~mode:"n"
        |> run_join [%here] client
      in
      let%bind screen = get_screen_contents [%here] ui in
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
    │hello world                                                                     │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │[No Name] [+]                                                 1,12           All│
    │-- INSERT --                                                                    │
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;

let%expect_test "screen contents after typing hello world" =
  let%bind () =
    let open Deferred.Or_error.Let_syntax in
    with_ui_client (fun client ui ->
      let%bind () =
        Vcaml.Nvim.feedkeys (`Escape_k_special_bytes "ihello world") ~mode:"n"
        |> run_join [%here] client
      in
      let%bind () = Vcaml.Nvim.command "vsplit" |> run_join [%here] client in
      let%bind () = Vcaml.Nvim.command "split" |> run_join [%here] client in
      let%bind screen = get_screen_contents [%here] ui in
      print_endline screen;
      return ())
  in
  [%expect
    {|
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │hello world                             │hello world                            │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │[No Name] [+]         1,12           All│~                                      │
    │hello world                             │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │[No Name] [+]         1,12           All [No Name] [+]        1,12           All│
    │-- INSERT --                                                                    │
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;

let%expect_test "timeout occurs" =
  let%map () =
    Expect_test_helpers_async.require_does_raise_async [%here] (fun () ->
      let open Deferred.Or_error.Let_syntax in
      with_ui_client (fun client ui ->
        let%bind () = Vcaml.Nvim.command "e term://sh" |> run_join [%here] client in
        let%bind () =
          Vcaml.Nvim.command "file my-terminal" |> run_join [%here] client
        in
        let%bind (_ : string) =
          wait_until_text [%here] ui ~f:(String.is_substring ~substring:"sh-4.2$")
        in
        let%bind (_ : string) =
          wait_until_text
            [%here]
            ui
            ~timeout:(Time_ns.Span.of_sec 0.01)
            ~f:(Fn.const false)
        in
        return ()))
  in
  [%expect
    {|
    ERROR: timeout when looking for value on screen
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │sh-4.2$                                                                         │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │my-terminal                                                   1,1            All│
    │                                                                                │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    "ERROR: timeout when looking for value on screen" |}]
;;

let%expect_test "open up sh" =
  let%bind () =
    let open Deferred.Or_error.Let_syntax in
    with_ui_client (fun client ui ->
      let%bind () = Vcaml.Nvim.command "e term://sh" |> run_join [%here] client in
      let%bind () = Vcaml.Nvim.command "file my-terminal" |> run_join [%here] client in
      let%bind screen =
        wait_until_text [%here] ui ~f:(String.is_substring ~substring:"sh-4.2$")
      in
      print_endline screen;
      return ())
  in
  [%expect
    {|
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │sh-4.2$                                                                         │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │my-terminal                                                   1,1            All│
    │                                                                                │
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;

let run_echo_tests ~f ~message =
  let tests =
    [ "native_echo", Nvim.command [%string "echo '%{message}'"]
    ; "api_out_write", Nvim.out_write (message ^ "\n")
    ; "api_echo", Nvim.echo [ { text = message; hl_group = None } ] ~add_to_history:false
    ]
  in
  Deferred.List.map tests ~f:(fun (name, echo) ->
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
        let%bind () = run_join [%here] client echo in
        get_screen_contents [%here] ui))
  in
  [%expect
    {|
    Output for: native_echo, api_out_write, api_echo
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
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;

let%expect_test "Test sending errors" =
  let%bind () =
    with_ui_client (fun client ui ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () =
        run_join [%here] client (Nvim.err_write "This should not display yet...")
      in
      let%bind screen = get_screen_contents [%here] ui in
      print_endline screen;
      let%bind () = run_join [%here] client (Nvim.err_write "but now it should!\n") in
      let%bind screen = get_screen_contents [%here] ui in
      print_endline screen;
      let%bind () =
        run_join [%here] client (Nvim.err_writeln "This should display now.")
      in
      let%bind screen = get_screen_contents [%here] ui in
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
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;

let%expect_test "Naively calling [echo] from inside [rpcrequest] fails" =
  let%bind () =
    run_echo_tests
      ~message:"If this message appears we should get rid of [echo_in_rpcrequest]"
      ~f:(fun ~echo ->
        let open Deferred.Or_error.Let_syntax in
        let rpc ~keyboard_interrupted:_ ~client ui =
          let%bind () = run_join [%here] client echo in
          get_screen_contents [%here] ui
        in
        with_ui_client (fun client ui ->
          let () =
            register_request_blocking
              client
              ~name:"rpc"
              ~type_:Defun.Ocaml.Sync.(return String)
              ~f:(rpc ui)
          in
          let channel = Client.channel client in
          Nvim.eval (sprintf "rpcrequest(%d, 'rpc')" channel) ~result_type:String
          |> run_join [%here] client))
  in
  [%expect
    {|
    Output for: native_echo, api_out_write, api_echo
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
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;

let%expect_test "[echo_in_rpcrequest] hack" =
  let%bind () =
    let open Deferred.Or_error.Let_syntax in
    let rpc ~keyboard_interrupted:_ ~client ui =
      let%bind () =
        run_join
          [%here]
          client
          (Nvim.echo_in_rpcrequest "Hello, world! 'Quotes' \"work\" too.")
      in
      let%bind screen = get_screen_contents [%here] ui in
      print_endline screen;
      return ()
    in
    with_ui_client (fun client ui ->
      let () =
        register_request_blocking
          client
          ~name:"rpc"
          ~type_:Defun.Ocaml.Sync.(return Nil)
          ~f:(rpc ui)
      in
      let channel = Client.channel client in
      Nvim.command (sprintf "call rpcrequest(%d, 'rpc')" channel)
      |> run_join [%here] client)
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
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;

let%expect_test "Failure to parse async request" =
  let%bind () =
    with_ui_client (fun client ui ->
      let open Deferred.Or_error.Let_syntax in
      let call_async_func =
        wrap_viml_function
          ~type_:Defun.Vim.(Integer @-> String @-> String @-> return Integer)
          ~function_name:"rpcnotify"
          (Client.channel client)
          "async_func"
      in
      register_request_async
        client
        ~name:"async_func"
        ~type_:Defun.Ocaml.Async.(Nil @-> unit)
        ~f:(fun ~client:_ () ->
          print_s [%message "Parsing unexpectedly succeeded."];
          Deferred.Or_error.return ());
      let%bind () = Nvim.command "set cmdheight=4" |> run_join [%here] client in
      let%bind rpcnotify_return_value =
        call_async_func "bad argument" |> run_join [%here] client
      in
      assert (rpcnotify_return_value = 1);
      let%map screen =
        wait_until_text [%here] ui ~f:(String.is_substring ~substring:"argument")
      in
      print_endline screen)
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
    │[No Name]                                                     0,0-1          All│
    │(((method_name async_func) (params ((String "bad argument"))))                  │
    │ ("Wrong argument type"                                                         │
    │  ("witness does not match message type" (witness Nil)                          │
    │   (msg (String "bad argument")))))                                             │
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;

let%expect_test "Too few arguments" =
  let%bind () =
    with_ui_client (fun client ui ->
      let open Deferred.Or_error.Let_syntax in
      let call_async_func =
        wrap_viml_function
          ~type_:Defun.Vim.(Integer @-> String @-> Nil @-> return Integer)
          ~function_name:"rpcnotify"
          (Client.channel client)
          "async_func"
      in
      register_request_async
        client
        ~name:"async_func"
        ~type_:Defun.Ocaml.Async.(Nil @-> Nil @-> unit)
        ~f:(fun ~client:_ () () ->
          print_s [%message "Parsing unexpectedly succeeded."];
          Deferred.Or_error.return ());
      let%bind () = Nvim.command "set cmdheight=2" |> run_join [%here] client in
      let%bind rpcnotify_return_value = call_async_func () |> run_join [%here] client in
      assert (rpcnotify_return_value = 1);
      let%map screen =
        wait_until_text [%here] ui ~f:(String.is_substring ~substring:"argument")
      in
      print_endline screen)
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
    │[No Name]                                                     0,0-1          All│
    │("Wrong number of arguments"                                                    │
    │ (event ((method_name async_func) (params (Nil)))))                             │
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;

let%expect_test "Too many arguments" =
  let%bind () =
    with_ui_client (fun client ui ->
      let open Deferred.Or_error.Let_syntax in
      let call_async_func =
        wrap_viml_function
          ~type_:Defun.Vim.(Integer @-> String @-> Nil @-> return Integer)
          ~function_name:"rpcnotify"
          (Client.channel client)
          "async_func"
      in
      register_request_async
        client
        ~name:"async_func"
        ~type_:Defun.Ocaml.Async.unit
        ~f:(fun ~client:_ ->
          print_s [%message "Parsing unexpectedly succeeded."];
          Deferred.Or_error.return ());
      let%bind () = Nvim.command "set cmdheight=2" |> run_join [%here] client in
      let%bind rpcnotify_return_value = call_async_func () |> run_join [%here] client in
      assert (rpcnotify_return_value = 1);
      let%map screen =
        wait_until_text [%here] ui ~f:(String.is_substring ~substring:"argument")
      in
      print_endline screen)
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
    │[No Name]                                                     0,0-1          All│
    │("Wrong number of arguments"                                                    │
    │ (event ((method_name async_func) (params (Nil)))))                             │
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;
