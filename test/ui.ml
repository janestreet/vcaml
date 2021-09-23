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
    (((channel_id (Id 1)) (height 200) (width 100)
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
        Vcaml.Nvim.feedkeys ~keys:"ihello world" ~mode:"n" ~escape_csi:true
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
        Vcaml.Nvim.feedkeys ~keys:"ihello world" ~mode:"n" ~escape_csi:true
        |> run_join [%here] client
      in
      let%bind () = Vcaml.Nvim.command ~command:"vsplit" |> run_join [%here] client in
      let%bind () = Vcaml.Nvim.command ~command:"split" |> run_join [%here] client in
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
        let%bind () =
          Vcaml.Nvim.command ~command:"e term://sh" |> run_join [%here] client
        in
        let%bind () =
          Vcaml.Nvim.command ~command:"file my-terminal" |> run_join [%here] client
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
      let%bind () =
        Vcaml.Nvim.command ~command:"e term://sh" |> run_join [%here] client
      in
      let%bind () =
        Vcaml.Nvim.command ~command:"file my-terminal" |> run_join [%here] client
      in
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

let%expect_test "Show echoed content in command line" =
  let%bind () =
    let open Deferred.Or_error.Let_syntax in
    with_ui_client (fun client ui ->
      let%bind () =
        run_join [%here] client (Nvim.command ~command:"echo 'Hello, world!'")
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
    │Hello, world!                                                                   │
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;

let%expect_test "Naively calling [echo] from inside [rpcrequest] fails" =
  let%bind () =
    let open Deferred.Or_error.Let_syntax in
    let rpc ~keyboard_interrupted:_ client ui () =
      let%bind () =
        run_join
          [%here]
          client
          (Nvim.command
             ~command:
               "echo 'If this message appears we should get rid of [echo_in_rpcrequest]'")
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
          ~type_:Defun.Ocaml.Sync.(Nil @-> return Nil)
          ~f:(rpc client ui)
        |> ok_exn
      in
      let channel = Client.rpc_channel_id client in
      Nvim.command ~command:(sprintf "call rpcrequest(%d, 'rpc', v:null)" channel)
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
    │                                                                                │
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;

let%expect_test "[echo_in_rpcrequest] hack" =
  let%bind () =
    let open Deferred.Or_error.Let_syntax in
    let rpc ~keyboard_interrupted:_ client ui () =
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
          ~type_:Defun.Ocaml.Sync.(Nil @-> return Nil)
          ~f:(rpc client ui)
        |> ok_exn
      in
      let channel = Client.rpc_channel_id client in
      Nvim.command ~command:(sprintf "call rpcrequest(%d, 'rpc', v:null)" channel)
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
