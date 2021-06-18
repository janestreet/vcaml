open! Core
open! Async
open! Import
open! Vcaml
open Test_client

let%expect_test "Simple test of attach, detach, describe_attached_uis" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind descriptions = Ui.describe_attached_uis |> run_join client in
      print_s [%sexp (descriptions : Ui.Description.t list)];
      let%bind ui =
        Ui.attach
          client
          ~width:100
          ~height:200
          ~options:
            { ext_cmdline = true
            ; ext_hlstate = false
            ; ext_linegrid = false
            ; ext_popupmenu = false
            ; ext_tabline = true
            ; ext_wildmenu = false
            ; rgb = true
            }
          ~on_event:ignore
      in
      let%bind descriptions = Ui.describe_attached_uis |> run_join client in
      print_s [%sexp (descriptions : Ui.Description.t list)];
      let%bind () = Ui.detach ui in
      let%bind descriptions = Ui.describe_attached_uis |> run_join client in
      print_s [%sexp (descriptions : Ui.Description.t list)];
      return ())
  in
  [%expect
    {|
    ()
    (((channel_id (Id 1)) (height 200) (width 100)
      (options
       ((ext_cmdline true) (ext_hlstate false) (ext_linegrid false)
        (ext_popupmenu false) (ext_tabline true) (ext_wildmenu false) (rgb true)))))
    () |}];
  return ()
;;

let%expect_test "get_screen_contents" =
  let%bind () =
    let open Deferred.Or_error.Let_syntax in
    with_client (fun client ->
      let%bind screen = get_screen_contents client in
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
    with_client (fun client ->
      let%bind screen = get_screen_contents client in
      print_endline screen;
      let%bind () =
        Vcaml.Nvim.feedkeys ~keys:"ihello world" ~mode:"n" ~escape_csi:true
        |> run_join client
      in
      let%bind screen = get_screen_contents client in
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
    │                                                                                │
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;

let%expect_test "screen contents after typing hello world" =
  let%bind () =
    let open Deferred.Or_error.Let_syntax in
    with_client (fun client ->
      let%bind () =
        Vcaml.Nvim.feedkeys ~keys:"ihello world" ~mode:"n" ~escape_csi:true
        |> run_join client
      in
      let%bind () = Vcaml.Nvim.command ~command:"vsplit" |> run_join client in
      let%bind () = Vcaml.Nvim.command ~command:"split" |> run_join client in
      let%bind screen = get_screen_contents client in
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
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │[No Name] [+]         1,12           All [No Name] [+]        1,12           All│
    │                                                                                │
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;

let%expect_test "timeout occurs" =
  let%map () =
    Expect_test_helpers_async.require_does_raise_async [%here] (fun () ->
      let open Deferred.Or_error.Let_syntax in
      with_client (fun client ->
        let%bind () = Vcaml.Nvim.command ~command:"e term://sh" |> run_join client in
        let%bind () =
          Vcaml.Nvim.command ~command:"file my-terminal" |> run_join client
        in
        let%bind (_ : string) =
          wait_until_text client ~f:(String.is_substring ~substring:"sh-4.2$")
        in
        let%bind (_ : string) =
          wait_until_text
            client
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
    with_client (fun client ->
      let%bind () = Vcaml.Nvim.command ~command:"e term://sh" |> run_join client in
      let%bind () = Vcaml.Nvim.command ~command:"file my-terminal" |> run_join client in
      let%bind screen =
        wait_until_text client ~f:(String.is_substring ~substring:"sh-4.2$")
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

