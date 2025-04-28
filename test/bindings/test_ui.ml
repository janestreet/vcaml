module Do_not_move = struct
  let line2 = [%here]
end

open Core
open Async
open Vcaml
open Vcaml_test_helpers

let%expect_test "Simple test of attach, detach, describe_attached_uis" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind descriptions = Ui.describe_attached_uis client in
      print_s [%sexp (descriptions : Ui.Description.t list)];
      let%bind reader =
        Ui.attach
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
          ~only_enable_options_supported_by_other_attached_uis:true
      in
      let%bind descriptions = Ui.describe_attached_uis client in
      print_s [%sexp (descriptions : Ui.Description.t list)];
      Pipe.close_read reader;
      (* Yield to allow the detach message to be sent to Neovim. *)
      let%bind () = Scheduler.yield_until_no_jobs_remain () |> Deferred.ok in
      let%bind descriptions = Ui.describe_attached_uis client in
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
    ()
    |}];
  return ()
;;

let%expect_test "get_screen_contents" =
  let%bind () =
    let open Deferred.Or_error.Let_syntax in
    with_ui_client (fun _client ui ->
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
    |}];
  return ()
;;

let%expect_test "get screen contents multiple times" =
  let%bind () =
    let open Deferred.Or_error.Let_syntax in
    with_ui_client (fun client ui ->
      let%bind screen = get_screen_contents ui in
      print_endline screen;
      let%bind () = Nvim.feedkeys client (`Raw "ihello world") ~mode:"n" in
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
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  return ()
;;

let%expect_test "screen contents after typing hello world" =
  let%bind () =
    let open Deferred.Or_error.Let_syntax in
    with_ui_client (fun client ui ->
      let%bind () = Nvim.feedkeys client (`Raw "ihello world") ~mode:"n" in
      let%bind () = Command.exec client "vsplit" in
      let%bind () = Command.exec client "split" in
      let%bind screen = get_screen_contents ui in
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
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  return ()
;;

let%expect_test "timeout occurs" =
  let%map () =
    Expect_test_helpers_async.require_does_raise_async (fun () ->
      let open Deferred.Or_error.Let_syntax in
      with_ui_client (fun _client ui ->
        let%bind (_ : string) =
          wait_until_text
            ~here:Do_not_move.line2
            ui
            ~timeout:(Time_ns.Span.of_sec 0.01)
            ~f:(Fn.const false)
        in
        return ()))
  in
  [%expect
    {|
    ("ERROR: timeout when looking for value on screen"
     ("Called from" lib/vcaml/test/bindings/test_ui.ml:2:14))
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
    ("ERROR: timeout when looking for value on screen"
     ("Called from" lib/vcaml/test/bindings/test_ui.ml:2:14))
    |}]
;;
