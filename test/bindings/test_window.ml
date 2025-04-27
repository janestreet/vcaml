open Core
open Async
open Vcaml
open Vcaml_test_helpers

let%expect_test "get_height, set_height" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = Window.set_height client Current ~height:10 in
      let%bind height = Window.get_height client Current in
      print_s [%message (height : int)];
      return ())
  in
  [%expect "(height 10)"];
  return ()
;;

let%expect_test "get_width, set_width" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = Command.exec client "vsplit" in
      let%bind () = Window.set_width client Current ~width:10 in
      let%bind width = Window.get_width client Current in
      print_s [%message (width : int)];
      return ())
  in
  [%expect "(width 10)"];
  return ()
;;

let%expect_test "get_cursor, set_cursor" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = Nvim.feedkeys client (`Raw "ithisisatest") ~mode:"n" in
      let%bind { row; col } = Window.get_cursor client Current in
      print_s [%message (row : int) (col : int)];
      let%bind () = Window.set_cursor client Current { row = 1; col = 5 } in
      let%bind position = Window.get_cursor client Current in
      print_s [%sexp (position : Position.One_indexed_row.t)];
      return ())
  in
  [%expect
    {|
    ((row 1) (col 11))
    ((row 1) (col 5))
    |}];
  return ()
;;

let%expect_test "get_buf, set_buf" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind buffer = Buffer.create client ~listed:false ~scratch:true in
      print_s [%sexp (buffer : Buffer.t)];
      let%bind () = Window.set_buf client Current ~buffer in
      let%bind buffer = Window.get_buf client Current in
      print_s [%sexp (buffer : Buffer.t)];
      return ())
  in
  [%expect
    {|
    2
    2
    |}];
  return ()
;;

let%expect_test "get_var, set_var, delete_var" =
  Dynamic.set_root Backtrace.elide true;
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = Window.set_var client Current "foo" ~type_:Bool ~value:true in
      let%bind.Deferred value = Window.get_var client Current "foo" ~type_:Bool in
      print_s [%sexp (value : bool Or_error.t)];
      let%bind () = Window.delete_var client Current "foo" in
      let%bind.Deferred value = Window.get_var client Current "foo" ~type_:Bool in
      print_s [%sexp (value : bool Or_error.t)];
      return ())
  in
  Dynamic.set_root Backtrace.elide false;
  [%expect
    {|
    (Ok true)
    (Error
     (("Vim returned error" "Key not found: foo" (error_type Validation))
      (("Called from" lib/vcaml/test/bindings/test_window.ml:LINE:COL))))
    |}];
  return ()
;;

let%expect_test "API for window-local options" =
  with_client (fun client ->
    let open Deferred.Or_error.Let_syntax in
    let%bind buffer = Nvim.get_current_buf client in
    let print_spell () =
      let%bind current_buffer =
        Window.Option.get_for_current_buffer_in_window client Current Spell
      in
      let%map new_buffers =
        Window.Option.get_for_new_buffers_opened_in_window client Current Spell
      in
      print_s [%message (current_buffer : bool) (new_buffers : bool)]
    in
    let fresh_buffer () =
      (* By marking the buffer modified we ensure [enew] creates a fresh one. *)
      let%bind () = Buffer.Option.set client Current Modified true in
      Command.exec client "enew"
    in
    let%bind () = print_spell () in
    [%expect {| ((current_buffer false) (new_buffers false)) |}];
    let%bind () = fresh_buffer () in
    let%bind () =
      Window.Option.set_for_current_buffer_in_window client Current Spell true
    in
    let%bind () = print_spell () in
    [%expect {| ((current_buffer true) (new_buffers false)) |}];
    let%bind () =
      Window.Option.set_for_new_buffers_opened_in_window client Current Spell true
    in
    let%bind () = print_spell () in
    [%expect {| ((current_buffer true) (new_buffers true)) |}];
    let%bind () = fresh_buffer () in
    let%bind () = print_spell () in
    [%expect {| ((current_buffer true) (new_buffers true)) |}];
    let%bind () = Window.set_buf client Current ~buffer in
    let%bind () = print_spell () in
    [%expect {| ((current_buffer false) (new_buffers true)) |}];
    return ())
;;

(* We don't have an analogous test for special buffer-local options because the test would
   not be interesting. The API just prevents you from setting the global value since it
   won't work anyway. But for special window-local options, what's interesting is changing
   the buffer in the window and seeing that having set the local value affects the other
   buffers. *)
let%expect_test "API for special window-local options" =
  with_client (fun client ->
    let open Deferred.Or_error.Let_syntax in
    let%bind buffer = Nvim.get_current_buf client in
    let print_winfixheight () =
      let%map winfixheight = Window.Option.get client Current Winfixheight in
      printf "%b\n" winfixheight
    in
    let fresh_buffer () =
      (* By marking the buffer modified we ensure [enew] creates a fresh one. *)
      let%bind () = Buffer.Option.set client Current Modified true in
      Command.exec client "enew"
    in
    let%bind () = print_winfixheight () in
    [%expect {| false |}];
    let%bind () = fresh_buffer () in
    let%bind () = Window.Option.set client Current Winfixheight true in
    let%bind () = print_winfixheight () in
    [%expect {| true |}];
    let%bind () = Window.set_buf client Current ~buffer in
    let%bind () = print_winfixheight () in
    [%expect {| true |}];
    return ())
;;

let%expect_test "API for global-local window options" =
  with_client (fun client ->
    let open Deferred.Or_error.Let_syntax in
    let%bind buffer = Nvim.get_current_buf client in
    let print_listchars () =
      let%bind current_listchars =
        Window.Option.get_for_current_buffer_in_window client Current Listchars
      in
      let%map default_listchars = Window.Option.get_default client Listchars in
      print_s
        [%message (current_listchars : string list) (default_listchars : string list)]
    in
    let fresh_buffer () =
      (* By marking the buffer modified we ensure [enew] creates a fresh one. *)
      let%bind () = Buffer.Option.set client Current Modified true in
      Command.exec client "enew"
    in
    let%bind () = print_listchars () in
    [%expect
      {|
      ((current_listchars ("tab:> " trail:- nbsp:+))
       (default_listchars ("tab:> " trail:- nbsp:+)))
      |}];
    let%bind () = fresh_buffer () in
    let%bind () =
      Window.Option.set_for_current_buffer_in_window client Current Listchars [ "tab:??" ]
    in
    let%bind () = print_listchars () in
    [%expect
      {| ((current_listchars (tab:??)) (default_listchars ("tab:> " trail:- nbsp:+))) |}];
    let%bind () = Window.Option.set_default client Listchars [ "tab:!!" ] in
    let%bind () = print_listchars () in
    [%expect {| ((current_listchars (tab:??)) (default_listchars (tab:!!))) |}];
    let%bind () = Window.set_buf client Current ~buffer in
    let%bind () = print_listchars () in
    [%expect {| ((current_listchars (tab:!!)) (default_listchars (tab:!!))) |}];
    return ())
;;

let%expect_test "exists, close" =
  with_client (fun client ->
    let open Deferred.Or_error.Let_syntax in
    let%bind () = Command.exec client "new" in
    let%bind window = Nvim.get_current_win client in
    let%bind valid = Window.exists client window in
    print_s [%sexp (valid : bool)];
    [%expect {| true |}];
    let%bind () =
      Window.close client Current ~when_this_is_the_buffer's_last_window:Hide
    in
    let%bind valid = Window.exists client window in
    print_s [%sexp (valid : bool)];
    [%expect {| false |}];
    return ())
;;

let%expect_test "close semantics" =
  with_client (fun client ->
    Dynamic.set_root Backtrace.elide true;
    let%bind buffer = Buffer.create client ~listed:true ~scratch:false >>| ok_exn in
    let%bind () = Buffer.Option.set client (Id buffer) Modified true >>| ok_exn in
    let test ~hidden when_this_is_the_buffer's_last_window =
      let%map result =
        let open Deferred.Or_error.Let_syntax in
        let%bind () = Nvim.Option.set client Hidden hidden in
        let%bind () =
          Command.exec client "sbuffer" ~range_or_count:(Count (buffer :> int))
        in
        let%bind () =
          Window.close client Current ~when_this_is_the_buffer's_last_window
        in
        let%bind bufloaded = Buffer.loaded client buffer in
        return [%message (bufloaded : bool)]
      in
      print_s [%sexp (result : Sexp.t Or_error.t)]
    in
    let%bind () = test ~hidden:true Hide in
    [%expect {| (Ok (bufloaded true)) |}];
    let%bind () = test ~hidden:true (Unload { if_modified = `Hide }) in
    [%expect {| (Ok (bufloaded true)) |}];
    let%bind () =
      test ~hidden:true (Unload { if_modified = `Abort_if_hiding_is_disabled })
    in
    [%expect {| (Ok (bufloaded true)) |}];
    let%bind () = test ~hidden:false Hide in
    [%expect {| (Ok (bufloaded true)) |}];
    let%bind () = test ~hidden:false (Unload { if_modified = `Hide }) in
    [%expect {| (Ok (bufloaded true)) |}];
    let%bind () =
      test ~hidden:false (Unload { if_modified = `Abort_if_hiding_is_disabled })
    in
    [%expect
      {|
      (Error
       (("Vim returned error"
         "Vim:E37: No write since last change (add ! to override)"
         (error_type Exception))
        (("Called from" lib/vcaml/test/bindings/test_window.ml:LINE:COL))))
      |}];
    Dynamic.set_root Backtrace.elide false;
    Deferred.Or_error.return ())
;;

let%expect_test "get_number, get_tab, get_position" =
  with_client (fun client ->
    let open Deferred.Or_error.Let_syntax in
    let%bind number = Window.get_number client Current in
    let%bind tab = Window.get_tab client Current in
    let%bind position = Window.get_position client Current in
    print_s [%message (number : int) (tab : Tabpage.t) (position : Position.t)];
    [%expect {| ((number 1) (tab 1) (position ((row 0) (col 0)))) |}];
    return ())
;;

let%expect_test "open_floating, get_config, set_config" =
  with_ui_client (fun client ui ->
    let open Deferred.Or_error.Let_syntax in
    let%bind config = Window.get_config client Current in
    print_s [%sexp (config : Window.Config.t option)];
    [%expect {| () |}];
    let%bind buffer = Buffer.create client ~listed:false ~scratch:true in
    let%bind () =
      Buffer.set_lines
        client
        (Id buffer)
        ~start:0
        ~end_:(-1)
        ~strict_indexing:true
        [ "This is a"; "floating window" ]
    in
    let%bind window =
      Window.open_floating
        client
        ()
        ~buffer:(Id buffer)
        ~enter:false
        ~config:
          { width = 20
          ; height = 2
          ; corner = Top_left
          ; corner_pos = Relative_to_editor { pos = { row = 2; col = 10 } }
          ; zindex = None
          ; focusable = false
          ; border = Some Single_line
          ; title = None
          }
        ~minimal_style:true
    in
    let print_screen_and_config ~(here : [%call_pos]) () =
      let%bind screen = get_screen_contents ui in
      print_endline screen;
      let%map config = Window.get_config ~here client (Id window) in
      print_s [%sexp (config : Window.Config.t option)]
    in
    let%bind () = print_screen_and_config () in
    [%expect
      {|
      ╭────────────────────────────────────────────────────────────────────────────────╮
      │                                                                                │
      │~                                                                               │
      │~         ┌────────────────────┐                                                │
      │~         │This is a           │                                                │
      │~         │floating window     │                                                │
      │~         └────────────────────┘                                                │
      │~                                                                               │
      │~                                                                               │
      │~                                                                               │
      │~                                                                               │
      │~                                                                               │
      │~                                                                               │
      │~                                                                               │
      │~                                                                               │
      │~                                                                               │
      │~                                                                               │
      │~                                                                               │
      │~                                                                               │
      │~                                                                               │
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
      ((Floating
        ((width 20) (height 2) (corner Top_left)
         (corner_pos (Relative_to_editor (pos ((row 2) (col 10))))) (zindex (50))
         (focusable false)
         (border
          ((Custom
            (((text "\226\148\140") (hl_group ()))
             ((text "\226\148\128") (hl_group ()))
             ((text "\226\148\144") (hl_group ()))
             ((text "\226\148\130") (hl_group ()))
             ((text "\226\148\152") (hl_group ()))
             ((text "\226\148\128") (hl_group ()))
             ((text "\226\148\148") (hl_group ()))
             ((text "\226\148\130") (hl_group ()))))))
         (title ()))))
      |}];
    let%bind () =
      Window.set_config
        client
        (Id window)
        (Floating
           { width = 20
           ; height = 2
           ; corner = Top_left
           ; corner_pos =
               Relative_to_window { window = Current; pos = { row = 5; col = 5 } }
           ; zindex = None
           ; focusable = false
           ; border =
               Some
                 (Custom
                    [ { text = "+"; hl_group = None }
                    ; { text = "-"; hl_group = None }
                    ; { text = "+"; hl_group = None }
                    ; { text = "|"; hl_group = None }
                    ])
           ; title = None
           })
    in
    let%bind () = print_screen_and_config () in
    [%expect
      {|
      ╭────────────────────────────────────────────────────────────────────────────────╮
      │                                                                                │
      │~                                                                               │
      │~                                                                               │
      │~                                                                               │
      │~                                                                               │
      │~    +--------------------+                                                     │
      │~    |This is a           |                                                     │
      │~    |floating window     |                                                     │
      │~    +--------------------+                                                     │
      │~                                                                               │
      │~                                                                               │
      │~                                                                               │
      │~                                                                               │
      │~                                                                               │
      │~                                                                               │
      │~                                                                               │
      │~                                                                               │
      │~                                                                               │
      │~                                                                               │
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
      ((Floating
        ((width 20) (height 2) (corner Top_left)
         (corner_pos
          (Relative_to_window (window (Id 1000)) (pos ((row 5) (col 5)))))
         (zindex (50)) (focusable false)
         (border
          ((Custom
            (((text +) (hl_group ())) ((text -) (hl_group ()))
             ((text +) (hl_group ())) ((text |) (hl_group ()))
             ((text +) (hl_group ())) ((text -) (hl_group ()))
             ((text +) (hl_group ())) ((text |) (hl_group ()))))))
         (title ()))))
      |}];
    let%bind () =
      Buffer.set_lines
        client
        Current
        ~start:0
        ~end_:(-1)
        ~strict_indexing:true
        (List.init 30 ~f:(fun nr -> [%string "This is line %{nr#Int}"]))
    in
    let%bind () = Window.set_cursor client Current { row = 21; col = 14 } in
    let%bind () =
      Buffer.set_lines
        client
        (Id buffer)
        ~start:0
        ~end_:(-1)
        ~strict_indexing:true
        [ "#" ]
    in
    let%bind () =
      Window.set_config
        client
        (Id window)
        (Floating
           { width = 1
           ; height = 1
           ; corner = Top_left
           ; corner_pos =
               Relative_to_cursor_in_current_window { pos = { row = 0; col = 0 } }
           ; zindex = None
           ; focusable = false
           ; border = None
           ; title = None
           })
    in
    let%bind () = print_screen_and_config () in
    [%expect
      {|
      ╭────────────────────────────────────────────────────────────────────────────────╮
      │This is line 0                                                                  │
      │This is line 1                                                                  │
      │This is line 2                                                                  │
      │This is line 3                                                                  │
      │This is line 4                                                                  │
      │This is line 5                                                                  │
      │This is line 6                                                                  │
      │This is line 7                                                                  │
      │This is line 8                                                                  │
      │This is line 9                                                                  │
      │This is line 10                                                                 │
      │This is line 11                                                                 │
      │This is line 12                                                                 │
      │This is line 13                                                                 │
      │This is line 14                                                                 │
      │This is line 15                                                                 │
      │This is line 16                                                                 │
      │This is line 17                                                                 │
      │This is line 18                                                                 │
      │This is line 19                                                                 │
      │This is line 2#                                                                 │
      │This is line 21                                                                 │
      │This is line 22                                                                 │
      │This is line 23                                                                 │
      │This is line 24                                                                 │
      │This is line 25                                                                 │
      │This is line 26                                                                 │
      │This is line 27                                                                 │
      │[No Name] [+]                                                 21,15          Top│
      │                                                                                │
      ╰────────────────────────────────────────────────────────────────────────────────╯
      ((Floating
        ((width 1) (height 1) (corner Top_left)
         (corner_pos
          (Relative_to_window (window (Id 1000)) (pos ((row 20) (col 14)))))
         (zindex (50)) (focusable false) (border ()) (title ()))))
      |}];
    let%bind () =
      Buffer.set_lines
        client
        Current
        ~start:25
        ~end_:26
        ~strict_indexing:true
        [ "Breaking from the pattern!" ]
    in
    let%bind () =
      Buffer.set_lines
        client
        (Id buffer)
        ~start:0
        ~end_:(-1)
        ~strict_indexing:true
        [ " Pattern violation." ]
    in
    let%bind () =
      Window.set_config
        client
        (Id window)
        (Floating
           { width = 20
           ; height = 1
           ; corner = Bottom_left
           ; corner_pos =
               Relative_to_text_in_window
                 { window = Current; text_pos = { row = 25; col = 20 }; pos = None }
           ; zindex = None
           ; focusable = false
           ; border = Some Single_line_rounded_corners
           ; title =
               Some { text = [ { text = "Diagnostic"; hl_group = None } ]; pos = Center }
           })
    in
    let%bind () = print_screen_and_config () in
    [%expect
      {|
      ╭────────────────────────────────────────────────────────────────────────────────╮
      │This is line 0                                                                  │
      │This is line 1                                                                  │
      │This is line 2                                                                  │
      │This is line 3                                                                  │
      │This is line 4                                                                  │
      │This is line 5                                                                  │
      │This is line 6                                                                  │
      │This is line 7                                                                  │
      │This is line 8                                                                  │
      │This is line 9                                                                  │
      │This is line 10                                                                 │
      │This is line 11                                                                 │
      │This is line 12                                                                 │
      │This is line 13                                                                 │
      │This is line 14                                                                 │
      │This is line 15                                                                 │
      │This is line 16                                                                 │
      │This is line 17                                                                 │
      │This is line 18                                                                 │
      │This is line 19                                                                 │
      │This is line 20                                                                 │
      │This is line 21                                                                 │
      │This is line 22     ╭─────Diagnostic─────╮                                      │
      │This is line 23     │ Pattern violation. │                                      │
      │This is line 24     ╰────────────────────╯                                      │
      │Breaking from the pattern!                                                      │
      │This is line 26                                                                 │
      │This is line 27                                                                 │
      │[No Name] [+]                                                 21,15          Top│
      │                                                                                │
      ╰────────────────────────────────────────────────────────────────────────────────╯
      ((Floating
        ((width 20) (height 1) (corner Bottom_left)
         (corner_pos
          (Relative_to_text_in_window (window (Id 1000))
           (text_pos ((row 25) (col 20))) (pos (((row 0) (col 0))))))
         (zindex (50)) (focusable false)
         (border
          ((Custom
            (((text "\226\149\173") (hl_group ()))
             ((text "\226\148\128") (hl_group ()))
             ((text "\226\149\174") (hl_group ()))
             ((text "\226\148\130") (hl_group ()))
             ((text "\226\149\175") (hl_group ()))
             ((text "\226\148\128") (hl_group ()))
             ((text "\226\149\176") (hl_group ()))
             ((text "\226\148\130") (hl_group ()))))))
         (title (((pos Center) (text (((text Diagnostic) (hl_group ()))))))))))
      |}];
    return ())
;;

let%expect_test "open_external" =
  with_client (fun client ->
    let open Deferred.Or_error.Let_syntax in
    let%bind (_ : Ui.Event.t Pipe.Reader.t) =
      Ui.attach
        client
        ~width:80
        ~height:30
        ~options:{ Ui.Options.default with ext_multigrid = true }
        ~only_enable_options_supported_by_other_attached_uis:true
    in
    let%bind () = Clock_ns.after Time_ns.Span.second |> Deferred.ok in
    let%bind buffer = Buffer.create client ~listed:false ~scratch:true in
    let%bind window =
      Window.open_external
        client
        ()
        ~buffer:(Id buffer)
        ~enter:false
        ~config:{ width = 20; height = 2; focusable = false; border = None; title = None }
        ~minimal_style:true
    in
    let print_config ~(here : [%call_pos]) () =
      let%map config = Window.get_config ~here client (Id window) in
      print_s [%sexp (config : Window.Config.t option)]
    in
    let%bind () = print_config () in
    [%expect
      {| ((External ((width 20) (height 2) (focusable false) (border ()) (title ())))) |}];
    return ())
;;

let%expect_test "eval_statusline" =
  with_client (fun client ->
    let open Deferred.Or_error.Let_syntax in
    let%bind () = Command.exec client "file" ~args:[ "foo.txt" ] in
    let test ~include_highlights =
      let%map statusline =
        Window.Fast.eval_statusline
          client
          Current
          ~max_width:25
          ~include_highlights
          "%<%1*(vcaml) %0*%f %h%m%r%=%-5.(%l,%c%V%) %P"
      in
      print_s [%sexp (statusline : Window.Statusline.t)]
    in
    let%bind () = test ~include_highlights:false in
    [%expect
      {| ((text "(vcaml) foo.txt 0,0-1 All") (display_width 25) (highlights ())) |}];
    let%bind () = test ~include_highlights:true in
    [%expect
      {|
      ((text "(vcaml) foo.txt 0,0-1 All") (display_width 25)
       (highlights
        ((((text "(vcaml) ") (hl_group (User1)))
          ((text "foo.txt 0,0-1 All") (hl_group (StatusLine)))))))
      |}];
    return ())
;;

let%expect_test "eval_winbar" =
  with_client (fun client ->
    let open Deferred.Or_error.Let_syntax in
    let%bind () = Command.exec client "file" ~args:[ "foo.txt" ] in
    let test ~include_highlights =
      let%map winbar =
        Window.Fast.eval_winbar
          client
          Current
          ~max_width:25
          ~include_highlights
          "%<%1*(vcaml) %0*%f %h%m%r%=%-5.(%l,%c%V%) %P"
      in
      print_s [%sexp (winbar : Window.Winbar.t)]
    in
    let%bind () = test ~include_highlights:false in
    [%expect
      {| ((text "(vcaml) foo.txt 0,0-1 All") (display_width 25) (highlights ())) |}];
    let%bind () = test ~include_highlights:true in
    [%expect
      {|
      ((text "(vcaml) foo.txt 0,0-1 All") (display_width 25)
       (highlights
        ((((text "(vcaml) ") (hl_group (User1)))
          ((text "foo.txt 0,0-1 All") (hl_group (WinBar)))))))
      |}];
    return ())
;;

let%expect_test "eval_statuscolumn" =
  with_client (fun client ->
    let open Deferred.Or_error.Let_syntax in
    let%bind () =
      Window.Option.set_for_current_buffer_in_window client Current Number true
    in
    let test ~include_highlights =
      let%map statuscolumn =
        Window.Fast.eval_statuscolumn
          client
          Current
          ~max_width:5
          ~include_highlights
          ~one_indexed_row:1
          "%1*+%0*%=%l|"
      in
      print_s [%sexp (statuscolumn : Window.Statuscolumn.t)]
    in
    let%bind () = test ~include_highlights:false in
    [%expect {| ((text "+  1|") (display_width 5) (highlights ())) |}];
    let%bind () = test ~include_highlights:true in
    [%expect
      {|
      ((text "+  1|") (display_width 5)
       (highlights
        ((((text +) (hl_group (User1))) ((text "  1|") (hl_group (LineNr)))))))
      |}];
    return ())
;;
