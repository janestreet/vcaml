open Core
open Async
open Vcaml
open Vcaml_test_helpers

let clear_mappings ~(here : [%call_pos]) client =
  let%bind.Deferred.Or_error () = Command.exec ~here client ~bang:true "mapclear" in
  [ "mapclear"; "vmapclear"; "lmapclear"; "tmapclear" ]
  |> Deferred.Or_error.List.iter ~how:`Parallel ~f:(Command.exec ~here client)
;;

let%expect_test "Test keymaps" =
  let test ~client mode =
    let open Deferred.Or_error.Let_syntax in
    let%bind () = clear_mappings client in
    let%bind () =
      Keymap.set
        client
        ~lhs:"a"
        ~rhs:(Viml "a")
        ~mode
        ~scope:`Global
        ~description:"Test description"
        ()
    in
    let%bind () =
      Keymap.set
        client
        ~lhs:"b"
        ~rhs:(Viml "a")
        ~mode
        ~scope:`Global
        ~recursive:true
        ~silent:true
        ()
    in
    let%bind () =
      Keymap.set
        client
        ~lhs:"c"
        ~rhs:(Viml "c")
        ~mode
        ~scope:(`Buffer_local Current)
        ~nowait:true
        ()
    in
    let%bind global_keymaps = Keymap.get client ~scope:`Global ~mode in
    let%map local_keymaps = Keymap.get client ~scope:(`Buffer_local Current) ~mode in
    let keymaps = global_keymaps @ local_keymaps in
    print_s [%message "Test" (mode : Keymap.Mode.t) (keymaps : Keymap.t list)]
  in
  let%bind () =
    with_client (fun client ->
      Deferred.Or_error.List.iter ~how:`Sequential Keymap.Mode.all ~f:(test ~client))
  in
  [%expect
    {|
    (Test (mode Normal)
     (keymaps
      (((lhs a) (mode Normal) (scope Global) (description "Test description")
        (rhs a) (nowait false) (silent false) (recursive false) (script_id -9))
       ((lhs b) (mode Normal) (scope Global) (rhs a) (nowait false) (silent true)
        (recursive true) (script_id -9))
       ((lhs c) (mode Normal) (scope (Buffer_local 1)) (rhs c) (nowait true)
        (silent false) (recursive false) (script_id -9)))))
    (Test (mode Operator_pending)
     (keymaps
      (((lhs a) (mode Operator_pending) (scope Global)
        (description "Test description") (rhs a) (nowait false) (silent false)
        (recursive false) (script_id -9))
       ((lhs b) (mode Operator_pending) (scope Global) (rhs a) (nowait false)
        (silent true) (recursive true) (script_id -9))
       ((lhs c) (mode Operator_pending) (scope (Buffer_local 1)) (rhs c)
        (nowait true) (silent false) (recursive false) (script_id -9)))))
    (Test (mode Insert)
     (keymaps
      (((lhs a) (mode Insert) (scope Global) (description "Test description")
        (rhs a) (nowait false) (silent false) (recursive false) (script_id -9))
       ((lhs b) (mode Insert) (scope Global) (rhs a) (nowait false) (silent true)
        (recursive true) (script_id -9))
       ((lhs c) (mode Insert) (scope (Buffer_local 1)) (rhs c) (nowait true)
        (silent false) (recursive false) (script_id -9)))))
    (Test (mode Cmd_line)
     (keymaps
      (((lhs a) (mode Cmd_line) (scope Global) (description "Test description")
        (rhs a) (nowait false) (silent false) (recursive false) (script_id -9))
       ((lhs b) (mode Cmd_line) (scope Global) (rhs a) (nowait false)
        (silent true) (recursive true) (script_id -9))
       ((lhs c) (mode Cmd_line) (scope (Buffer_local 1)) (rhs c) (nowait true)
        (silent false) (recursive false) (script_id -9)))))
    (Test (mode Select)
     (keymaps
      (((lhs a) (mode Select) (scope Global) (description "Test description")
        (rhs a) (nowait false) (silent false) (recursive false) (script_id -9))
       ((lhs b) (mode Select) (scope Global) (rhs a) (nowait false) (silent true)
        (recursive true) (script_id -9))
       ((lhs c) (mode Select) (scope (Buffer_local 1)) (rhs c) (nowait true)
        (silent false) (recursive false) (script_id -9)))))
    (Test (mode Visual)
     (keymaps
      (((lhs a) (mode Visual) (scope Global) (description "Test description")
        (rhs a) (nowait false) (silent false) (recursive false) (script_id -9))
       ((lhs b) (mode Visual) (scope Global) (rhs a) (nowait false) (silent true)
        (recursive true) (script_id -9))
       ((lhs c) (mode Visual) (scope (Buffer_local 1)) (rhs c) (nowait true)
        (silent false) (recursive false) (script_id -9)))))
    (Test (mode Terminal)
     (keymaps
      (((lhs a) (mode Terminal) (scope Global) (description "Test description")
        (rhs a) (nowait false) (silent false) (recursive false) (script_id -9))
       ((lhs b) (mode Terminal) (scope Global) (rhs a) (nowait false)
        (silent true) (recursive true) (script_id -9))
       ((lhs c) (mode Terminal) (scope (Buffer_local 1)) (rhs c) (nowait true)
        (silent false) (recursive false) (script_id -9)))))
    (Test (mode Visual_and_select)
     (keymaps
      (((lhs a) (mode Visual_and_select) (scope Global)
        (description "Test description") (rhs a) (nowait false) (silent false)
        (recursive false) (script_id -9))
       ((lhs b) (mode Visual_and_select) (scope Global) (rhs a) (nowait false)
        (silent true) (recursive true) (script_id -9))
       ((lhs c) (mode Visual_and_select) (scope (Buffer_local 1)) (rhs c)
        (nowait true) (silent false) (recursive false) (script_id -9)))))
    (Test (mode Normal_and_visual_and_operator_pending)
     (keymaps
      (((lhs a) (mode Normal_and_visual_and_operator_pending) (scope Global)
        (description "Test description") (rhs a) (nowait false) (silent false)
        (recursive false) (script_id -9))
       ((lhs b) (mode Normal_and_visual_and_operator_pending) (scope Global)
        (rhs a) (nowait false) (silent true) (recursive true) (script_id -9))
       ((lhs c) (mode Normal_and_visual_and_operator_pending)
        (scope (Buffer_local 1)) (rhs c) (nowait true) (silent false)
        (recursive false) (script_id -9)))))
    (Test (mode Insert_and_command_line)
     (keymaps
      (((lhs a) (mode Insert_and_command_line) (scope Global)
        (description "Test description") (rhs a) (nowait false) (silent false)
        (recursive false) (script_id -9))
       ((lhs b) (mode Insert_and_command_line) (scope Global) (rhs a)
        (nowait false) (silent true) (recursive true) (script_id -9))
       ((lhs c) (mode Insert_and_command_line) (scope (Buffer_local 1)) (rhs c)
        (nowait true) (silent false) (recursive false) (script_id -9)))))
    (Test (mode Language)
     (keymaps
      (((lhs a) (mode Language) (scope Global) (description "Test description")
        (rhs a) (nowait false) (silent false) (recursive false) (script_id -9))
       ((lhs b) (mode Language) (scope Global) (rhs a) (nowait false)
        (silent true) (recursive true) (script_id -9))
       ((lhs c) (mode Language) (scope (Buffer_local 1)) (rhs c) (nowait true)
        (silent false) (recursive false) (script_id -9)))))
    |}];
  return ()
;;

let%expect_test "Test unsetting keymap in specific mode" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = clear_mappings client in
      let%bind () =
        Keymap.set
          client
          ~lhs:"a"
          ~rhs:(Viml "a")
          ~mode:Normal_and_visual_and_operator_pending
          ~scope:`Global
          ()
      in
      let%bind () = Keymap.unset client ~lhs:"a" ~mode:Visual ~scope:`Global in
      let%bind () = Keymap.unset client ~lhs:"a" ~mode:Normal ~scope:`Global in
      let%map keymaps =
        Keymap.get client ~scope:`Global ~mode:Normal_and_visual_and_operator_pending
      in
      print_s [%message "Remaining keymaps" (keymaps : Keymap.t list)])
  in
  [%expect
    {|
    ("Remaining keymaps"
     (keymaps
      (((lhs a) (mode Operator_pending) (scope Global) (rhs a) (nowait false)
        (silent false) (recursive false) (script_id -9))
       ((lhs a) (mode Select) (scope Global) (rhs a) (nowait false)
        (silent false) (recursive false) (script_id -9)))))
    |}];
  return ()
;;

let%expect_test "Test replacing keycodes in an expr mapping" =
  with_ui_client (fun client ui ->
    let open Deferred.Or_error.Let_syntax in
    let%bind () = clear_mappings client in
    let%bind () =
      Nvim.exec_viml
        client
        {| function! SayHi()
             return ":echo 'Hi'<CR>"
           endfunction |}
    in
    let test ~replace_keycodes =
      let%bind () =
        Keymap.set_expr
          client
          ~lhs:"X"
          ~rhs:(Viml "SayHi()")
          ~replace_keycodes
          ~mode:Normal
          ~scope:`Global
          ()
      in
      let%bind keymaps = Keymap.get client ~scope:`Global ~mode:Normal in
      print_s [%sexp (keymaps : Keymap.t list)];
      let%bind () = Nvim.feedkeys client (`Raw "X") ~mode:"t" in
      get_screen_contents ui >>| print_endline
    in
    let%bind () = test ~replace_keycodes:true in
    [%expect
      {|
      (((lhs X) (mode Normal) (scope Global) (rhs "SayHi()")
        (expr (Replace_keycodes true)) (nowait false) (silent false)
        (recursive false) (script_id -9)))
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
      │Hi                                                                              │
      ╰────────────────────────────────────────────────────────────────────────────────╯
      |}];
    let%bind () = test ~replace_keycodes:false in
    [%expect
      {|
      (((lhs X) (mode Normal) (scope Global) (rhs "SayHi()")
        (expr (Replace_keycodes false)) (nowait false) (silent false)
        (recursive false) (script_id -9)))
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
      │:echo 'Hi'<CR>                                                                  │
      ╰────────────────────────────────────────────────────────────────────────────────╯
      |}];
    return ())
;;
