open! Core_kernel
open! Async
open! Import
open Vcaml
open Test_client

let%expect_test "Test keymaps" =
  let test ~client ~current_buffer mode =
    let open Deferred.Or_error.Let_syntax in
    let query =
      let open Api_call.Or_error.Let_syntax in
      let%map () = Keymap.set ~lhs:"a" ~rhs:"a" ~mode ()
      and () = Keymap.set ~lhs:"b" ~rhs:"a" ~mode ~recursive:true ~silent:true ()
      and () = Keymap.set ~lhs:"c" ~rhs:"c" ~mode ~scope:`Current_buffer ~nowait:true ()
      and global_keymaps = Keymap.get ~scope:`Global ~mode
      and local_keymaps = Keymap.get ~scope:(`Buffer_local current_buffer) ~mode in
      global_keymaps @ local_keymaps
    in
    let%map keymaps = run_join client query in
    print_s [%message "Test" (mode : Keymap.Mode.t) (keymaps : Keymap.t list)]
  in
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind current_buffer = run_join client Vcaml.Nvim.get_current_buf in
      Deferred.Or_error.List.iter Keymap.Mode.all ~f:(test ~client ~current_buffer))
  in
  [%expect
    {|
    (Test (mode Normal)
     (keymaps
      (((lhs a) (rhs a) (mode Normal) (scope Global) (expr false) (nowait false)
        (silent false) (recursive false) (sid 0))
       ((lhs b) (rhs a) (mode Normal) (scope Global) (expr false) (nowait false)
        (silent true) (recursive true) (sid 0))
       ((lhs c) (rhs c) (mode Normal) (scope (Buffer_local 1)) (expr false)
        (nowait true) (silent false) (recursive false) (sid 0)))))
    (Test (mode Operator_pending)
     (keymaps
      (((lhs a) (rhs a) (mode Operator_pending) (scope Global) (expr false)
        (nowait false) (silent false) (recursive false) (sid 0))
       ((lhs b) (rhs a) (mode Operator_pending) (scope Global) (expr false)
        (nowait false) (silent true) (recursive true) (sid 0))
       ((lhs c) (rhs c) (mode Operator_pending) (scope (Buffer_local 1))
        (expr false) (nowait true) (silent false) (recursive false) (sid 0)))))
    (Test (mode Insert)
     (keymaps
      (((lhs a) (rhs a) (mode Insert) (scope Global) (expr false) (nowait false)
        (silent false) (recursive false) (sid 0))
       ((lhs b) (rhs a) (mode Insert) (scope Global) (expr false) (nowait false)
        (silent true) (recursive true) (sid 0))
       ((lhs c) (rhs c) (mode Insert) (scope (Buffer_local 1)) (expr false)
        (nowait true) (silent false) (recursive false) (sid 0)))))
    (Test (mode Cmd_line)
     (keymaps
      (((lhs a) (rhs a) (mode Cmd_line) (scope Global) (expr false)
        (nowait false) (silent false) (recursive false) (sid 0))
       ((lhs b) (rhs a) (mode Cmd_line) (scope Global) (expr false)
        (nowait false) (silent true) (recursive true) (sid 0))
       ((lhs c) (rhs c) (mode Cmd_line) (scope (Buffer_local 1)) (expr false)
        (nowait true) (silent false) (recursive false) (sid 0)))))
    (Test (mode Select)
     (keymaps
      (((lhs a) (rhs a) (mode Select) (scope Global) (expr false) (nowait false)
        (silent false) (recursive false) (sid 0))
       ((lhs b) (rhs a) (mode Select) (scope Global) (expr false) (nowait false)
        (silent true) (recursive true) (sid 0))
       ((lhs c) (rhs c) (mode Select) (scope (Buffer_local 1)) (expr false)
        (nowait true) (silent false) (recursive false) (sid 0)))))
    (Test (mode Visual)
     (keymaps
      (((lhs a) (rhs a) (mode Visual) (scope Global) (expr false) (nowait false)
        (silent false) (recursive false) (sid 0))
       ((lhs b) (rhs a) (mode Visual) (scope Global) (expr false) (nowait false)
        (silent true) (recursive true) (sid 0))
       ((lhs c) (rhs c) (mode Visual) (scope (Buffer_local 1)) (expr false)
        (nowait true) (silent false) (recursive false) (sid 0)))))
    (Test (mode Terminal)
     (keymaps
      (((lhs a) (rhs a) (mode Terminal) (scope Global) (expr false)
        (nowait false) (silent false) (recursive false) (sid 0))
       ((lhs b) (rhs a) (mode Terminal) (scope Global) (expr false)
        (nowait false) (silent true) (recursive true) (sid 0))
       ((lhs c) (rhs c) (mode Terminal) (scope (Buffer_local 1)) (expr false)
        (nowait true) (silent false) (recursive false) (sid 0)))))
    (Test (mode Visual_and_select)
     (keymaps
      (((lhs a) (rhs a) (mode Visual_and_select) (scope Global) (expr false)
        (nowait false) (silent false) (recursive false) (sid 0))
       ((lhs b) (rhs a) (mode Visual_and_select) (scope Global) (expr false)
        (nowait false) (silent true) (recursive true) (sid 0))
       ((lhs c) (rhs c) (mode Visual_and_select) (scope (Buffer_local 1))
        (expr false) (nowait true) (silent false) (recursive false) (sid 0)))))
    (Test (mode Normal_and_visual_and_operator_pending)
     (keymaps
      (((lhs a) (rhs a) (mode Normal_and_visual_and_operator_pending)
        (scope Global) (expr false) (nowait false) (silent false)
        (recursive false) (sid 0))
       ((lhs b) (rhs a) (mode Normal_and_visual_and_operator_pending)
        (scope Global) (expr false) (nowait false) (silent true) (recursive true)
        (sid 0))
       ((lhs c) (rhs c) (mode Normal_and_visual_and_operator_pending)
        (scope (Buffer_local 1)) (expr false) (nowait true) (silent false)
        (recursive false) (sid 0)))))
    (Test (mode Insert_and_command_line)
     (keymaps
      (((lhs a) (rhs a) (mode Insert_and_command_line) (scope Global)
        (expr false) (nowait false) (silent false) (recursive false) (sid 0))
       ((lhs b) (rhs a) (mode Insert_and_command_line) (scope Global)
        (expr false) (nowait false) (silent true) (recursive true) (sid 0))
       ((lhs c) (rhs c) (mode Insert_and_command_line) (scope (Buffer_local 1))
        (expr false) (nowait true) (silent false) (recursive false) (sid 0)))))
    (Test (mode Language)
     (keymaps
      (((lhs a) (rhs a) (mode Language) (scope Global) (expr false)
        (nowait false) (silent false) (recursive false) (sid 0))
       ((lhs b) (rhs a) (mode Language) (scope Global) (expr false)
        (nowait false) (silent true) (recursive true) (sid 0))
       ((lhs c) (rhs c) (mode Language) (scope (Buffer_local 1)) (expr false)
        (nowait true) (silent false) (recursive false) (sid 0))))) |}];
  return ()
;;
