open Core
open Async
open Vcaml
open Vcaml_test_helpers

let clear_mappings =
  [ "mapclear"; "mapclear!"; "vmapclear"; "lmapclear"; "tmapclear" ]
  |> List.map ~f:Nvim.command
  |> Api_call.Or_error.all_unit
;;


let%expect_test "Test keymaps" =
  let test ~client mode =
    let open Deferred.Or_error.Let_syntax in
    let query =
      let open Api_call.Or_error.Let_syntax in
      let%map () = clear_mappings
      and () =
        Keymap.set
          ~lhs:"a"
          ~rhs:"a"
          ~mode
          ~scope:`Global
          ~description:"Test description"
          ()
      and () =
        Keymap.set ~lhs:"b" ~rhs:"a" ~mode ~scope:`Global ~recursive:true ~silent:true ()
      and () =
        Keymap.set ~lhs:"c" ~rhs:"c" ~mode ~scope:(`Buffer_local Current) ~nowait:true ()
      and global_keymaps = Keymap.get ~scope:`Global ~mode
      and local_keymaps = Keymap.get ~scope:(`Buffer_local Current) ~mode in
      global_keymaps @ local_keymaps
    in
    let%map keymaps = run_join [%here] client query in
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
      (((description "Test description") (lhs a) (rhs a) (mode Normal)
        (scope Global) (expr false) (nowait false) (silent false)
        (recursive false) (sid -9))
       ((lhs b) (rhs a) (mode Normal) (scope Global) (expr false) (nowait false)
        (silent true) (recursive true) (sid -9))
       ((lhs c) (rhs c) (mode Normal) (scope (Buffer_local 1)) (expr false)
        (nowait true) (silent false) (recursive false) (sid -9)))))
    (Test (mode Operator_pending)
     (keymaps
      (((description "Test description") (lhs a) (rhs a) (mode Operator_pending)
        (scope Global) (expr false) (nowait false) (silent false)
        (recursive false) (sid -9))
       ((lhs b) (rhs a) (mode Operator_pending) (scope Global) (expr false)
        (nowait false) (silent true) (recursive true) (sid -9))
       ((lhs c) (rhs c) (mode Operator_pending) (scope (Buffer_local 1))
        (expr false) (nowait true) (silent false) (recursive false) (sid -9)))))
    (Test (mode Insert)
     (keymaps
      (((description "Test description") (lhs a) (rhs a) (mode Insert)
        (scope Global) (expr false) (nowait false) (silent false)
        (recursive false) (sid -9))
       ((lhs b) (rhs a) (mode Insert) (scope Global) (expr false) (nowait false)
        (silent true) (recursive true) (sid -9))
       ((lhs c) (rhs c) (mode Insert) (scope (Buffer_local 1)) (expr false)
        (nowait true) (silent false) (recursive false) (sid -9)))))
    (Test (mode Cmd_line)
     (keymaps
      (((description "Test description") (lhs a) (rhs a) (mode Cmd_line)
        (scope Global) (expr false) (nowait false) (silent false)
        (recursive false) (sid -9))
       ((lhs b) (rhs a) (mode Cmd_line) (scope Global) (expr false)
        (nowait false) (silent true) (recursive true) (sid -9))
       ((lhs c) (rhs c) (mode Cmd_line) (scope (Buffer_local 1)) (expr false)
        (nowait true) (silent false) (recursive false) (sid -9)))))
    (Test (mode Select)
     (keymaps
      (((description "Test description") (lhs a) (rhs a) (mode Select)
        (scope Global) (expr false) (nowait false) (silent false)
        (recursive false) (sid -9))
       ((lhs b) (rhs a) (mode Select) (scope Global) (expr false) (nowait false)
        (silent true) (recursive true) (sid -9))
       ((lhs c) (rhs c) (mode Select) (scope (Buffer_local 1)) (expr false)
        (nowait true) (silent false) (recursive false) (sid -9)))))
    (Test (mode Visual)
     (keymaps
      (((description "Test description") (lhs a) (rhs a) (mode Visual)
        (scope Global) (expr false) (nowait false) (silent false)
        (recursive false) (sid -9))
       ((lhs b) (rhs a) (mode Visual) (scope Global) (expr false) (nowait false)
        (silent true) (recursive true) (sid -9))
       ((lhs c) (rhs c) (mode Visual) (scope (Buffer_local 1)) (expr false)
        (nowait true) (silent false) (recursive false) (sid -9)))))
    (Test (mode Terminal)
     (keymaps
      (((description "Test description") (lhs a) (rhs a) (mode Terminal)
        (scope Global) (expr false) (nowait false) (silent false)
        (recursive false) (sid -9))
       ((lhs b) (rhs a) (mode Terminal) (scope Global) (expr false)
        (nowait false) (silent true) (recursive true) (sid -9))
       ((lhs c) (rhs c) (mode Terminal) (scope (Buffer_local 1)) (expr false)
        (nowait true) (silent false) (recursive false) (sid -9)))))
    (Test (mode Visual_and_select)
     (keymaps
      (((description "Test description") (lhs a) (rhs a) (mode Visual_and_select)
        (scope Global) (expr false) (nowait false) (silent false)
        (recursive false) (sid -9))
       ((lhs b) (rhs a) (mode Visual_and_select) (scope Global) (expr false)
        (nowait false) (silent true) (recursive true) (sid -9))
       ((lhs c) (rhs c) (mode Visual_and_select) (scope (Buffer_local 1))
        (expr false) (nowait true) (silent false) (recursive false) (sid -9)))))
    (Test (mode Normal_and_visual_and_operator_pending)
     (keymaps
      (((description "Test description") (lhs a) (rhs a)
        (mode Normal_and_visual_and_operator_pending) (scope Global) (expr false)
        (nowait false) (silent false) (recursive false) (sid -9))
       ((lhs b) (rhs a) (mode Normal_and_visual_and_operator_pending)
        (scope Global) (expr false) (nowait false) (silent true) (recursive true)
        (sid -9))
       ((lhs c) (rhs c) (mode Normal_and_visual_and_operator_pending)
        (scope (Buffer_local 1)) (expr false) (nowait true) (silent false)
        (recursive false) (sid -9)))))
    (Test (mode Insert_and_command_line)
     (keymaps
      (((description "Test description") (lhs a) (rhs a)
        (mode Insert_and_command_line) (scope Global) (expr false) (nowait false)
        (silent false) (recursive false) (sid -9))
       ((lhs b) (rhs a) (mode Insert_and_command_line) (scope Global)
        (expr false) (nowait false) (silent true) (recursive true) (sid -9))
       ((lhs c) (rhs c) (mode Insert_and_command_line) (scope (Buffer_local 1))
        (expr false) (nowait true) (silent false) (recursive false) (sid -9)))))
    (Test (mode Language)
     (keymaps
      (((description "Test description") (lhs a) (rhs a) (mode Language)
        (scope Global) (expr false) (nowait false) (silent false)
        (recursive false) (sid -9))
       ((lhs b) (rhs a) (mode Language) (scope Global) (expr false)
        (nowait false) (silent true) (recursive true) (sid -9))
       ((lhs c) (rhs c) (mode Language) (scope (Buffer_local 1)) (expr false)
        (nowait true) (silent false) (recursive false) (sid -9))))) |}];
  return ()
;;

let%expect_test "Test unsetting keymap in specific mode" =
  let%bind () =
    with_client (fun client ->
      let query =
        let open Api_call.Or_error.Let_syntax in
        let%map () = clear_mappings
        and () =
          Keymap.set
            ~lhs:"a"
            ~rhs:"a"
            ~mode:Normal_and_visual_and_operator_pending
            ~scope:`Global
            ()
        and () = Keymap.unset ~lhs:"a" ~mode:Visual ~scope:`Global
        and () = Keymap.unset ~lhs:"a" ~mode:Normal ~scope:`Global
        and remaining_keymaps =
          Keymap.get ~scope:`Global ~mode:Normal_and_visual_and_operator_pending
        in
        remaining_keymaps
      in
      let open Deferred.Or_error.Let_syntax in
      let%map keymaps = run_join [%here] client query in
      print_s [%message "Remaining keymaps" (keymaps : Keymap.t list)])
  in
  [%expect
    {|
    ("Remaining keymaps"
     (keymaps
      (((lhs a) (rhs a) (mode Operator_pending) (scope Global) (expr false)
        (nowait false) (silent false) (recursive false) (sid -9))
       ((lhs a) (rhs a) (mode Select) (scope Global) (expr false) (nowait false)
        (silent false) (recursive false) (sid -9))))) |}];
  return ()
;;
