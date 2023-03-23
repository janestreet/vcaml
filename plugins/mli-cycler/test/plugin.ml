open! Core
open! Async
open! Vcaml
open! Mli_plugin.Plugin

let () = Backtrace.elide := true

let setup_files_for_testing ~empty_files ~files_with_includes ~temp_dir =
  let create_empty file = Writer.save file ~contents:"" in
  let create_with_include file intf = Writer.save file ~contents:("include " ^ intf) in
  let%bind () =
    empty_files
    |> List.map ~f:(Core.( ^/ ) temp_dir)
    |> Deferred.List.iter ~how:`Sequential ~f:create_empty
  in
  let%map () =
    files_with_includes
    |> List.map ~f:(fun (file, incl) -> Core.( ^/ ) temp_dir file, incl)
    |> Deferred.List.iter ~how:`Sequential ~f:(fun (file, incl) ->
      create_with_include file incl)
  in
  Ok ()
;;

let print_current_file client =
  let open Deferred.Or_error.Let_syntax in
  let%bind name = run_join [%here] client (Buffer.get_name Current) in
  let _dir, current_file = Core.Filename.split name in
  print_s [%message (current_file : string)];
  return ()
;;

let setup_client ~empty_files ~files_with_includes ~entry_point ~client =
  let open Deferred.Or_error.Let_syntax in
  let temp_dir = Filename_unix.temp_dir "mli_plugin" "test" in
  let%bind () = setup_files_for_testing ~empty_files ~files_with_includes ~temp_dir in
  let entry_file_full_path = Core.( ^/ ) temp_dir entry_point in
  run_join [%here] client (Nvim.command (":e " ^ entry_file_full_path))
;;

let rec repeat_n_times times ~f =
  match times with
  | 0 -> Deferred.Or_error.return ()
  | n ->
    let%bind.Deferred.Or_error () = f () in
    repeat_n_times (n - 1) ~f
;;

let cycle_backward client =
  print_s (Sexp.Atom "Cycling backward...");
  let%bind.Deferred.Or_error () = For_testing.prev_file_pattern client in
  print_current_file client
;;

let cycle_forward client =
  print_s (Sexp.Atom "Cycling forward...");
  let%bind.Deferred.Or_error () = For_testing.next_file_pattern client in
  print_current_file client
;;

let print_file_list client =
  let open Deferred.Or_error.Let_syntax in
  let%bind file_list = run_join [%here] client (Nvim.source "1 message") in
  print_s [%message (file_list : string)];
  return ()
;;

let%expect_test "lists the files and ignores ones which don't match" =
  let empty_files =
    [ "foo.ml"
    ; "foo.mli"
    ; "foo_intf.ml"
    ; "foo0.ml"
    ; "foo0.mli"
    ; "foo0_intf.ml"
    ; "bar.ml"
    ; "bar.mli"
    ; "bar_intf.ml"
    ]
  in
  let%bind () =
    Vcaml_test_helpers.with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () =
        setup_client ~empty_files ~files_with_includes:[] ~entry_point:"foo.ml" ~client
      in
      let%bind () = For_testing.echo_file_patterns client in
      let%bind () = print_file_list client in
      return ())
  in
  [%expect
    {| (file_list "foo.ml, foo.mli, foo_intf.ml, foo0.ml, foo0.mli, foo0_intf.ml") |}];
  return ()
;;

let%expect_test "cycles forwards and backwards in the long list, wrapping around at each \
                 end"
  =
  let empty_files =
    [ "foo.ml"
    ; "foo.mli"
    ; "foo_intf.ml"
    ; "foo0.ml"
    ; "foo0.mli"
    ; "foo0_intf.ml"
    ; "foo.mly"
    ; "foo.mll"
    ; "bar.ml"
    ; "bar.mli"
    ; "bar_intf.ml"
    ]
  in
  let%bind () =
    Vcaml_test_helpers.with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () =
        setup_client ~empty_files ~files_with_includes:[] ~entry_point:"foo.ml" ~client
      in
      let%bind () = print_current_file client in
      let%bind () = repeat_n_times 8 ~f:(fun () -> cycle_forward client) in
      repeat_n_times 8 ~f:(fun () -> cycle_backward client))
  in
  [%expect
    {|
    (current_file foo.ml)
    "Cycling forward..."
    (current_file foo.mli)
    "Cycling forward..."
    (current_file foo_intf.ml)
    "Cycling forward..."
    (current_file foo0.ml)
    "Cycling forward..."
    (current_file foo0.mli)
    "Cycling forward..."
    (current_file foo0_intf.ml)
    "Cycling forward..."
    (current_file foo.mll)
    "Cycling forward..."
    (current_file foo.mly)
    "Cycling forward..."
    (current_file foo.ml)
    "Cycling backward..."
    (current_file foo.mly)
    "Cycling backward..."
    (current_file foo.mll)
    "Cycling backward..."
    (current_file foo0_intf.ml)
    "Cycling backward..."
    (current_file foo0.mli)
    "Cycling backward..."
    (current_file foo0.ml)
    "Cycling backward..."
    (current_file foo_intf.ml)
    "Cycling backward..."
    (current_file foo.mli)
    "Cycling backward..."
    (current_file foo.ml) |}];
  return ()
;;

let%expect_test "does nothing if interaction is attempted from a non-ml file" =
  let empty_files = [ "foo.ml"; "foo.mli"; "foo_intf.ml"; "bar.baz" ] in
  let%bind () =
    Vcaml_test_helpers.with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () =
        setup_client ~empty_files ~files_with_includes:[] ~entry_point:"bar.baz" ~client
      in
      let%bind () = For_testing.echo_file_patterns client in
      let%bind () = print_file_list client in
      let%bind () = print_current_file client in
      let%bind () = For_testing.echo_file_patterns client in
      let%bind () = cycle_forward client in
      cycle_backward client)
  in
  [%expect
    {|
    (file_list "")
    (current_file bar.baz)
    "Cycling forward..."
    (current_file bar.baz)
    "Cycling backward..."
    (current_file bar.baz)|}];
  return ()
;;

let%expect_test "ignores redundant mlis in lists and cycling" =
  let empty_files = [ "foo.ml"; "foo_intf.ml" ] in
  let files_with_includes = [ "foo.mli", "Foo_intf.My_awesome_module" ] in
  let%bind () =
    Vcaml_test_helpers.with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () =
        setup_client ~empty_files ~files_with_includes ~entry_point:"foo.ml" ~client
      in
      let%bind () = For_testing.echo_file_patterns client in
      let%bind () = print_file_list client in
      let%bind () = print_current_file client in
      let%bind () = repeat_n_times 3 ~f:(fun () -> cycle_forward client) in
      repeat_n_times 3 ~f:(fun () -> cycle_backward client))
  in
  [%expect
    {|
    (file_list "foo.ml, foo_intf.ml")
    (current_file foo.ml)
    "Cycling forward..."
    (current_file foo_intf.ml)
    "Cycling forward..."
    (current_file foo.ml)
    "Cycling forward..."
    (current_file foo_intf.ml)
    "Cycling backward..."
    (current_file foo.ml)
    "Cycling backward..."
    (current_file foo_intf.ml)
    "Cycling backward..."
    (current_file foo.ml) |}];
  return ()
;;

let%expect_test "mlis without corresponding intf files are not treated as redundant" =
  let empty_files = [ "foo.ml" ] in
  let files_with_includes = [ "foo.mli", "Foo_intf.My_awesome_module" ] in
  let%bind () =
    Vcaml_test_helpers.with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () =
        setup_client ~empty_files ~files_with_includes ~entry_point:"foo.ml" ~client
      in
      let%bind () = For_testing.echo_file_patterns client in
      let%bind () = print_file_list client in
      let%bind () = print_current_file client in
      let%bind () = repeat_n_times 3 ~f:(fun () -> cycle_forward client) in
      repeat_n_times 3 ~f:(fun () -> cycle_backward client))
  in
  [%expect
    {|
    (file_list "foo.ml, foo.mli")
    (current_file foo.ml)
    "Cycling forward..."
    (current_file foo.mli)
    "Cycling forward..."
    (current_file foo.ml)
    "Cycling forward..."
    (current_file foo.mli)
    "Cycling backward..."
    (current_file foo.ml)
    "Cycling backward..."
    (current_file foo.mli)
    "Cycling backward..."
    (current_file foo.ml) |}];
  return ()
;;

let%expect_test "still lists files when on a redundant mli" =
  let empty_files = [ "foo.ml"; "foo_intf.ml" ] in
  let files_with_includes = [ "foo.mli", "Foo_intf.My_awesome_module" ] in
  let%bind () =
    Vcaml_test_helpers.with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () =
        setup_client ~empty_files ~files_with_includes ~entry_point:"foo.mli" ~client
      in
      let%bind () = For_testing.echo_file_patterns client in
      print_file_list client)
  in
  [%expect {| (file_list "foo.ml, foo_intf.ml")|}];
  return ()
;;

let%expect_test "cycling forward from a redundant mli puts us in the intf we included" =
  let empty_files = [ "foo.ml"; "foo_intf.ml" ] in
  let files_with_includes = [ "foo.mli", "Foo_intf.My_awesome_module" ] in
  let%bind () =
    Vcaml_test_helpers.with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () =
        setup_client ~empty_files ~files_with_includes ~entry_point:"foo.mli" ~client
      in
      let%bind () = print_current_file client in
      cycle_forward client)
  in
  [%expect
    {|
    (current_file foo.mli)
    "Cycling forward..."
    (current_file foo_intf.ml) |}];
  return ()
;;

let%expect_test "cycling backward from a redundant mli puts us in the intf we included" =
  let empty_files = [ "foo.ml"; "foo_intf.ml" ] in
  let files_with_includes = [ "foo.mli", "Foo_intf.My_awesome_module" ] in
  let%bind () =
    Vcaml_test_helpers.with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () =
        setup_client ~empty_files ~files_with_includes ~entry_point:"foo.mli" ~client
      in
      let%bind () = print_current_file client in
      cycle_backward client)
  in
  [%expect
    {|
    (current_file foo.mli)
    "Cycling backward..."
    (current_file foo_intf.ml) |}];
  return ()
;;

let%expect_test "cycling forward within numbered files switches between them" =
  let empty_files = [ "foo1.ml"; "foo1.mli"; "foo2.ml"; "foo2.mli" ] in
  let%bind () =
    Vcaml_test_helpers.with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () =
        setup_client ~empty_files ~files_with_includes:[] ~entry_point:"foo1.ml" ~client
      in
      let%bind () = print_current_file client in
      repeat_n_times 4 ~f:(fun () -> cycle_forward client))
  in
  [%expect
    {|
    (current_file foo1.ml)
    "Cycling forward..."
    (current_file foo1.mli)
    "Cycling forward..."
    (current_file foo2.ml)
    "Cycling forward..."
    (current_file foo2.mli)
    "Cycling forward..."
    (current_file foo1.ml) |}];
  return ()
;;

let%expect_test "files numbered with leading zeroes are treated as their own group (and \
                 don't cycle to 0{n+1}.ml)"
  =
  let empty_files = [ "foo_01.ml"; "foo_01.mli"; "foo_02.ml"; "foo_02.mli" ] in
  let%bind () =
    Vcaml_test_helpers.with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () =
        setup_client ~empty_files ~files_with_includes:[] ~entry_point:"foo_01.ml" ~client
      in
      let%bind () = print_current_file client in
      repeat_n_times 4 ~f:(fun () -> cycle_forward client))
  in
  [%expect
    {|
    (current_file foo_01.ml)
    "Cycling forward..."
    (current_file foo_01.mli)
    "Cycling forward..."
    (current_file foo_01.ml)
    "Cycling forward..."
    (current_file foo_01.mli)
    "Cycling forward..."
    (current_file foo_01.ml) |}];
  return ()
;;

let%expect_test "listing files in fzf attempts a call to MliCyclerFzf" =
  let%bind result =
    try_with
      ~run:`Schedule
      ~rest:`Log
      (fun () ->
         Vcaml_test_helpers.with_client (fun client ->
           let%bind.Deferred.Or_error () =
             setup_client
               ~empty_files:[ "foo.ml"; "foo.mli" ]
               ~files_with_includes:[]
               ~entry_point:"foo.ml"
               ~client
           in
           For_testing.list_file_patterns_in_fzf client))
  in
  Result.iter_error result ~f:(fun exn_ -> print_s [%message (exn_ : exn)]);
  (* The regular (fzf) version of list cannot be tested directly, as the headless Neovim
     does not have access to fzf or the vimscript where [MliCyclerFzf] is defined.
     Instead, we verify that the function which echoes the file list to the command line
     is echoing the right list of files and that this call to [list_fzf] fails by calling
     the [MliCyclerFzf] function. *)
  [%expect
    {|
      (exn_
       (monitor.ml.Error
        (("Called from" app/vim/mli-cycler/src/plugin.ml:LINE:COL)
         ("Vim returned error" "Vim:E117: Unknown function: MliCyclerFzf"
          (error_type Exception)))
        ("<backtrace elided in test>" "Caught by monitor Monitor.protect"))) |}];
  return ()
;;
