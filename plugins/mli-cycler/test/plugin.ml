open! Core
open! Async
open! Vcaml_test
open! Vcaml
open! Mli_plugin.Plugin

let setup_files_for_testing ~empty_files ~files_with_includes ~temp_dir =
  let create_empty file = Writer.save file ~contents:"" in
  let create_with_include file intf = Writer.save file ~contents:("include " ^ intf) in
  let%bind () =
    empty_files
    |> List.map ~f:(Core.( ^/ ) temp_dir)
    |> Deferred.List.iter ~f:create_empty
  in
  let%map () =
    files_with_includes
    |> List.map ~f:(fun (file, incl) -> Core.( ^/ ) temp_dir file, incl)
    |> Deferred.List.iter ~f:(fun (file, incl) -> create_with_include file incl)
  in
  Ok ()
;;

let print_current_file client =
  let open Deferred.Or_error.Let_syntax in
  let%bind buf = Vcaml.run_join client Client.get_current_buf in
  let%bind name = Vcaml.run_join client (Buffer.get_name ~buffer:buf) in
  let _dir, current_file = Core.Filename.split name in
  print_s [%message (current_file : string)];
  return ()
;;

let setup_client ~empty_files ~files_with_includes ~entry_point ~client =
  let open Deferred.Or_error.Let_syntax in
  let temp_dir = Core.Filename.temp_dir "mli_plugin" "test" in
  let%bind () = setup_files_for_testing ~empty_files ~files_with_includes ~temp_dir in
  let entry_file_full_path = Core.( ^/ ) temp_dir entry_point in
  Vcaml.run_join client (Client.command ~command:(":e " ^ entry_file_full_path))
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
  let%bind.Deferred.Or_error () = Prev_file_pattern.run_for_testing client in
  print_current_file client
;;

let cycle_forward client =
  print_s (Sexp.Atom "Cycling forward...");
  let%bind.Deferred.Or_error () = Next_file_pattern.run_for_testing client in
  print_current_file client
;;

let print_file_list client =
  let open Deferred.Or_error.Let_syntax in
  let%bind file_list =
    Vcaml.run_join client (Client.command_output ~command:"1 message")
  in
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
    Vcaml_plugin.For_testing.with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () =
        setup_client ~empty_files ~files_with_includes:[] ~entry_point:"foo.ml" ~client
      in
      let%bind () = Echo_file_patterns.run_for_testing client in
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
    ; "bar.ml"
    ; "bar.mli"
    ; "bar_intf.ml"
    ]
  in
  let%bind () =
    Vcaml_plugin.For_testing.with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () =
        setup_client ~empty_files ~files_with_includes:[] ~entry_point:"foo.ml" ~client
      in
      let%bind () = print_current_file client in
      let%bind () = repeat_n_times 6 ~f:(fun () -> cycle_forward client) in
      repeat_n_times 6 ~f:(fun () -> cycle_backward client))
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
    (current_file foo.ml)
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
    Vcaml_plugin.For_testing.with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () =
        setup_client ~empty_files ~files_with_includes:[] ~entry_point:"bar.baz" ~client
      in
      let%bind () = Echo_file_patterns.run_for_testing client in
      let%bind () = print_file_list client in
      let%bind () = print_current_file client in
      let%bind () = Echo_file_patterns.run_for_testing client in
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
    Vcaml_plugin.For_testing.with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () =
        setup_client ~empty_files ~files_with_includes ~entry_point:"foo.ml" ~client
      in
      let%bind () = Echo_file_patterns.run_for_testing client in
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

let%expect_test "still lists files when on a redundant mli" =
  let empty_files = [ "foo.ml"; "foo_intf.ml" ] in
  let files_with_includes = [ "foo.mli", "Foo_intf.My_awesome_module" ] in
  let%bind () =
    Vcaml_plugin.For_testing.with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () =
        setup_client ~empty_files ~files_with_includes ~entry_point:"foo.mli" ~client
      in
      let%bind () = Echo_file_patterns.run_for_testing client in
      print_file_list client)
  in
  [%expect {| (file_list "foo.ml, foo_intf.ml")|}];
  return ()
;;

let%expect_test "cycling forward from a redundant mli puts us at the start of the list" =
  let empty_files = [ "foo.ml"; "foo_intf.ml" ] in
  let files_with_includes = [ "foo.mli", "Foo_intf.My_awesome_module" ] in
  let%bind () =
    Vcaml_plugin.For_testing.with_client (fun client ->
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
    (current_file foo.ml) |}];
  return ()
;;

let%expect_test "cycling backward from a redundant mli puts us at the start of the list" =
  let empty_files = [ "foo.ml"; "foo_intf.ml" ] in
  let files_with_includes = [ "foo.mli", "Foo_intf.My_awesome_module" ] in
  let%bind () =
    Vcaml_plugin.For_testing.with_client (fun client ->
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
    (current_file foo.ml) |}];
  return ()
;;

let%expect_test "listing files in fzf attempts a call to fzf#run" =
  let%bind result =
    try_with
      ~run:
        `Schedule
      ~rest:`Log
      (fun () ->
         Vcaml_plugin.For_testing.with_client (fun client ->
           let%bind.Deferred.Or_error () =
             setup_client
               ~empty_files:[ "foo.ml"; "foo.mli" ]
               ~files_with_includes:[]
               ~entry_point:"foo.ml"
               ~client
           in
           List_file_patterns_in_fzf.run_for_testing client))
  in
  Result.iter_error result ~f:(fun exn_ -> print_s [%message (exn_ : exn)]);
  (* The regular (fzf) version of list cannot be tested directly, as the headless vim does
     not have access to fzf. Instead, we verify that the function which echoes the file
     list to the command line is echoing the right list of files and that this call to
     list_fzf fails by calling the fzf#run function. *)
  [%expect
    {|
      (exn_
       (monitor.ml.Error
        ("Vim returned error" "Vim:E117: Unknown function: fzf#run")
        ("<backtrace elided in test>"))) |}];
  return ()
;;
