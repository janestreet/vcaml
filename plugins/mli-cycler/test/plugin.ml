open! Core
open! Async
open! Vcaml_test
open! Vcaml
open! Mli_plugin

let setup_test ~empty_files ~files_with_includes ~temp_dir =
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
  let%bind name = Vcaml.run_join client (Buf.get_name ~buffer:buf) in
  let _dir, current_file = Core.Filename.split name in
  print_s [%message (current_file : string)];
  return ()
;;

let print_list_of_files client =
  let open Deferred.Or_error.Let_syntax in
  let%bind () = Plugin.For_testing.list_raw client in
  let%bind file_list =
    Vcaml.run_join client (Client.command_output ~command:"1 message")
  in
  print_s [%message (file_list : string)];
  return ()
;;

let run_test ~empty_files ~files_with_includes ~entry_point ~f =
  Test_client.with_client ~f:(fun client ->
    let open Deferred.Or_error.Let_syntax in
    let temp_dir = Core.Filename.temp_dir "mli_plugin" "test" in
    let%bind () = setup_test ~empty_files ~files_with_includes ~temp_dir in
    let entry_file_full_path = Core.( ^/ ) temp_dir entry_point in
    let%bind () =
      Vcaml.run_join client (Client.command ~command:(":e " ^ entry_file_full_path))
    in
    let%bind () = f client in
    return ())
;;

let rec repeat_n_times times ~f =
  match times with
  | 0 -> Deferred.Or_error.return ()
  | n ->
    let%bind.Deferred.Or_error () = f () in
    repeat_n_times (n - 1) ~f
;;

let cycle_backward client =
  let open Deferred.Or_error.Let_syntax in
  print_s (Sexp.Atom "Cycling backward...");
  let%bind () = Plugin.For_testing.prev client in
  print_current_file client
;;

let cycle_forward client =
  let open Deferred.Or_error.Let_syntax in
  print_s (Sexp.Atom "Cycling forward...");
  let%bind () = Plugin.For_testing.next client in
  print_current_file client
;;

let go_back_and_forth_in_list_n_times n client =
  let open Deferred.Or_error.Let_syntax in
  let%bind () = print_current_file client in
  let%bind () = repeat_n_times n ~f:(fun () -> cycle_forward client) in
  repeat_n_times n ~f:(fun () -> cycle_backward client)
;;

let list_files_and_cycle n client =
  let open Deferred.Or_error.Let_syntax in
  let%bind () = print_list_of_files client in
  go_back_and_forth_in_list_n_times n client
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
    run_test
      ~empty_files
      ~files_with_includes:[]
      ~entry_point:"foo.ml"
      ~f:print_list_of_files
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
    run_test
      ~empty_files
      ~files_with_includes:[]
      ~entry_point:"foo.ml"
      ~f:(go_back_and_forth_in_list_n_times 6)
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
  let%bind () =
    run_test
      ~empty_files:[ "foo.ml"; "foo.mli"; "foo_intf.ml"; "bar.baz" ]
      ~files_with_includes:[]
      ~entry_point:"bar.baz"
      ~f:(list_files_and_cycle 1)
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
  let%bind () =
    run_test
      ~empty_files:[ "foo.ml"; "foo_intf.ml" ]
      ~files_with_includes:[ "foo.mli", "Foo_intf.My_awesome_module" ]
      ~entry_point:"foo.ml"
      ~f:(list_files_and_cycle 3)
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
  let%bind () =
    run_test
      ~empty_files:[ "foo.ml"; "foo_intf.ml" ]
      ~files_with_includes:[ "foo.mli", "Foo_intf.My_awesome_module" ]
      ~entry_point:"foo.mli"
      ~f:print_list_of_files
  in
  [%expect {| (file_list "foo.ml, foo_intf.ml")|}];
  return ()
;;

let%expect_test "cycling forward from a redundant mli puts us at the start of the list" =
  let%bind () =
    run_test
      ~empty_files:[ "foo.ml"; "foo_intf.ml" ]
      ~files_with_includes:[ "foo.mli", "Foo_intf.My_awesome_module" ]
      ~entry_point:"foo.mli"
      ~f:(fun client ->
        let%bind.Deferred.Or_error () = print_current_file client in
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
  let%bind () =
    run_test
      ~empty_files:[ "foo.ml"; "foo_intf.ml" ]
      ~files_with_includes:[ "foo.mli", "Foo_intf.My_awesome_module" ]
      ~entry_point:"foo.mli"
      ~f:(fun client ->
        let%bind.Deferred.Or_error () = print_current_file client in
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
  let%bind () =
    Test_client.with_client ~f:(fun client ->
      let open Deferred.Or_error.Let_syntax in
      let temp_dir = Core.Filename.temp_dir "mli_plugin" "test" in
      let%bind () =
        setup_test
          ~empty_files:[ "foo.ml"; "foo.mli" ]
          ~files_with_includes:[]
          ~temp_dir
      in
      let entry_file_full_path = Core.( ^/ ) temp_dir "foo.ml" in
      let%bind () =
        Vcaml.run_join client (Client.command ~command:(":e " ^ entry_file_full_path))
      in
      let%bind.Deferred err = Plugin.For_testing.list_fzf client in
      print_s [%message (err : unit Or_error.t)];
      return ())
  in
  (* The regular (fzf) version of list cannot be tested directly, as the headless vim does
     not have access to fzf. Instead, we verify that the function which echoes the file
     list to the command line is echoing the right list of files and that this call to
     list_fzf fails by calling the fzf#run function. *)
  [%expect
    {| (err (Error ("Vim returned error" "Vim:E117: Unknown function: fzf#run"))) |}];
  return ()
;;
