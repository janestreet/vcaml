open! Core_kernel
open! Async
open! Import
open Vcaml
open Test_client

let%expect_test "open neovim and get channel list" =
  let%bind () =
    simple Client.list_chans (fun channels -> channels |> List.length > 0 |> sexp_of_bool)
  in
  [%expect "true"];
  return ()
;;

let%expect_test "get_chan_info" =
  let%bind () =
    simple (Client.get_chan_info ~chan:1) ("call-succeeded" |> Sexp.Atom |> Fn.const)
  in
  [%expect "call-succeeded"];
  return ()
;;

let%expect_test "command output" =
  let%bind () = simple (Client.command_output ~command:"echo 'hi'") sexp_of_string in
  [%expect "hi"];
  return ()
;;

let%expect_test "command, list_bufs, Buf.get_name" =
  let%bind () =
    with_client ~f:(fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = run_join client (Client.command ~command:"e foo.txt") in
      let%bind () = run_join client (Client.command ~command:"e bar.txt") in
      let%bind () = run_join client (Client.command ~command:"e baz.txt") in
      let%bind buffers = run_join client Client.list_bufs in
      let%map buffer_names =
        buffers
        |> List.map ~f:(fun buffer -> run_join client (Vcaml.Buf.get_name ~buffer))
        |> Deferred.Or_error.combine_errors
        |> Deferred.Or_error.map ~f:(fun filenames ->
          List.map filenames ~f:(fun file ->
            file |> Filename.parts |> List.last_exn))
      in
      print_s [%message (buffers : Vcaml.Buf.t list) (buffer_names : string list)])
  in
  [%expect
    {|
    ((buffers ((Buffer (Integer 1)) (Buffer (Integer 2)) (Buffer (Integer 3))))
     (buffer_names (foo.txt bar.txt baz.txt)))|}];
  return ()
;;

let%expect_test "eval" =
  let%bind () = simple (Client.eval ~expr:"1 + 2") [%sexp_of: Msgpack.t] in
  [%expect {| (Integer 3) |}];
  return ()
;;

let%expect_test "neovim environment" =
  let sanitize string =
    string
    |> String.substr_replace_all ~pattern:"\\n" ~with_:""
    |> String.substr_replace_all ~pattern:"\\r" ~with_:""
    |> String.split_lines
    |> List.map ~f:(String.split_on_chars ~on:[ '=' ])
    |> List.filter_map ~f:List.hd
    |> List.filter ~f:(String.exists ~f:Char.is_uppercase)
    |> List.sort ~compare:compare_string
    |> List.map ~f:(fun a -> Sexp.Atom a)
    |> Sexp.List
  in
  let%bind () = simple (Client.command_output ~command:"!env") sanitize in
  [%expect {|(NVIM_LISTEN_ADDRESS NVIM_LOG_FILE PWD SHLVL VIMRUNTIME)|}];
  return ()
;;

let%expect_test "set_current_buf" =
  let%bind () =
    with_client ~f:(fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = run_join client (Client.command ~command:"e foo.txt") in
      let%bind expected_buf = run_join client Client.get_current_buf in
      let%bind () = run_join client (Client.command ~command:"e bar.txt") in
      let%bind () = run_join client (Client.set_current_buf ~buffer:expected_buf) in
      let%bind actual_buf = run_join client Client.get_current_buf in
      print_s [%message (expected_buf : Vcaml.Buf.t) (actual_buf : Vcaml.Buf.t)];
      return ())
  in
  [%expect "((expected_buf (Buffer (Integer 1))) (actual_buf (Buffer (Integer 1))))"];
  return ()
;;

let%expect_test "call_function" =
  let%bind () =
    with_client ~f:(fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind strlen =
        run_join client (Client.call_function ~fn:"strlen" ~args:[ String "foo" ])
      in
      print_s [%message (strlen : Msgpack.t)];
      return ())
  in
  [%expect "(strlen (Integer 3))"];
  return ()
;;

let get_current_chan ~client =
  let%map.Deferred.Or_error chan_list = run_join client Client.list_chans in
  (List.hd_exn chan_list).client
;;

let%expect_test "set_client_info" =
  let test_method =
    { Client_info.Client_method.async = false
    ; nargs = Some (`Fixed 1)
    ; opts = String.Map.empty
    }
  in
  let%bind () =
    with_client ~f:(fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind client_before_setting_info = get_current_chan ~client in
      let%bind () =
        run_join
          client
          (Client.set_client_info
             ~version:
               { major = Some 1
               ; minor = Some 2
               ; patch = Some 3
               ; prerelease = Some "test_prerelease"
               ; commit = Some "test_commit"
               }
             ~methods:(String.Map.of_alist_exn [ "test_method", test_method ])
             ~attributes:(String.Map.of_alist_exn [ "attr1", "val1" ])
             ~name:"foo"
             ~type_:`Embedder
             ())
      in
      let%bind client_after_setting_info = get_current_chan ~client in
      print_s
        [%message
          (client_before_setting_info : Client_info.t option)
            (client_after_setting_info : Client_info.t option)];
      return ())
  in
  [%expect
    {|
    ((client_before_setting_info
      (((version ()) (methods ()) (attributes ()) (name ()) (type_ ()))))
     (client_after_setting_info
      (((version
         (((major (1)) (minor (2)) (patch (3)) (prerelease (test_prerelease))
           (commit (test_commit)))))
        (methods ((test_method ((async false) (nargs ((Fixed 1))) (opts ())))))
        (attributes ((attr1 val1))) (name (foo)) (type_ (Embedder))))))|}];
  return ()
;;

let%expect_test "get_current_win, set_current_win" =
  let%bind () =
    with_client ~f:(fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind original_win = run_join client Client.get_current_win in
      let%bind () = run_join client (Client.command ~command:"split") in
      let%bind win_after_split = run_join client Client.get_current_win in
      let%bind () = run_join client (Client.set_current_win ~window:original_win) in
      let%bind win_after_set = run_join client Client.get_current_win in
      print_s
        [%message
          (original_win : Vcaml.Window.t)
            (win_after_split : Vcaml.Window.t)
            (win_after_set : Vcaml.Window.t)];
      return ())
  in
  [%expect "((original_win 1000) (win_after_split 1001) (win_after_set 1000))"];
  return ()
;;

let%expect_test "list_wins" =
  let%bind () =
    with_client ~f:(fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = run_join client (Client.command ~command:"split") in
      let%bind () = run_join client (Client.command ~command:"split") in
      let%bind win_list = run_join client Client.list_wins in
      print_s [%message (win_list : Window.t list)];
      return ())
  in
  [%expect "(win_list (1002 1001 1000))"];
  return ()
;;
