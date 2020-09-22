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
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = Client.command ~command:"e foo.txt" |> run_join client in
      let%bind () = Client.command ~command:"e bar.txt" |> run_join client in
      let%bind () = Client.command ~command:"e baz.txt" |> run_join client in
      let%bind buffers = Client.list_bufs |> run_join client in
      let%map buffer_names =
        buffers
        |> List.map ~f:(fun buffer -> Vcaml.Buf.get_name ~buffer |> run_join client)
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
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = Client.command ~command:"e foo.txt" |> run_join client in
      let%bind expected_buf = Client.get_current_buf |> run_join client in
      let%bind () = Client.command ~command:"e bar.txt" |> run_join client in
      let%bind () = Client.set_current_buf ~buffer:expected_buf |> run_join client in
      let%bind actual_buf = Client.get_current_buf |> run_join client in
      print_s [%message (expected_buf : Vcaml.Buf.t) (actual_buf : Vcaml.Buf.t)];
      return ())
  in
  [%expect "((expected_buf (Buffer (Integer 1))) (actual_buf (Buffer (Integer 1))))"];
  return ()
;;

let%expect_test "call_function" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind strlen =
        Client.call_function ~fn:"strlen" ~args:[ String "foo" ] |> run_join client
      in
      print_s [%message (strlen : Msgpack.t)];
      return ())
  in
  [%expect "(strlen (Integer 3))"];
  return ()
;;

let get_current_chan ~client =
  let%map.Deferred.Or_error chan_list = Client.list_chans |> run_join client in
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
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind client_before_setting_info = get_current_chan ~client in
      let%bind () =
        Client.set_client_info
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
          ()
        |> run_join client
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
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind original_win = Client.get_current_win |> run_join client in
      let%bind () = Client.command ~command:"split" |> run_join client in
      let%bind win_after_split = Client.get_current_win |> run_join client in
      let%bind () = Client.set_current_win ~window:original_win |> run_join client in
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
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = Client.command ~command:"split" |> run_join client in
      let%bind () = Client.command ~command:"split" |> run_join client in
      let%bind win_list = Client.list_wins |> run_join client in
      print_s [%message (win_list : Window.t list)];
      return ())
  in
  [%expect "(win_list (1002 1001 1000))"];
  return ()
;;

let%expect_test "replace_termcodes" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind escaped_keys =
        Client.replace_termcodes
          ~str:"ifoobar<ESC><Left><Left>XXX"
          ~from_part:true
          ~do_lt:false
          ~special:true
        |> run_join client
      in
      let%bind () =
        Client.feedkeys ~keys:escaped_keys ~mode:"n" ~escape_csi:true |> run_join client
      in
      let%bind buffer = Client.get_current_buf |> run_join client in
      let%bind lines =
        Vcaml.Buf.get_lines ~buffer ~start:0 ~end_:(-1) ~strict_indexing:false
        |> run_join client
      in
      print_s [%message (lines : string list)];
      return ())
  in
  [%expect {| (lines (bar)) |}];
  return ()
;;
