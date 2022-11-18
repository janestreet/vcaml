open Core
open Async
open Vcaml
open Vcaml_simple_editor
open Vcaml_test_helpers
open Deferred.Or_error.Let_syntax

let print_window_count ~client =
  let%map wins = Nvim.list_wins |> run_join [%here] client in
  let num_wins = List.length wins in
  print_s [%message (num_wins : int)]
;;

let escape_and_feedkeys ~client ~keys =
  let%bind escaped_keys =
    Nvim.replace_termcodes_and_keycodes keys |> run_join [%here] client
  in
  Nvim.feedkeys (`Already_escaped escaped_keys) ~mode:"mx" |> run_join [%here] client
;;

let get_contents ~client ~buffer =
  Buffer.get_lines (Id buffer) ~start:0 ~end_:(-1) ~strict_indexing:true
  |> run_join [%here] client
;;

let kill_plugin ~client = Nvim.command "q!" |> run_join [%here] client

let%expect_test "splits open a new window and allows the user to send keys" =
  let%map.Deferred () =
    with_client (fun client ->
      let%bind () = print_window_count ~client in
      let%bind { plugin_state = { buffer; window = _ }
               ; shutdown = _
               ; wait_for_shutdown
               }
        =
        Simple_editor.For_testing.start ~client
      in
      let buffer = Set_once.get_exn buffer [%here] in
      let key_sequence =
        "the quick brown fox jumps over the lazy dog<CR>THE QUICK BROWN FOX JUMPS OVER \
         THE LAZY DOG "
      in
      let%bind () = print_window_count ~client in
      let%bind () = escape_and_feedkeys ~client ~keys:key_sequence in
      let%bind contents = get_contents ~client ~buffer in
      print_s [%message (contents : string list)];
      let%bind () = kill_plugin ~client in
      wait_for_shutdown)
  in
  [%expect
    {|
  (num_wins 1)
  (num_wins 2)
  (contents
   ("the quick brown fox jumps over the lazy dog"
    "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG ")) |}]
;;

let%expect_test "backspace" =
  let%map.Deferred () =
    with_client (fun client ->
      let%bind { plugin_state = { buffer; window }; shutdown = _; wait_for_shutdown } =
        Simple_editor.For_testing.start ~client
      in
      let buffer = Set_once.get_exn buffer [%here] in
      let window = Set_once.get_exn window [%here] in
      let key_sequence = "<BS>this<BS><CR><BS> a drink with jam and bread" in
      let%bind () = escape_and_feedkeys ~client ~keys:key_sequence in
      let%bind () =
        Window.set_cursor (Id window) { row = 1; col = 2 } |> run_join [%here] client
      in
      let%bind () = escape_and_feedkeys ~client ~keys:"<BS>" in
      let%bind contents = get_contents ~client ~buffer in
      print_s [%message (contents : string list)];
      let%bind () = kill_plugin ~client in
      wait_for_shutdown)
  in
  [%expect {| (contents ("ti a drink with jam and bread")) |}]
;;

let%expect_test "enter" =
  let%map.Deferred () =
    with_client (fun client ->
      let%bind { plugin_state = { buffer; window }; shutdown = _; wait_for_shutdown } =
        Simple_editor.For_testing.start ~client
      in
      let buffer = Set_once.get_exn buffer [%here] in
      let window = Set_once.get_exn window [%here] in
      let key_sequence = "<CR>second linethird line<CR>fourth line" in
      let%bind () = escape_and_feedkeys ~client ~keys:key_sequence in
      let%bind () =
        Window.set_cursor (Id window) { row = 2; col = 11 } |> run_join [%here] client
      in
      let%bind () = escape_and_feedkeys ~client ~keys:"<CR>" in
      let%bind contents = get_contents ~client ~buffer in
      print_s [%message (contents : string list)];
      let%bind () = kill_plugin ~client in
      wait_for_shutdown)
  in
  [%expect {| (contents ("" "second line" "third line" "fourth line")) |}]
;;
