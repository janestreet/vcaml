open! Core
open! Async
open Vcaml
open Vcaml_simple_editor
open Deferred.Or_error.Let_syntax

let print_window_count ~client =
  let%map wins = Nvim.list_wins |> run_join [%here] client in
  let num_wins = List.length wins in
  print_s [%message (num_wins : int)]
;;

(* Keys between < and > count as one keypress, despite being represented as
   multiple characters in the orignal string, this function only counts the contents
   between < and > as a single character, instead of potentially many of them. *)
let expected_num_jobs ~keys =
  String.split_on_chars ~on:[ '<'; '>' ] keys
  |> List.mapi ~f:(fun i x -> if i mod 2 = 0 then String.length x else 1)
  |> List.sum (module Int) ~f:Fn.id
;;

(* Because of a bug in neovim (https://github.com/neovim/neovim/issues/12722), we cannot
   use synchronous rpc requests to kick off async work, and hence must wait for the keys
   to finish sending to the plugin before waiting for prior jobs to be done. We do this
   by waiting for the correct number of jobs to be enqueued in the sequencer. *)
let wait_for_jobs ~sequencer ~num_jobs =
  let rec loop_until sequencer num_jobs =
    if Throttle.num_jobs_waiting_to_start sequencer = num_jobs
    then Deferred.unit
    else (
      let%bind.Deferred () = Scheduler.yield () in
      loop_until sequencer num_jobs)
  in
  Throttle.enqueue sequencer (fun () -> loop_until sequencer num_jobs)
;;

let escape_and_feedkeys ~client ~keys ~sequencer =
  let%bind escaped_keys =
    Nvim.replace_termcodes ~str:keys ~replace_keycodes:true |> run_join [%here] client
  in
  let wait_on_completion = wait_for_jobs ~sequencer ~num_jobs:(expected_num_jobs ~keys) in
  let%bind () =
    Nvim.feedkeys ~keys:escaped_keys ~mode:"t" ~escape_csi:true |> run_join [%here] client
  in
  let%bind () = Deferred.ok wait_on_completion in
  Deferred.ok (Async.Throttle.prior_jobs_done sequencer)
;;

let get_contents ~client ~buffer =
  Buffer.get_lines ~buffer ~start:0 ~end_:(-1) ~strict_indexing:true
  |> run_join [%here] client
;;

let kill_plugin ~client = Nvim.command ~command:"q!" |> run_join [%here] client

module Alphabet_test = struct
  let before_plugin ~client = print_window_count ~client

  let during_plugin ~sequencer ~client ~chan_id:_ ~state:{ Simple_editor.State.buffer; _ }
    =
    let key_sequence =
      "the quick brown fox jumps over the lazy dog<CR>THE QUICK BROWN FOX JUMPS OVER THE \
       LAZY DOG "
    in
    let%bind () = print_window_count ~client in
    let%bind () = escape_and_feedkeys ~client ~keys:key_sequence ~sequencer in
    let%bind contents = get_contents ~client ~buffer in
    print_s [%message (contents : string list)];
    kill_plugin ~client
  ;;
end

let%expect_test "splits open a new window and allows the user to send keys" =
  let sequencer = Async.Sequencer.create () in
  let (module Plugin) = Simple_editor.For_testing.create_plugin ~sequencer in
  let%bind.Deferred () =
    Vcaml_test.with_client (fun client ->
      let%bind () = Alphabet_test.before_plugin ~client in
      let%bind _state =
        Plugin.run_for_testing
          ~during_plugin:(Alphabet_test.during_plugin ~sequencer ~client)
          client
      in
      return ())
  in
  [%expect
    {|
  (num_wins 1)
  (num_wins 2)
  (contents
   ("the quick brown fox jumps over the lazy dog"
    "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG ")) |}];
  Deferred.return ()
;;

module _ = struct
  let during_plugin
        ~sequencer
        ~client
        ~chan_id:_
        ~state:{ Simple_editor.State.buffer; window }
    =
    let key_sequence = "<BS>this<BS><CR><BS> a drink with jam and bread" in
    let%bind () = escape_and_feedkeys ~client ~keys:key_sequence ~sequencer in
    let%bind () =
      Window.set_cursor ~window { row = 1; col = 2 } |> run_join [%here] client
    in
    let%bind () = escape_and_feedkeys ~client ~keys:"<BS>" ~sequencer in
    let%bind contents = get_contents ~client ~buffer in
    print_s [%message (contents : string list)];
    kill_plugin ~client
  ;;

  let%expect_test "backspace" =
    let sequencer = Async.Sequencer.create () in
    let (module Plugin) = Simple_editor.For_testing.create_plugin ~sequencer in
    let%bind.Deferred _state =
      Vcaml_test.with_client (fun client ->
        Plugin.run_for_testing ~during_plugin:(during_plugin ~client ~sequencer) client)
    in
    [%expect {| (contents ("ti a drink with jam and bread")) |}];
    Deferred.return ()
  ;;
end

module _ = struct
  let during_plugin
        ~sequencer
        ~client
        ~chan_id:_
        ~state:{ Simple_editor.State.buffer; window }
    =
    let key_sequence = "<CR>second linethird line<CR>fourth line" in
    let%bind () = escape_and_feedkeys ~client ~keys:key_sequence ~sequencer in
    let%bind () =
      Window.set_cursor ~window { row = 2; col = 11 } |> run_join [%here] client
    in
    let%bind () = escape_and_feedkeys ~client ~keys:"<CR>" ~sequencer in
    let%bind contents = get_contents ~client ~buffer in
    print_s [%message (contents : string list)];
    kill_plugin ~client
  ;;

  let%expect_test "enter" =
    let sequencer = Async.Sequencer.create () in
    let (module Plugin) = Simple_editor.For_testing.create_plugin ~sequencer in
    let%bind.Deferred _state =
      Vcaml_test.with_client (fun client ->
        Plugin.run_for_testing ~during_plugin:(during_plugin ~client ~sequencer) client)
    in
    [%expect {| (contents ("" "second line" "third line" "fourth line")) |}];
    Deferred.return ()
  ;;
end
