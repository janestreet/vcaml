open! Core
open! Async
open! Vcaml
open! Vcaml_buffer_clock
open Deferred.Or_error.Let_syntax
module Time_ns = Time_ns_unix

let kill_buffer_in_window ~window =
  let%map.Api_call set_win_or_err = Nvim.set_current_win ~window
  and delete_buf_or_err = Nvim.command ~command:"bd!" in
  Or_error.all_unit [ set_win_or_err; delete_buf_or_err ]
;;

let check_window_count ~client =
  let%map win_list = Nvim.list_wins |> run_join client in
  List.length win_list
;;

let print_buffer_contents ~client ~buffer =
  let%map contents =
    Buffer.get_lines ~buffer ~start:0 ~end_:(-1) ~strict_indexing:false |> run_join client
  in
  print_s [%message (contents : string list)]
;;

let print_window_count client =
  let%map current_window_count = check_window_count ~client in
  print_s [%message (current_window_count : int)]
;;

let during_plugin
      ~time_source
      ~client
      ~chan_id:_
      ~state:{ Buffer_clock.State.window; buffer }
  =
  let%bind () = print_window_count client in
  let%bind () = print_buffer_contents ~client ~buffer in
  let%bind () =
    Deferred.ok
      (Time_source.advance_by_alarms_by time_source (Time_ns.Span.of_int_ms 1000))
  in
  let%bind () = print_buffer_contents ~client ~buffer in
  kill_buffer_in_window ~window |> run_join client
;;

let%expect_test "plugin opens a new buffer/window which updates until buffer deletion" =
  let now_str = "2020-01-01 00:00:00.000000000Z" in
  let mock_time_source = Time_source.create ~now:(Time_ns.of_string now_str) () in
  let mock_time_source_readonly = Time_source.read_only mock_time_source in
  let%map.Deferred _state =
    Vcaml_plugin.For_testing.with_client (fun client ->
      let%bind () = print_window_count client in
      Buffer_clock.For_testing.run
        client
        ~time_source:mock_time_source_readonly
        ~during_plugin:(during_plugin ~time_source:mock_time_source ~client))
  in
  [%expect
    {|
    (current_window_count 1)
    (current_window_count 2)
    (contents ("2020-01-01 00:00:00.000000000Z"))
    (contents ("2020-01-01 00:00:01.000000000Z"))|}]
;;
