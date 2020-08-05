open! Core
open! Async
open! Vcaml
open! Vcaml_buffer_clock

let check_window_count ~client =
  let%map.Deferred.Or_error win_list = Vcaml.run_join client Client.list_wins in
  List.length win_list
;;

let get_buffer_contents ~client ~buf =
  Vcaml.run_join
    client
    (Buf.get_lines ~buffer:buf ~start:0 ~end_:(-1) ~strict_indexing:false)
;;

let kill_buffer_api_call ~window =
  let%map.Api_call set_win_or_err = Client.set_current_win ~window
  and delete_buf_or_err = Client.command ~command:"bd!" in
  Or_error.all_unit [ set_win_or_err; delete_buf_or_err ]
;;

let kill_buffer_in_window ~client ~window =
  Vcaml.run_join client (kill_buffer_api_call ~window)
;;

let%expect_test "plugin opens a new buffer/window which updates until buffer deletion" =
  let now_str = "2020-01-01 00:00:00.000000000Z" in
  let mock_time_source = Time_source.create ~now:(Time_ns.of_string now_str) () in
  let mock_time_source_readonly = Time_source.read_only mock_time_source in
  let terminate_var = Ivar.create () in
  let%bind () =
    Vcaml_test.Test_client.with_client ~f:(fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind window_count_before_plugin = check_window_count ~client in
      print_s [%message (window_count_before_plugin : int)];
      let%bind buf, new_win =
        Buffer_clock.For_testing.run_plugin_for_testing
          ~client
          ~terminate_var
          ~time_source:mock_time_source_readonly
      in
      let%bind window_count_after_plugin = check_window_count ~client in
      print_s [%message (window_count_after_plugin : int)];
      let%bind buffer_contents_now = get_buffer_contents ~client ~buf in
      print_s [%message (buffer_contents_now : string list)];
      let%bind () =
        Deferred.ok
          (Time_source.advance_by_alarms_by
             mock_time_source
             (Time_ns.Span.of_int_ms 1000))
      in
      let%bind buffer_contents_after_update = get_buffer_contents ~client ~buf in
      print_s [%message (buffer_contents_after_update : string list)];
      print_s [%message (terminate_var : unit Ivar.t)];
      let%bind () = kill_buffer_in_window ~client ~window:new_win in
      print_s [%message (terminate_var : unit Ivar.t)];
      return ())
  in
  [%expect
    {|
  (window_count_before_plugin 1)
  (window_count_after_plugin 2)
  (buffer_contents_now ("2020-01-01 00:00:00.000000000Z"))
  (buffer_contents_after_update ("2020-01-01 00:00:01.000000000Z"))
  (terminate_var Empty)
  (terminate_var (Full ()))|}]
;;
