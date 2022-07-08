open! Core
open! Async
open! Vcaml
open! Vcaml_buffer_clock
open Deferred.Or_error.Let_syntax
module Time_ns = Time_ns_unix

let kill_buffer_in_window ~window =
  let%map.Api_call set_win_or_err = Nvim.set_current_win window
  and delete_buf_or_err = Nvim.command "bd!" in
  Or_error.all_unit [ set_win_or_err; delete_buf_or_err ]
;;

let check_window_count ~client =
  let%map win_list = Nvim.list_wins |> run_join [%here] client in
  List.length win_list
;;

let print_buffer_contents ~client ~buffer =
  let%map contents =
    Buffer.get_lines (Id buffer) ~start:0 ~end_:(-1) ~strict_indexing:false
    |> run_join [%here] client
  in
  print_s [%message (contents : string list)]
;;

let print_window_count client =
  let%map.Deferred.Or_error current_window_count = check_window_count ~client in
  print_s [%message (current_window_count : int)]
;;

let%expect_test "plugin opens a new buffer/window which updates until buffer deletion" =
  let%map.Deferred () =
    Vcaml_test.with_client (fun client ->
      let now_str = "2020-01-01 00:00:00.000000000Z" in
      let time_source = Time_source.create ~now:(Time_ns.of_string now_str) () in
      let (module Plugin) =
        Buffer_clock.For_testing.create_plugin
          ~time_source:(Time_source.read_only time_source)
      in
      let%bind () = print_window_count client in
      let%bind { plugin_state = { window; buffer }; shutdown = _; wait_for_shutdown } =
        Plugin.start ~client
      in
      let window = Set_once.get_exn window [%here] in
      let buffer = Set_once.get_exn buffer [%here] in
      let%bind () = print_window_count client in
      let%bind () = print_buffer_contents ~client ~buffer in
      let%bind () =
        Deferred.ok
          (Time_source.advance_by_alarms_by time_source (Time_ns.Span.of_int_ms 1000))
      in
      let%bind () = print_buffer_contents ~client ~buffer in
      let%bind () = kill_buffer_in_window ~window |> run_join [%here] client in
      wait_for_shutdown)
  in
  [%expect
    {|
    (current_window_count 1)
    (current_window_count 2)
    (contents ("2020-01-01 00:00:00.000000000Z"))
    (contents ("2020-01-01 00:00:01.000000000Z"))|}]
;;
