open Core_kernel
open Async
open Vcaml

module State = struct
  type t =
    { window : Window.t
    ; buffer : Buf.t
    }
end

(* Simple Vcaml plugin to create a new buffer/window which will display the current time
   and update every second. The plugin is killed upon buffer deletion.
   Source buffer_clock.vim to use it. *)

let win_update_api_call ~new_win ~buffer =
  let%map.Api_call set_height_or_err = Window.set_height ~window:new_win ~height:1
  (* Executing multiple api calls separately is not atomic, so we should
     restore focus our new window before setting the buffer in case there's
     a race condition that arises. *)
  and set_win_or_err = Client.set_current_win ~window:new_win
  and set_buf_or_err = Client.set_current_buf ~buffer in
  Or_error.all_unit [ set_height_or_err; set_win_or_err; set_buf_or_err ]
;;

let wins_api_call =
  (* We want to preserve the original window so that we can restore focus
     after we create the clock window *)
  let%map.Api_call orig_win_or_err = Client.get_current_win
  and split_or_err = Client.command ~command:"split"
  and new_win_or_err = Client.get_current_win in
  let open Or_error.Let_syntax in
  let%bind orig_win = orig_win_or_err in
  let%bind () = split_or_err in
  let%bind new_win = new_win_or_err in
  return (orig_win, new_win)
;;

let update_buffer_with_current_time ~client ~buffer ~time_source () =
  let%bind res_or_err =
    Buf.set_lines
      ~buffer
      ~start:0
      ~end_:1
      ~strict_indexing:false
      ~replacement:[ Time_ns.to_string (Time_source.now time_source) ]
    |> run_join client
  in
  let%map () = Time_source.after time_source (Time_ns.Span.of_int_ms 1000) in
  match res_or_err with
  | Ok () -> `Repeat ()
  | Error err -> `Finished err
;;

let start_buffer_updates ~time_source ~client ~buffer ~shutdown =
  let%map clock_error =
    Deferred.repeat_until_finished
      ()
      (update_buffer_with_current_time ~client ~buffer ~time_source)
  in
  print_s [%message (clock_error : Error.t)];
  shutdown ()
;;

let start_plugin ~time_source ~client ~buffer ~shutdown =
  let open Deferred.Or_error.Let_syntax in
  let%bind orig_win, new_win = run_join client wins_api_call in
  let%bind () = win_update_api_call ~new_win ~buffer |> run_join client in
  (* Restore the user to their original window *)
  let%bind () = Client.set_current_win ~window:orig_win |> run_join client in
  Async.don't_wait_for (start_buffer_updates ~time_source ~client ~buffer ~shutdown);
  return new_win
;;

let ignore_buffer_event _state _client _event = Deferred.Or_error.return ()
let on_buffer_close ~shutdown _state _client = Deferred.Or_error.return (shutdown ())

module type Time_source_arg = sig
  val time_source : Time_source.t
end

module Make_buffer_clock (T : Time_source_arg) = Vcaml_plugin.Persistent.Make (struct
    type state = State.t

    let rpc_handlers = []

    let startup (client, shutdown) =
      let open Deferred.Or_error.Let_syntax in
      let%bind buffer =
        Buf.find_by_name_or_create ~name:"Vcaml_buffer_clock" |> run_join client
      in
      let%bind win = start_plugin ~time_source:T.time_source ~client ~buffer ~shutdown in
      let%map () =
        Vcaml_plugin.setup_buffer_events
          ~client
          ~buffer
          ~state:win
          ~on_buffer_event:ignore_buffer_event
          ~on_buffer_close:(on_buffer_close ~shutdown)
      in
      { State.window = win; buffer }
    ;;

    let vimscript_notify_fn = None
    let on_shutdown = Fn.const (Deferred.Or_error.return ())
  end)

module Buffer_clock_plugin = Make_buffer_clock (struct
    let time_source = Time_source.wall_clock ()
  end)

let main =
  Buffer_clock_plugin.command
    ~summary:
      "start a Vcaml process which opens a new buffer and window with a clock in the \
       current neovim instance (shuts down on clock buffer deletion)"
    ()
;;

module For_testing = struct
  let run ~time_source =
    let module Test_plugin =
      Make_buffer_clock (struct
        let time_source = time_source
      end)
    in
    Test_plugin.run_for_testing
  ;;
end
