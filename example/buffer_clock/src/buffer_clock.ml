open Core_kernel
open Async
open Vcaml

(* Simple Vcaml plugin to create a new buffer/window which will display the current time
   and update every second. The plugin is killed upon buffer deletion.
   Source buffer_clock.vim to use it. *)

let win_update_api_call ~new_win ~buf =
  let%map.Api_call set_height_or_err = Window.set_height ~window:new_win ~height:1
  (* Executing multiple api calls separately is not atomic, so we should
     restore focus our new window before setting the buffer in case there's
     a race condition that arises. *)
  and set_win_or_err = Client.set_current_win ~window:new_win
  and set_buf_or_err = Client.set_current_buf ~buffer:buf in
  Or_error.all_unit [ set_height_or_err; set_win_or_err; set_buf_or_err ]
;;

let wins_and_bufs_api_call =
  let%map.Api_call buf_or_err = Buf.find_by_name_or_create ~name:"Vcaml_buffer_clock"
  (* We want to preserve the original window so that we can restore focus
     after we create the clock window *)
  and orig_win_or_err = Client.get_current_win
  and split_or_err = Client.command ~command:"split"
  and new_win_or_err = Client.get_current_win in
  let open Or_error.Let_syntax in
  let%bind buf = buf_or_err in
  let%bind orig_win = orig_win_or_err in
  let%bind () = split_or_err in
  let%bind new_win = new_win_or_err in
  return (buf, orig_win, new_win)
;;

let attach_listener_for_buffer_close ~client ~terminate_var =
  let open Deferred.Or_error.Let_syntax in
  let%bind pipe_reader = Buf.attach client ~buffer:`Current ~send_buffer:false in
  Async.don't_wait_for
  @@ Pipe.iter pipe_reader ~f:(fun event ->
    match event with
    | Lines _ | Changed_tick _ -> Deferred.return ()
    | Detach _ -> Deferred.return (Ivar.fill terminate_var ()));
  return ()
;;

let update_buffer_with_current_time ~client ~buf ~time_source () =
  let%bind res_or_err =
    Vcaml.run_join
      client
      (Buf.set_lines
         ~buffer:buf
         ~start:0
         ~end_:1
         ~strict_indexing:false
         ~replacement:[ Time_ns.to_string (Time_source.now time_source) ])
  in
  let%map () = Time_source.after time_source (Time_ns.Span.of_int_ms 1000) in
  match res_or_err with
  | Ok () -> `Repeat ()
  | Error err -> `Finished err
;;

let start_plugin ~client ~terminate_var =
  let open Deferred.Or_error.Let_syntax in
  let%bind buf, orig_win, new_win = Vcaml.run_join client wins_and_bufs_api_call in
  let%bind () = Vcaml.run_join client (win_update_api_call ~new_win ~buf) in
  let%bind () = attach_listener_for_buffer_close ~client ~terminate_var in
  (* Restore the user to their original window *)
  let%bind () = Vcaml.run_join client (Client.set_current_win ~window:orig_win) in
  return (buf, new_win)
;;

let start_buffer_updates ~client ~buf ~time_source =
  Deferred.repeat_until_finished
    ()
    (update_buffer_with_current_time ~client ~buf ~time_source)
;;

let create_clock_buffer pipe ~terminate_var ~time_source () =
  let open Deferred.Or_error.Let_syntax in
  let%bind client, _process = Client.attach (Unix pipe) in
  let%bind buf, _new_win = start_plugin ~client ~terminate_var in
  let writing_buf_err = start_buffer_updates ~client ~buf ~time_source in
  let shutdown_received = Ivar.read terminate_var in
  Deferred.ok (Deferred.any_unit [ Deferred.ignore_m writing_buf_err; shutdown_received ])
;;

let main =
  Command.async_or_error
    ~summary:
      "start a Vcaml process which opens a new buffer and window with a clock in the \
       current neovim instance (shuts down on clock buffer deletion)"
    (let%map_open.Command () = return () in
     let terminate_var = Ivar.create () in
     let time_source = Time_source.wall_clock () in
     let pipe = Sys.getenv_exn "NVIM_LISTEN_ADDRESS" in
     create_clock_buffer pipe ~terminate_var ~time_source)
;;

module For_testing = struct
  let run_plugin_for_testing ~client ~terminate_var ~time_source =
    let open Deferred.Or_error.Let_syntax in
    let%bind buf, new_win = start_plugin ~client ~terminate_var in
    let (_ : Error.t Deferred.t) = start_buffer_updates ~client ~buf ~time_source in
    return (buf, new_win)
  ;;
end
