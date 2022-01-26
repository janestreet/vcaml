open Core
open Async
open Vcaml

module State = struct
  type t =
    { window : Window.t Set_once.t
    ; buffer : Buffer.t Set_once.t
    }
  [@@deriving sexp_of]
end

(* Simple Vcaml plugin to create a new buffer/window which will display the current time
   and update every second. The plugin is killed upon buffer deletion. Source
   buffer_clock.vim to use it. *)

let win_update_api_call ~new_win ~buffer =
  let%map.Api_call set_height_or_err = Window.set_height ~window:new_win ~height:1
  (* Executing multiple api calls separately is not atomic, so we should
     restore focus our new window before setting the buffer in case there's
     a race condition that arises. *)
  and set_win_or_err = Nvim.set_current_win ~window:new_win
  and set_buf_or_err = Nvim.set_current_buf ~buffer in
  Or_error.all_unit [ set_height_or_err; set_win_or_err; set_buf_or_err ]
;;

let wins_api_call =
  (* We want to preserve the original window so that we can restore focus
     after we create the clock window *)
  let%map.Api_call orig_win_or_err = Nvim.get_current_win
  and split_or_err = Nvim.command ~command:"split"
  and new_win_or_err = Nvim.get_current_win in
  let open Or_error.Let_syntax in
  let%bind orig_win = orig_win_or_err in
  let%bind () = split_or_err in
  let%bind new_win = new_win_or_err in
  return (orig_win, new_win)
;;

let update_buffer_with_current_time ~client ~buffer ~time_source () =
  let%bind res_or_err =
    Buffer.set_lines
      ~buffer
      ~start:0
      ~end_:1
      ~strict_indexing:true
      ~replacement:[ Time_ns.to_string_utc (Time_source.now time_source) ]
    |> run_join [%here] client
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
  let%bind orig_win, new_win = run_join [%here] client wins_api_call in
  let%bind () = win_update_api_call ~new_win ~buffer |> run_join [%here] client in
  (* Restore the user to their original window *)
  let%bind () = Nvim.set_current_win ~window:orig_win |> run_join [%here] client in
  Async.don't_wait_for (start_buffer_updates ~time_source ~client ~buffer ~shutdown);
  return new_win
;;

module type S = Vcaml_plugin.Persistent.S with type state := State.t

let create_plugin ~time_source =
  let module Arg = struct
    let name = "buffer-clock"

    type state = State.t [@@deriving sexp_of]

    let description =
      "Start a VCaml process that opens a new buffer and window with a clock in the \
       current Neovim instance (shuts down on clock buffer deletion)."
    ;;

    let rpc_handlers = []
    let init_state () = { State.window = Set_once.create (); buffer = Set_once.create () }

    let on_startup client state ~shutdown =
      let open Deferred.Or_error.Let_syntax in
      let subscriber = Buffer.Subscriber.create client ~on_parse_error:`Raise in
      let%bind buffer =
        Buffer.find_by_name_or_create ~name:"Vcaml_buffer_clock"
        |> run_join [%here] client
      in
      let%bind window = start_plugin ~time_source ~client ~buffer ~shutdown in
      Set_once.set_exn state.State.buffer [%here] buffer;
      Set_once.set_exn state.State.window [%here] window;
      let%bind events =
        Buffer.Subscriber.subscribe
          subscriber
          [%here]
          ~buffer:(`Numbered buffer)
          ~send_buffer:true
      in
      don't_wait_for
        (Pipe.iter_without_pushback events ~f:(function
           | Lines _ | Changed_tick _ -> ()
           | Detach _ -> shutdown ()));
      return ()
    ;;

    let on_error = `Raise
    let vimscript_notify_fn = None
    let on_shutdown _ _ = Deferred.Or_error.return ()
  end
  in
  (module Vcaml_plugin.Persistent.Make (Arg) : S)
;;

let main =
  let (module Main) = create_plugin ~time_source:(Time_source.wall_clock ()) in
  Main.command
;;

module For_testing = struct
  module type S = Vcaml_plugin.Persistent.For_testing.S with type plugin_state := State.t

  let create_plugin ~time_source =
    let (module Plugin) = create_plugin ~time_source in
    (module Plugin.For_testing : S)
  ;;
end
