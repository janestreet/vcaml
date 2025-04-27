open Core
open Async
open Vcaml

type state =
  | Prod
  | Test of { time_source : Time_source.Read_write.t }

let start_buffer_updates ~time_source ~zone ~client ~buffer =
  Time_source.run_at_intervals' time_source Time_ns.Span.second (fun () ->
    match%bind
      Buffer.set_lines
        client
        (Id buffer)
        ~start:0
        ~end_:1
        ~strict_indexing:true
        [ Time_ns.to_sec_string (Time_source.now time_source) ~zone ]
    with
    | Ok () -> return ()
    | Error _ -> exit 0)
;;

let on_startup client =
  let open Deferred.Or_error.Let_syntax in
  let state, time_source, zone =
    match Option.is_some (Unix.getenv "INSIDE_DUNE") with
    | false -> Prod, Time_source.wall_clock (), force Time_ns_unix.Zone.local
    | true ->
      let time_source = Time_source.create ~now:Time_ns.epoch () in
      Test { time_source }, Time_source.read_only time_source, Time_ns_unix.Zone.utc
  in
  let%bind buffer =
    let%bind buffer = Buffer.create client ~listed:false ~scratch:true in
    let%bind () = Buffer.Option.set client (Id buffer) Bufhidden "wipe" in
    (* We block Neovim for this sequence of commands so that they are not interleaved by
       unrelated logic. *)
    block_nvim client ~f:(fun client ->
      let%bind original_window = Nvim.get_current_win client in
      let%bind () = Command.exec client "split" in
      let%bind () = Nvim.set_current_buf client buffer in
      let%bind () = Window.set_height client Current ~height:1 in
      let%bind () = Window.Option.set client Current Winfixheight true in
      let%bind () = Nvim.set_current_win client original_window in
      return buffer)
  in
  start_buffer_updates ~time_source ~zone ~client ~buffer;
  return state
;;

(* This RPC is only used in tests to make them deterministic. It is not meant to be called
   during regular use. *)
let advance_time_for_test =
  Vcaml_plugin.Persistent.Rpc.create_async
    "advance-time"
    ~type_:Ocaml_from_nvim.Async.unit
    ~f:(fun state ~client:_ ->
      match state with
      | Prod -> Deferred.Or_error.error_string "This RPC can only be called from tests."
      | Test { time_source } ->
        Time_source.advance_by_alarms_by time_source Time_ns.Span.second |> Deferred.ok)
;;

let command =
  Vcaml_plugin.Persistent.create
    ~name:"buffer-clock"
    ~description:"Opens a window that displays a clock"
    ~on_startup
    ~notify_fn:(`Lua "buffer_clock_setup")
    [ advance_time_for_test ]
;;
