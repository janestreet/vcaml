open Core
open Async
module Buffer = Nvim_internal.Buffer

let nvim_buf_lines_event = "nvim_buf_lines_event"
let nvim_buf_changedtick_event = "nvim_buf_changedtick_event"
let nvim_buf_detach_event = "nvim_buf_detach_event"
let redraw = "redraw"

let events =
  [ nvim_buf_lines_event; nvim_buf_changedtick_event; nvim_buf_detach_event; redraw ]
;;

module Buffer_event = struct
  type changedtick = int [@@deriving sexp_of]

  module Private = struct
    let changedtick_eq = Type_equal.T
  end

  type t =
    | Changedtick of changedtick
    | Lines of
        { changedtick : changedtick option
        ; firstline : int
        ; lastline : int
        ; linedata : String.Utf8.t list
        ; more : bool
        }
  [@@deriving sexp_of]

  let buffer_of_msgpack_exn msg =
    match Buffer.of_msgpack msg with
    | Ok buffer -> buffer
    | Error error ->
      raise_s [%message "Failed to parse buffer" (error : Error.t)] [@nontail]
  ;;

  let on_lines ~f ~on_error =
    let method_name = nvim_buf_lines_event in
    let f params =
      match
        match (params : Msgpack.t list) with
        | [ buffer; changedtick; Int firstline; Int lastline; Array changes; Bool more ]
          ->
          let linedata =
            List.map changes ~f:(function
              | String s -> String.Utf8.of_string_unchecked s
              | change ->
                raise_s
                  [%message
                    [%string "Failed to parse %{method_name} change"] (change : Msgpack.t)]
                [@nontail])
          in
          let buffer = buffer_of_msgpack_exn buffer in
          let changedtick =
            match changedtick with
            | Nil -> None
            | Int i -> Some i
            | _ ->
              raise_s
                [%message
                  [%string "Failed to parse %{method_name} changedtick"]
                    (changedtick : Msgpack.t)] [@nontail]
          in
          buffer, Lines { changedtick; firstline; lastline; linedata; more }
        | _ -> failwith [%string "Failed to parse %{method_name} params"] [@nontail]
      with
      | buffer, event -> f ~buffer event
      | exception exn ->
        on_error
          (Vcaml_error.Nvim_buffer_event_parse_failure (exn, { method_name; params }))
    in
    method_name, f
  ;;

  let on_changedtick ~f ~on_error =
    let method_name = nvim_buf_changedtick_event in
    let f params =
      match
        match (params : Msgpack.t list) with
        | [ buffer; Int changedtick ] ->
          let buffer = buffer_of_msgpack_exn buffer in
          buffer, Changedtick changedtick
        | _ -> failwith [%string "Failed to parse %{method_name} params"] [@nontail]
      with
      | buffer, event -> f ~buffer event
      | exception exn ->
        on_error
          (Vcaml_error.Nvim_buffer_event_parse_failure (exn, { method_name; params }))
    in
    method_name, f
  ;;

  let on_detach ~f ~on_error =
    let method_name = nvim_buf_detach_event in
    let f params =
      match
        match (params : Msgpack.t list) with
        | [ buffer ] -> buffer_of_msgpack_exn buffer
        | _ -> failwith [%string "Failed to parse %{method_name} params"] [@nontail]
      with
      | buffer -> f ~buffer
      | exception exn ->
        on_error
          (Vcaml_error.Nvim_buffer_event_parse_failure (exn, { method_name; params }))
    in
    method_name, f
  ;;
end

module Ui_event = struct
  include Nvim_internal.Ui_event

  let on_redraw =
    let method_name = redraw in
    let of_msgpack_exn param =
      match of_msgpack param with
      | Ok events -> events
      | Error error -> Error.raise error [@nontail]
    in
    fun ~f ~on_error ->
      let f params =
        match List.concat_map params ~f:of_msgpack_exn with
        | events -> List.iter events ~f
        | exception exn ->
          on_error
            (Vcaml_error.Nvim_ui_event_parse_failure (exn, { method_name; params }))
      in
      method_name, f
  ;;
end

type t =
  { buffer_subscriptions : Buffer_event.t Pipe.Writer.t Buffer.Table.t
      (* [pending_buffer_subscriptions] keeps track of subscriptions we still need to make
         that are blocked on [unsubscribe] cleaning up after a previous connection before
         we can re-establish the subscription. *)
  ; pending_buffer_subscriptions : Buffer_event.t Pipe.Reader.t Ivar.t Buffer.Table.t
  ; mutable ui_subscription : Ui_event.t Pipe.Writer.t option
  }

(* [unsubscribe] is only safe to call when we are sure that the event stream for a
   subscription has ended. Otherwise, we could resubscribe and mistake a message from the
   previous subscription as being from the new subscription. *)
let unsubscribe t ~buffer =
  (match Hashtbl.find_and_remove t.buffer_subscriptions buffer with
   | None -> ()
   | Some writer -> Pipe.close writer);
  match Hashtbl.find_and_remove t.pending_buffer_subscriptions buffer with
  | None -> ()
  | Some pending_subscription ->
    (* There was a buffer subscription waiting for this [unsubscribe], so now that we have
       successfully unsubscribed we perform the pending subscription. *)
    let reader, writer = Pipe.create () in
    Hashtbl.set t.buffer_subscriptions ~key:buffer ~data:writer;
    Ivar.fill_exn pending_subscription reader
;;

let create rpc ~on_error =
  let t =
    { buffer_subscriptions = Buffer.Table.create ()
    ; pending_buffer_subscriptions = Buffer.Table.create ()
    ; ui_subscription = None
    }
  in
  let register_handler on_event ~f =
    let method_name, f = on_event ~f ~on_error in
    match Msgpack_rpc.register_notification_handler rpc ~name:method_name ~f with
    | `Ok -> ()
    | `Duplicate ->
      failwithf "Bug: Already registered %s handler" method_name () [@nontail]
  in
  let write_buffer_event ~buffer event =
    match Hashtbl.find t.buffer_subscriptions buffer with
    | None -> ()
    | Some writer -> Pipe.write_without_pushback_if_open writer event
  in
  let write_ui_event event =
    match t.ui_subscription with
    | None -> ()
    | Some writer -> Pipe.write_without_pushback_if_open writer event
  in
  register_handler Buffer_event.on_lines ~f:write_buffer_event;
  register_handler Buffer_event.on_changedtick ~f:write_buffer_event;
  register_handler Buffer_event.on_detach ~f:(unsubscribe t);
  register_handler Ui_event.on_redraw ~f:write_ui_event;
  t
;;

let subscribe_to_buffer t ~buffer =
  match Hashtbl.find t.buffer_subscriptions buffer with
  | None ->
    let reader, writer = Pipe.create () in
    Hashtbl.set t.buffer_subscriptions ~key:buffer ~data:writer;
    Deferred.Or_error.return reader
  | Some writer ->
    (match Pipe.is_closed writer with
     | false ->
       Deferred.Or_error.error_s
         [%message "Already subscribed to buffer" (buffer : Buffer.t)]
     | true ->
       (match Hashtbl.find t.pending_buffer_subscriptions buffer with
        | Some _ ->
          Deferred.Or_error.error_s
            [%message "Already subscribing to buffer" (buffer : Buffer.t)]
        | None ->
          let pending_subscription = Ivar.create () in
          Hashtbl.set
            t.pending_buffer_subscriptions
            ~key:buffer
            ~data:pending_subscription;
          Ivar.read pending_subscription |> Deferred.ok))
;;

let subscribe_to_ui_events t =
  match t.ui_subscription with
  | None ->
    let reader, writer = Pipe.create () in
    t.ui_subscription <- Some writer;
    Deferred.Or_error.return reader
  | Some _ -> Deferred.Or_error.error_string "Resubscribing to UI events is not supported"
;;

let cleanup_failed_buffer_subscription = unsubscribe
