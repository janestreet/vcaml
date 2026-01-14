open Core
open Async

module Error = struct
  type t =
    | Fatal_parse_failure of string
    | Invalid_rpc_message of Msgpack.t
    | Response_for_unknown_request of Msgpack.t
  [@@deriving sexp_of]

  let to_error t = Error.create_s [%sexp (t : t)]
end

module Request_handler = struct
  type t =
    | T :
        { f : Msgpack.t list -> (Msgpack.t Or_error.t * 'a) Deferred.t
        ; on_response_sent : 'a -> unit
        }
        -> t
end

module Connection_closed_reason = struct
  type t =
    | Reader_reached_eof
    | Read_failure of Core.Error.t
    | Write_failure of Core.Error.t
end

type t =
  { reader : Reader.t Set_once.t
  ; writer : Writer.t Set_once.t
  ; connection_closed : (Connection_closed_reason.t -> unit) Bus.Read_write.t
  ; pending_requests : (Msgpack.t, Msgpack.t) Result.t Ivar.t Int.Table.t
  ; request_handlers : Request_handler.t String.Table.t
  ; notification_handlers : (Msgpack.t list -> unit) String.Table.t
  ; create_id : unit -> Int63.t
  ; on_error : Error.t -> unit
  ; mutable default_notification_handler : name:string -> Msgpack.t list -> unit
  }

let reader t =
  match Set_once.get t.reader with
  | Some reader -> reader
  | None ->
    raise_s [%message "Tried to get reader for Msgpack RPC before calling [connect]"]
;;

let writer t =
  match Set_once.get t.writer with
  | Some writer -> writer
  | None ->
    raise_s [%message "Tried to get writer for Msgpack RPC before calling [connect]"]
;;

let close_connection t reason =
  match Bus.is_closed t.connection_closed with
  | true -> ()
  | false ->
    Bus.write t.connection_closed reason;
    Bus.close t.connection_closed;
    let reader = Set_once.get_exn t.reader in
    let writer = Set_once.get_exn t.writer in
    (* We can close the [Reader.t] because even though there still may be unconsumed data,
       we have already responded to all the pending requests. *)
    don't_wait_for
      (let%map () = Writer.close writer
       and () = Reader.close reader in
       ())
;;

let event_loop t =
  let writer = writer t in
  let handle_message msg =
    match msg with
    | Msgpack.Array [ Int 1; Int msgid; Nil; result ] ->
      (match Hashtbl.find t.pending_requests msgid with
       | None -> t.on_error (Response_for_unknown_request msg)
       | Some box -> Ivar.fill_exn box (Ok result))
    | Array [ Int 1; Int msgid; err; Nil ] ->
      (match Hashtbl.find t.pending_requests msgid with
       | None -> t.on_error (Response_for_unknown_request msg)
       | Some box -> Ivar.fill_exn box (Error err))
    | Array [ Int 2; String method_name; Array params ] ->
      (match Hashtbl.find t.notification_handlers method_name with
       | None -> t.default_notification_handler ~name:method_name params
       | Some f -> f params)
    | Array [ Int 0; Int msgid; String method_name; Array params ] ->
      let write_response result ~on_response_sent x =
        match Writer.is_closed writer with
        | true -> return ()
        | false ->
          let result : Msgpack.t list =
            match result with
            | `Unknown_method -> [ String (sprintf "Unknown method %s" method_name); Nil ]
            | `Result (Ok result) -> [ Nil; result ]
            | `Result (Error error) -> [ String (Core.Error.to_string_hum error); Nil ]
          in
          let message = Msgpack.string_of_t_exn (Array (Int 1 :: Int msgid :: result)) in
          Writer.write writer message;
          let%map () = Writer.flushed writer in
          on_response_sent x
      in
      don't_wait_for
        (match Hashtbl.find t.request_handlers method_name with
         | None -> write_response `Unknown_method ~on_response_sent:ignore ()
         | Some (T { f; on_response_sent }) ->
           let%bind result, x = f params in
           write_response (`Result result) ~on_response_sent x)
    | _ -> t.on_error (Invalid_rpc_message msg)
  in
  let%map parse_result =
    Angstrom_async.parse_many
      Msgpack.Internal.Parser.msg
      (fun msg ->
        (* Force synchronous message handling. Note that if a request or notification
           handler raises, we don't do anything special to handle it. Clients that want to
           prevent raising can use [Monitor.try_with] inside their handlers. If
           [handle_message] raises due to its own logic, that's a bug. *)
        handle_message msg;
        return ())
      (reader t)
  in
  let (reason : Connection_closed_reason.t) =
    match parse_result with
    | Ok () -> Reader_reached_eof
    | Error parse_failure_details ->
      t.on_error (Fatal_parse_failure parse_failure_details);
      Read_failure
        (Core.Error.of_string
           [%string
             "Connection closed due to fatal parse failure: %{parse_failure_details}"])
  in
  close_connection t reason
;;

let connect t reader writer =
  Set_once.set_exn t.reader reader;
  Set_once.set_exn t.writer writer;
  don't_wait_for (event_loop t);
  (* If the [write] system call fails, close the connection. *)
  don't_wait_for
    (let%map exn = Monitor.detach_and_get_next_error (Writer.monitor writer) in
     close_connection t (Write_failure (Core.Error.of_exn (Monitor.extract_exn exn))))
;;

let create ~on_error =
  let connection_closed =
    (* Although we will only ever write a single value to [connection_closed], we
       implement it as a bus rather than as an ivar to allow listeners to unsubscribe,
       thereby avoiding a memory leak. *)
    Bus.create_exn
      ~on_subscription_after_first_write:Raise
      ~on_callback_raise:Core.Error.raise
      ()
  in
  let module Id_factory = Unique_id.Int63 () in
  { reader = Set_once.create ()
  ; writer = Set_once.create ()
  ; connection_closed
  ; pending_requests = Int.Table.create ()
  ; request_handlers = String.Table.create ()
  ; notification_handlers = String.Table.create ()
  ; create_id = (fun () -> (Id_factory.create () :> Int63.t))
  ; on_error
  ; default_notification_handler = (fun ~name:_ _ -> ())
  }
;;

let to_native_uint32 =
  let mask =
    match Word_size.word_size with
    | W32 -> Int63.of_int 0x3FFFFFFF
    | W64 -> Int63.of_int64_trunc 0xFFFFFFFFL
  in
  fun x -> Int63.(x land mask |> to_int_exn)
;;

let write_message t ~query ~on_successful_flush =
  let writer = writer t in
  match Writer.is_closed writer with
  | true ->
    Deferred.Or_error.error_s
      [%message
        "Failed to send Msgpack RPC message: writer is closed" ~_:(query : Msgpack.t)]
  | false ->
    let connection_closed = Ivar.create () in
    let subscriber =
      (* We subscribe to [t.connection_closed] before calling [write] to avoid a race
         where the write fails and we miss the message that the connection was closed. *)
      Bus.subscribe_exn t.connection_closed ~f:(Ivar.fill_exn connection_closed)
    in
    let connection_closed = Ivar.read connection_closed in
    Writer.write writer (Msgpack.string_of_t_exn query);
    let%map result =
      match%bind Writer.flushed_or_failed_with_result writer with
      | Flushed _ -> on_successful_flush ~connection_closed
      | Force_closed ->
        Deferred.Or_error.error_s
          [%message
            "Failed to send Msgpack RPC message: writer is closed" ~_:(query : Msgpack.t)]
      | Consumer_left ->
        failwith
          "BUG: [Consumer_left] should only be possible when \
           [raise_when_consumer_leaves] is set to false."
      | Error ->
        (* The write side of the connection closing can race with the read side - we
           return whichever error is reported first. *)
        (match%map connection_closed with
         | Read_failure error | Write_failure error ->
           Or_error.error_s
             [%message
               "Failed to send Msgpack RPC message"
                 ~_:(query : Msgpack.t)
                 (error : Core.Error.t)]
         | Reader_reached_eof ->
           Or_error.error_s
             [%message
               "Failed to send Msgpack RPC message: consumer left" ~_:(query : Msgpack.t)])
    in
    Bus.unsubscribe t.connection_closed subscriber;
    result
;;

let call t ~method_name ~parameters =
  let msg_id = to_native_uint32 (t.create_id ()) in
  let request =
    Msgpack.Array [ Int 0; Int msg_id; String method_name; Array parameters ]
  in
  let result = Ivar.create () in
  Hashtbl.set t.pending_requests ~key:msg_id ~data:result;
  let%map result =
    write_message t ~query:request ~on_successful_flush:(fun ~connection_closed ->
      choose
        [ choice connection_closed (function
            | Read_failure error ->
              Or_error.error_s
                [%message
                  "While this request was waiting for a response, a message was received \
                   that failed to parse"
                    (request : Msgpack.t)
                    (error : Core.Error.t)]
            | Reader_reached_eof ->
              Or_error.error_s
                [%message "Consumer left without responding" (request : Msgpack.t)]
            | Write_failure unrelated_error ->
              Or_error.error_s
                [%message
                  "While this request was waiting for a response, an unrelated write \
                   failed and caused the connection to close."
                    (request : Msgpack.t)
                    (unrelated_error : Core.Error.t)])
        ; choice (Ivar.read result) (fun result -> Ok result)
        ])
  in
  Hashtbl.remove t.pending_requests msg_id;
  result
;;

let notify t ~method_name ~parameters =
  let query = Msgpack.Array [ Int 2; String method_name; Array parameters ] in
  write_message t ~query ~on_successful_flush:(fun ~connection_closed:_ ->
    Deferred.Or_error.return ())
;;

let register_request_handler t ~name ~f ~on_response_sent =
  Hashtbl.add t.request_handlers ~key:name ~data:(T { f; on_response_sent })
;;

let register_notification_handler t ~name ~f =
  Hashtbl.add t.notification_handlers ~key:name ~data:f
;;

let set_default_notification_handler t ~f = t.default_notification_handler <- f

module Expert = struct
  let unregister_request_handler t ~name = Hashtbl.remove t.request_handlers name

  let unregister_notification_handler t ~name =
    Hashtbl.remove t.notification_handlers name
  ;;

  let registered_request_handlers t = Hashtbl.keys t.request_handlers
  let registered_notification_handlers t = Hashtbl.keys t.notification_handlers
end
