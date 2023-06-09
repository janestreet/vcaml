open Core
open Async

module Event = struct
  type t =
    { method_name : string
    ; params : Msgpack.t list
    }
  [@@deriving sexp_of]
end

module Error = struct
  type t =
    | Fatal_parse_failure of string
    | Invalid_rpc_message of Msgpack.t
    | Response_for_unknown_request of Msgpack.t
    | Unknown_method_called of Msgpack.t
  [@@deriving sexp_of]

  let to_error t = Error.create_s [%sexp (t : t)]
end

(* Because we copy [t] when [connect] is called, this record should not have any mutable
   fields. We don't want a situation where a [[`not_connected] t] is manipulated after
   [connect] is called but the [t] returned by [connect] doesn't reflect the change.  *)
type _ t =
  { reader : Reader.t Set_once.t
  ; writer : Writer.t Set_once.t
  ; notifications : (Event.t -> unit) Bus.Read_write.t
  ; pending_requests : (Msgpack.t, Msgpack.t) Result.t Ivar.t Int.Table.t
  ; callbacks : (Msgpack.t list -> Msgpack.t Deferred.Or_error.t) String.Table.t
  ; create_id : unit -> Int63.t
  ; on_error : Error.t -> unit
  }
[@@deriving fields]

let event_loop t ~close_reader_and_writer_on_disconnect =
  let reader = Set_once.get_exn t.reader [%here] in
  let writer = Set_once.get_exn t.writer [%here] in
  let handle_message msg =
    match msg with
    | Msgpack.Array [ Integer 1; Integer msgid; Nil; result ] ->
      (match Hashtbl.find t.pending_requests msgid with
       | None -> t.on_error (Response_for_unknown_request msg)
       | Some box -> Ivar.fill_exn box (Ok result))
    | Array [ Integer 1; Integer msgid; err; Nil ] ->
      (match Hashtbl.find t.pending_requests msgid with
       | None -> t.on_error (Response_for_unknown_request msg)
       | Some box -> Ivar.fill_exn box (Error err))
    | Array [ Integer 2; String method_name; Array params ] ->
      Bus.write t.notifications { method_name; params }
    | Array [ Integer 0; Integer msgid; String method_name; Array params ] ->
      let respond msg = Writer.write writer (Msgpack.string_of_t_exn msg) in
      (match Hashtbl.find t.callbacks method_name with
       | None ->
         Array
           [ Integer 1
           ; Integer msgid
           ; String (sprintf "Unknown method %s" method_name)
           ; Nil
           ]
         |> respond;
         t.on_error (Unknown_method_called msg)
       | Some f ->
         don't_wait_for
           (let%map result = f params in
            let response : Msgpack.t =
              match result with
              | Ok r -> Array [ Integer 1; Integer msgid; Nil; r ]
              | Error error ->
                Array
                  [ Integer 1
                  ; Integer msgid
                  ; String (Core.Error.to_string_mach error)
                  ; Nil
                  ]
            in
            respond response))
    | _ -> t.on_error (Invalid_rpc_message msg)
  in
  let%bind parse_result =
    Angstrom_async.parse_many
      Msgpack.Internal.Parser.msg
      (fun msg ->
         (* Force synchronous message handling. *)
         handle_message msg;
         return ())
      reader
  in
  (match parse_result with
   | Ok () -> ()
   | Error details -> t.on_error (Fatal_parse_failure details));
  match close_reader_and_writer_on_disconnect with
  | false -> return ()
  | true ->
    let%map () = Writer.close writer
    and () = Reader.close reader in
    ()
;;

(* If the [write] system call fails, fill all outstanding requests with errors. *)
let handle_write_syscall_failure t ~close_reader_and_writer_on_disconnect =
  let reader = Set_once.get_exn t.reader [%here] in
  let writer = Set_once.get_exn t.writer [%here] in
  let monitor = Writer.monitor writer in
  Monitor.detach_and_iter_errors monitor ~f:(fun exn ->
    let exn = Monitor.extract_exn exn in
    let error = Error (Msgpack.String (Exn.to_string exn)) in
    Hashtbl.iter t.pending_requests ~f:(fun result -> Ivar.fill_exn result error);
    Hashtbl.clear t.pending_requests;
    match close_reader_and_writer_on_disconnect with
    | false -> ()
    | true ->
      (* We can close the [Reader.t] because even though there still may be unconsumed
         data, we have already responded to all the pending requests. *)
      don't_wait_for
        (let%map () = Writer.close writer
         and () = Reader.close reader in
         ()))
;;

let connect t reader writer ~close_reader_and_writer_on_disconnect =
  Set_once.set_exn t.reader [%here] reader;
  Set_once.set_exn t.writer [%here] writer;
  don't_wait_for (event_loop t ~close_reader_and_writer_on_disconnect);
  handle_write_syscall_failure t ~close_reader_and_writer_on_disconnect;
  (* Copy [t] to satisfy type checker. Only happens once per connection (and we are using
     Async anyway) so we prefer it to using [Obj.magic]. *)
  { t with reader = t.reader }
;;

let create ~on_error =
  let notifications =
    Bus.create_exn
      [%here]
      Arity1
      ~on_subscription_after_first_write:Allow
      ~on_callback_raise:Core.Error.raise
      (* We don't want to catch errors raised by client code. *)
  in
  let module Id_factory = Unique_id.Int63 () in
  { reader = Set_once.create ()
  ; writer = Set_once.create ()
  ; notifications
  ; pending_requests = Int.Table.create ()
  ; callbacks = String.Table.create ()
  ; create_id = (fun () -> (Id_factory.create () :> Int63.t))
  ; on_error
  }
;;

let notifications t = Bus.read_only t.notifications

let to_native_uint32 =
  let mask =
    match Word_size.word_size with
    | W32 -> Int63.of_int 0x3FFFFFFF
    | W64 -> Int63.of_int64_trunc 0xFFFFFFFFL
  in
  fun x -> Int63.(x land mask |> to_int_exn)
;;

let call t ~method_name ~parameters =
  let writer = Set_once.get_exn t.writer [%here] in
  let msg_id = to_native_uint32 (t.create_id ()) in
  let query =
    Msgpack.Array [ Integer 0; Integer msg_id; String method_name; Array parameters ]
    (* This should be safe b/c we aren't serializing an extension. *)
  in
  match Writer.is_closed writer with
  | true ->
    Error
      (Msgpack.String
         (Sexp.to_string
            [%message
              "Failed to send Msgpack RPC request: writer is closed"
                (method_name : string)
                (parameters : Msgpack.t list)]))
    |> return
  | false ->
    let result = Ivar.create () in
    Hashtbl.set t.pending_requests ~key:msg_id ~data:result;
    let () = Writer.write writer (Msgpack.string_of_t_exn query) in
    (* Note that "flushed" performs a flush. We don't bind on it because it will fail if
       the file descriptor is bad (e.g., because the other side disconnected). *)
    don't_wait_for (Writer.flushed writer);
    let%bind result = Ivar.read result in
    Hashtbl.remove t.pending_requests msg_id;
    return result
;;

let notify t ~method_name ~parameters =
  let writer = Set_once.get_exn t.writer [%here] in
  let query =
    Msgpack.Array [ Integer 2; String method_name; Array parameters ]
    (* This should be safe b/c we aren't serializing an extension. *)
  in
  match Writer.is_closed writer with
  | true -> ()
  | false -> Writer.write writer (Msgpack.string_of_t_exn query)
;;

let register_method t ~name ~f = Hashtbl.add t.callbacks ~key:name ~data:f
let reader t = Set_once.get_exn t.reader [%here]
let writer t = Set_once.get_exn t.writer [%here]
