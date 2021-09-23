open Core
open Async

type event =
  { method_name : string
  ; params : Msgpack.t list
  }
[@@deriving sexp]

type t =
  { reader : Reader.t
  ; writer : Writer.t
  ; notifications_bus : (event -> unit) Bus.Read_only.t
  ; pending_requests : (Msgpack.t, Msgpack.t) Result.t Ivar.t Int.Table.t
  ; callbacks : (Msgpack.t list -> Msgpack.t Deferred.Or_error.t) String.Table.t
  ; create_id : unit -> Int63.t
  }
[@@deriving fields]

let event_loop t notifications_bus ~on_error ~close_reader_and_writer_on_disconnect =
  let handle_message msg =
    match msg with
    | Msgpack.Array [ Integer 1; Integer msgid; Nil; result ] ->
      (match Hashtbl.find t.pending_requests msgid with
       | None -> on_error ~message:(sprintf "Unknown message ID: %d" msgid) msg
       | Some box -> Ivar.fill box (Ok result))
    | Array [ Integer 1; Integer msgid; err; Nil ] ->
      (match Hashtbl.find t.pending_requests msgid with
       | None -> on_error ~message:(sprintf "Unknown message ID: %d" msgid) msg
       | Some box -> Ivar.fill box (Error err))
    | Array [ Integer 2; String method_name; Array params ] ->
      Bus.write notifications_bus { method_name; params }
    | Array [ Integer 0; Integer msgid; String method_name; Array params ] ->
      let respond msg = Writer.write t.writer (Msgpack.string_of_t_exn msg) in
      (match Hashtbl.find t.callbacks method_name with
       | None ->
         Array
           [ Integer 1; Integer msgid; String (sprintf "no method %s" method_name); Nil ]
         |> respond
       | Some f ->
         don't_wait_for
           (let%map result = f params in
            let response : Msgpack.t =
              match result with
              | Ok r -> Array [ Integer 1; Integer msgid; Nil; r ]
              | Error e ->
                Array
                  [ Integer 1
                  ; Integer msgid
                  ; String (e |> [%sexp_of: Error.t] |> Sexp.to_string)
                  ; Nil
                  ]
            in
            respond response))
    | _ -> on_error ~message:"Unexpected response" msg
  in
  let%bind parse_result =
    Angstrom_async.parse_many
      Msgpack.Internal.Parser.msg
      (fun msg ->
         (* Force synchronous message handling. *)
         handle_message msg;
         return ())
      t.reader
  in
  (match parse_result with
   | Ok () -> ()
   | Error details ->
     let message =
       sprintf "Unable to parse MessagePack-RPC data: %s. Parser stopped." details
     in
     on_error ~message Nil);
  match close_reader_and_writer_on_disconnect with
  | false -> return ()
  | true ->
    let%map () = Writer.close t.writer
    and () = Reader.close t.reader in
    ()
;;

(* If the [write] system call fails, fill all outstanding requests with errors. *)
let handle_write_syscall_failure t ~close_reader_and_writer_on_disconnect =
  let monitor = Writer.monitor t.writer in
  Monitor.detach_and_iter_errors monitor ~f:(fun exn ->
    let exn = Monitor.extract_exn exn in
    let error = Error (Msgpack.String (Exn.to_string exn)) in
    Hashtbl.iter t.pending_requests ~f:(fun result -> Ivar.fill result error);
    Hashtbl.clear t.pending_requests;
    match close_reader_and_writer_on_disconnect with
    | false -> ()
    | true ->
      (* We can close the [Reader.t] because even though there still may be unconsumed
         data, we have already responded to all the pending requests. *)
      don't_wait_for
        (let%map () = Writer.close t.writer
         and () = Reader.close t.reader in
         ()))
;;

let connect reader writer ~on_error ~close_reader_and_writer_on_disconnect =
  let notifications_bus =
    Bus.create
      [%here]
      Arity1
      ~on_subscription_after_first_write:Allow
      ~on_callback_raise:(* This should be impossible. *)
        Error.raise
  in
  let module Id_factory = Unique_id.Int63 () in
  let t =
    { reader
    ; writer
    ; notifications_bus = Bus.read_only notifications_bus
    ; pending_requests = Int.Table.create ()
    ; callbacks = String.Table.create ()
    ; create_id = (fun () -> (Id_factory.create () :> Int63.t))
    }
  in
  don't_wait_for
    (event_loop t notifications_bus ~on_error ~close_reader_and_writer_on_disconnect);
  handle_write_syscall_failure t ~close_reader_and_writer_on_disconnect;
  t
;;

let subscribe t = Async_bus.pipe1_exn t.notifications_bus

let to_native_uint32 =
  let mask =
    match Word_size.word_size with
    | W32 -> Int63.of_int 0x3FFFFFFF
    | W64 -> Int63.of_int64_trunc 0xFFFFFFFFL
  in
  fun x -> Int63.(x land mask |> to_int_exn)
;;

let call t ~method_name ~parameters =
  let msg_id = to_native_uint32 (t.create_id ()) in
  let query =
    Msgpack.Array [ Integer 0; Integer msg_id; String method_name; Array parameters ]
    (* This should be safe b/c we aren't serializing an extension. *)
  in
  match Writer.is_closed t.writer with
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
    let () = Writer.write t.writer (Msgpack.string_of_t_exn query) in
    (* Note that "flushed" performs a flush. We don't bind on it because it will fail if
       the file descriptor is bad (e.g., because the other side disconnected). *)
    don't_wait_for (Writer.flushed t.writer);
    let%bind result = Ivar.read result in
    Hashtbl.remove t.pending_requests msg_id;
    return result
;;

let notify t ~method_name ~parameters =
  let query =
    Msgpack.Array [ Integer 2; String method_name; Array parameters ]
    (* This should be safe b/c we aren't serializing an extension. *)
  in
  match Writer.is_closed t.writer with
  | true -> ()
  | false -> Writer.write t.writer (Msgpack.string_of_t_exn query)
;;

let register_method t ~name ~f =
  match Hashtbl.add t.callbacks ~key:name ~data:f with
  | `Ok -> Ok ()
  | `Duplicate -> Or_error.errorf "Duplicate method name %s" name
;;
