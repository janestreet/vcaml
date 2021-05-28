open Core_kernel
open Async

type event =
  { method_name : string
  ; params : Msgpack.t list
  }
[@@deriving sexp]

module type Connection = sig
  type t

  val reader : t -> Async.Reader.t
  val writer : t -> Async.Writer.t
end

module type S = sig
  type conn
  type t

  val subscribe : t -> Source_code_position.t -> event Pipe.Reader.t

  val call
    :  t
    -> method_name:string
    -> parameters:Msgpack.t
    -> (Msgpack.t, Msgpack.t) Deferred.Result.t

  val notify : t -> method_name:string -> parameters:Msgpack.t -> unit
  val connect : conn -> on_error:(message:string -> Msgpack.t -> unit) -> t

  val register_method
    :  name:string
    -> f:(Msgpack.t list -> Msgpack.t Deferred.Or_error.t)
    -> unit Or_error.t
end

module Make (M : Connection) () = struct
  type conn = M.t

  type t =
    { connection : M.t
    ; notifications_bus : (event -> unit) Bus.Read_only.t
    }
  [@@deriving fields]

  let pending_requests : (Msgpack.t, Msgpack.t) Result.t Ivar.t Int.Table.t =
    Int.Table.create ()
  ;;

  let subscribe { connection = _; notifications_bus } =
    Async_bus.pipe1_exn notifications_bus
  ;;

  let callbacks : (Msgpack.t list -> Msgpack.t Deferred.Or_error.t) String.Table.t =
    String.Table.create ()
  ;;

  let register_method ~name ~f =
    match Hashtbl.add callbacks ~key:name ~data:f with
    | `Ok -> Ok ()
    | `Duplicate -> Or_error.errorf "Duplicate method name %s" name
  ;;

  let event_loop conn notifications_bus ~on_error =
    let handle_message msg =
      match msg with
      | Msgpack.Array [ Integer 1; Integer msgid; Nil; result ] ->
        (match Hashtbl.find pending_requests msgid with
         | None -> on_error ~message:(sprintf "Unknown message ID: %d" msgid) msg
         | Some box -> Ivar.fill box (Ok result))
      | Array [ Integer 1; Integer msgid; err; Nil ] ->
        (match Hashtbl.find pending_requests msgid with
         | None -> on_error ~message:(sprintf "Unknown message ID: %d" msgid) msg
         | Some box -> Ivar.fill box (Error err))
      | Array [ Integer 2; String method_name; Array params ] ->
        Bus.write notifications_bus { method_name; params }
      | Array [ Integer 0; Integer msgid; String method_name; Array params ] ->
        let respond msg = Writer.write (M.writer conn) (Msgpack.string_of_t_exn msg) in
        (match Hashtbl.find callbacks method_name with
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
    match%map
      Angstrom_async.parse_many
        Msgpack.Internal.Parser.msg
        (fun msg ->
           (* Force synchronous message handling. *)
           handle_message msg;
           return ())
        (M.reader conn)
    with
    | Ok () -> ()
    | Error details ->
      let message =
        sprintf "Unable to parse MessagePack-RPC data: %s. Parser stopped." details
      in
      on_error ~message Nil
  ;;

  let register msg_id =
    let box = Ivar.create () in
    Hashtbl.set pending_requests ~key:msg_id ~data:box;
    box, msg_id
  ;;

  let wait_for_response (box, msg_id) =
    let%bind result = Ivar.read box in
    Hashtbl.remove pending_requests msg_id;
    return result
  ;;

  let connect connection ~on_error =
    let notifications_bus =
      Bus.create
        [%here]
        Arity1
        ~on_subscription_after_first_write:Allow
        ~on_callback_raise:(* This should be impossible. *)
          Error.raise
    in
    don't_wait_for (event_loop connection notifications_bus ~on_error);
    { connection; notifications_bus = Bus.read_only notifications_bus }
  ;;

  module Id_factory = Unique_id.Int63 ()

  let to_native_uint32 =
    let mask =
      match Word_size.word_size with
      | W32 -> Int63.of_int 0x3FFFFFFF
      | W64 -> Int63.of_int64_trunc 0xFFFFFFFFL
    in
    fun x -> Int63.(x land mask |> to_int_exn)
  ;;

  let call t ~method_name ~parameters =
    let msg_id = to_native_uint32 (Id_factory.create () :> Int63.t) in
    let query =
      Array [ Integer 0; Integer msg_id; String method_name; parameters ]
      (* This should be safe b/c we aren't serializing an extension. *)
      |> Msgpack.string_of_t_exn
    in
    let result_box = register msg_id in
    let () = Async.Writer.write (M.writer t.connection) query in
    wait_for_response result_box
  ;;

  let notify t ~method_name ~parameters =
    let query =
      Array [ Integer 2; String method_name; parameters ]
      (* This should be safe b/c we aren't serializing an extension. *)
      |> Msgpack.string_of_t_exn
    in
    Async.Writer.write (M.writer t.connection) query
  ;;
end
