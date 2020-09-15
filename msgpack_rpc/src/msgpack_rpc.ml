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

  val connect : conn -> t

  val register_method
    :  name:string
    -> f:(Msgpack.t list -> Msgpack.t Or_error.t)
    -> unit Or_error.t
end

module Make (M : Connection) () = struct
  type conn = M.t
  type t = M.t * (event -> unit) Bus.Read_only.t

  module Id_factory = Unique_id.Int63 ()

  let pending_requests : (Msgpack.t, Msgpack.t) Result.t Ivar.t Int.Table.t =
    Int.Table.create ()
  ;;

  let subscribe ((_, notifications_bus) : t) = Async_bus.pipe1_exn notifications_bus

  let synchronous_callbacks : (Msgpack.t list -> Msgpack.t Or_error.t) String.Table.t =
    String.Table.create ()
  ;;

  let register_method ~name ~f =
    match Hashtbl.add synchronous_callbacks ~key:name ~data:f with
    | `Ok -> Ok ()
    | `Duplicate -> Or_error.errorf "duplicate method name %s" name
  ;;

  let event_loop conn notifications_bus =
    let handle_message = function
      | Msgpack.Array [ Integer 1; Integer msgid; Nil; result ] ->
        (match Hashtbl.find pending_requests msgid with
         | None ->
           Log.Global.error "Unknown message ID: %d" msgid;
           return ()
         | Some box ->
           Ivar.fill box (Ok result);
           return ())
      | Msgpack.Array [ Integer 1; Integer msgid; err; Nil ] ->
        (match Hashtbl.find pending_requests msgid with
         | None ->
           Log.Global.error "Unknown message ID: %d" msgid;
           return ()
         | Some box ->
           Ivar.fill box (Error err);
           return ())
      | Msgpack.Array [ Integer 2; String method_name; Array params ] ->
        Bus.write notifications_bus { method_name; params };
        return ()
      | Msgpack.Array [ Integer 0; Integer msgid; String method_name; Array params ] ->
        let resp =
          match Hashtbl.find synchronous_callbacks method_name with
          | None ->
            Msgpack.Array
              [ Msgpack.Integer 1
              ; Integer msgid
              ; String (sprintf "no method %s" method_name)
              ; Nil
              ]
          | Some f ->
            (match Or_error.try_with_join (fun () -> f params) with
             | Ok r -> Msgpack.Array [ Msgpack.Integer 1; Integer msgid; Nil; r ]
             | Error e ->
               Msgpack.Array
                 [ Msgpack.Integer 1
                 ; Integer msgid
                 ; String (e |> [%sexp_of: Error.t] |> Sexp.to_string)
                 ; Nil
                 ])
        in
        Async.Writer.write (M.writer conn) (Msgpack.string_of_t_exn resp);
        return ()
      | msg ->
        Log.Global.error !"unexpected msgpack response: %{sexp:Msgpack.t}\n" msg;
        return ()
    in
    match%bind
      Angstrom_async.parse_many Msgpack.Internal.Parser.msg handle_message (M.reader conn)
    with
    | Ok () -> return ()
    | Error s ->
      Log.Global.error "Unable to parse messagepack-rpc response: %s" s;
      return ()
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

  let connect conn =
    let notifications_bus =
      Bus.create
        [%here]
        Arity1
        ~on_subscription_after_first_write:Allow
        ~on_callback_raise:(* This should be impossible. *)
          Error.raise
    in
    don't_wait_for (event_loop conn notifications_bus);
    conn, Bus.read_only notifications_bus
  ;;

  let call (conn, _) ~method_name ~parameters =
    let cross_plat_int_max = Int.pow 2 31 in
    let open Msgpack in
    let msg_id = Id_factory.create () |> Id_factory.to_int_exn in
    let method_name = String method_name in
    let query_msg =
      Array [ Integer 0; Integer (msg_id % cross_plat_int_max); method_name; parameters ]
    in
    let result_box = register msg_id in
    let query = Msgpack.string_of_t_exn query_msg in
    let () = Async.Writer.write (M.writer conn) query in
    wait_for_response result_box
  ;;
end
