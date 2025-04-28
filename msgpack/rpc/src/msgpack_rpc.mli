open Core
open Async_kernel

(** Implements the Msgpack RPC protocol. See
    https://github.com/msgpack-rpc/msgpack-rpc/blob/master/spec.md *)

module Error : sig
  (** [Fatal_parse_failure] happens when we receive data that does not conform to the
      Msgpack specification. Parsing stops and [reader] and [writer] will be closed.

      [Invalid_rpc_message] happens when we receive valid Msgpack data that does not
      conform to the Msgpack RPC protocol.

      [Response_for_unknown_request] happens when we receive a message that is meant to be
      a response to a synchronous request we sent but we don't have a record of that
      request (i.e., the response's message ID is unknown). *)
  type t =
    | Fatal_parse_failure of string
    | Invalid_rpc_message of Msgpack.t
    | Response_for_unknown_request of Msgpack.t
  [@@deriving sexp_of]

  val to_error : t -> Error.t
end

type t

(** Create a new Msgpack RPC instance. *)
val create : on_error:(Error.t -> unit) -> t

(** Once [connect] is called the counterparty can start calling methods and sending
    notifications. Any methods that should be callable at that time should be registered
    beforehand. Calling [connect] twice will raise. *)
val connect : t -> Async.Reader.t -> Async.Writer.t -> unit

(** Get the [reader] provided to [connect]. Fails if [connect] has not yet been called. *)
val reader : t -> Async.Reader.t

(** Get the [writer] provided to [connect]. Fails if [connect] has not yet been called. *)
val writer : t -> Async.Writer.t

val call
  :  t
  -> method_name:string
  -> parameters:Msgpack.t list
  -> (Msgpack.t, Msgpack.t) Result.t Deferred.Or_error.t

val notify
  :  t
  -> method_name:string
  -> parameters:Msgpack.t list
  -> unit Deferred.Or_error.t

(** Register a handler that will be called when a request (message type 0) for method
    [name] is received. [on_response_sent] is called after the response has been written
    and the writer is flushed (if you don't need this hook, just pass [ignore]).

    Although you can still register handlers after the RPC is connected, note that if
    clients call the method before registration they will receive an error response for an
    undefined method. *)
val register_request_handler
  :  t
  -> name:string
  -> f:(Msgpack.t list -> (Msgpack.t Or_error.t * 'a) Deferred.t)
  -> on_response_sent:('a -> unit)
  -> [ `Ok | `Duplicate ]

(** Register a handler that will be called when a notification (message type 2) for method
    [name] is received.

    Although you can still register handlers after the RPC is connected, note that if
    clients call the method before registration the default notification handler will be
    invoked. *)
val register_notification_handler
  :  t
  -> name:string
  -> f:(Msgpack.t list -> unit)
  -> [ `Ok | `Duplicate ]

(** Set the default notification handler to be invoked when a notification is received for
    a method for which no handler is registered. We don't need a default request handler
    because we can just return an error response when a request for an unknown method is
    received, but notifications do not have responses so we need to define a behavior for
    this scenario explicitly. By default these notifications will be ignored. *)
val set_default_notification_handler
  :  t
  -> f:(name:string -> Msgpack.t list -> unit)
  -> unit

module Expert : sig
  val unregister_request_handler : t -> name:string -> unit
  val unregister_notification_handler : t -> name:string -> unit
  val registered_request_handlers : t -> string list
  val registered_notification_handlers : t -> string list
end
