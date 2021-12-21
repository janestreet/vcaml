open Base
open Async_kernel

(** Implements the Msgpack RPC protocol.
    See https://github.com/msgpack-rpc/msgpack-rpc/blob/master/spec.md *)

module Event : sig
  type t =
    { method_name : string
    ; params : Msgpack.t list
    }
  [@@deriving sexp_of]
end

module Error : sig

  (** [Fatal_parse_failure] happens when we receive data that does not conform to the
      Msgpack specification. The parser stops and if [close_read_and_writer_on_disconnect]
      was [true] when [connect] was called, [reader] and [writer] will be closed.

      [Invalid_rpc_message] happens when we receive valid Msgpack data that does not
      conform to the Msgpack RPC protocol.

      [Response_for_unknown_request] happens when we receive a message that is meant to be
      a response to a synchronous request we sent but we don't have a record of that
      request (i.e., the response's message ID is unknown).

      [Unknown_method_called] happens when we receive a synchronous request for a method
      that has not yet been registered. *)
  type t =
    | Fatal_parse_failure of string
    | Invalid_rpc_message of Msgpack.t
    | Response_for_unknown_request of Msgpack.t
    | Unknown_method_called of Msgpack.t
  [@@deriving sexp_of]

  val to_error : t -> Error.t
end

type 'state t

val create : on_error:(Error.t -> unit) -> [ `not_connected ] t

(** Although you can still subscribe after the RPC is connected, note that if you do you
    may miss events sent before the subscription. *)
val notifications : _ t -> (Event.t -> unit) Bus.Read_only.t

(** Although you can still register methods after the RPC is connected, note that if
    clients call the method before registration they will see an error for an undefined
    method. *)
val register_method
  :  _ t
  -> name:string
  -> f:(Msgpack.t list -> Msgpack.t Deferred.Or_error.t)
  -> [ `Ok | `Duplicate ]


(** Once [connect] is called the counterparty can start calling methods and sending
    notifications. Any methods that should be callable at that time should be registered
    beforehand, as should any subscriptions to the event bus. Calling [connect] twice will
    raise. *)
val connect
  :  [ `not_connected ] t
  -> Async.Reader.t
  -> Async.Writer.t
  -> close_reader_and_writer_on_disconnect:bool
  -> [ `connected ] t

val reader : [ `connected ] t -> Async.Reader.t
val writer : [ `connected ] t -> Async.Writer.t

val call
  :  [ `connected ] t
  -> method_name:string
  -> parameters:Msgpack.t list
  -> (Msgpack.t, Msgpack.t) Deferred.Result.t

val notify : [ `connected ] t -> method_name:string -> parameters:Msgpack.t list -> unit
