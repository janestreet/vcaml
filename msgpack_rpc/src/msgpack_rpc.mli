open Core
open Async_kernel

(** Implements the Msgpack RPC protocol.
    See https://github.com/msgpack-rpc/msgpack-rpc/blob/master/spec.md *)

type event =
  { method_name : string
  ; params : Msgpack.t list
  }
[@@deriving sexp]

type t

val connect
  :  Async.Reader.t
  -> Async.Writer.t
  -> on_error:(message:string -> Msgpack.t -> unit)
  -> close_reader_and_writer_on_disconnect:bool
  -> t

val reader : t -> Async.Reader.t
val writer : t -> Async.Writer.t
val subscribe : t -> Source_code_position.t -> event Pipe.Reader.t

val call
  :  t
  -> method_name:string
  -> parameters:Msgpack.t list
  -> (Msgpack.t, Msgpack.t) Deferred.Result.t

val notify : t -> method_name:string -> parameters:Msgpack.t list -> unit

val register_method
  :  t
  -> name:string
  -> f:(Msgpack.t list -> Msgpack.t Deferred.Or_error.t)
  -> unit Or_error.t
