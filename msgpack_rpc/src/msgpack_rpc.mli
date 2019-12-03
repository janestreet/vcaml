open Core_kernel
open Async_kernel

module type Connection = sig
  type t

  val reader : t -> Async.Reader.t
  val writer : t -> Async.Writer.t
end

type event =
  { method_name : string
  ; params : Msgpack.t list
  }
[@@deriving sexp]

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

module Make (M : Connection) () : S with type conn = M.t
