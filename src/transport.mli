open! Core
open Async

module Make (U : Msgpack_rpc.S) : sig
  val attach : U.t -> Types.client Deferred.Or_error.t
end
