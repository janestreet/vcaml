open! Core
open Async

module Make (U : Msgpack_rpc.S) : sig
  val attach : U.t -> Types.Client.t Deferred.t
end
