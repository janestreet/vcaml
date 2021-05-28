open! Core
open Async

module Make (U : Msgpack_rpc.S) : sig
  val attach : U.t -> on_error:(Error.t -> unit) -> Client.t Deferred.t
end
