open! Async
open Vcaml

val run_rpc_calls
  :  client:Client.t
  -> chan_id:int
  -> rpcs:(string * Msgpack.t list * (Msgpack.t -> unit Deferred.Or_error.t)) list
  -> unit Deferred.Or_error.t
