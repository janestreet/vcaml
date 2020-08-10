open! Core
open! Async
open Vcaml

val main : Command.t

module For_testing : sig
  val start_plugin_for_testing
    :  client:Client.t
    -> name:string
    -> terminate_var:unit Ivar.t
    -> unit Deferred.Or_error.t

  val get_rpc_chan_for_testing : client:Client.t -> name:string -> int Deferred.Or_error.t
end
