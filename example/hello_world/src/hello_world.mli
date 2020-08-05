open! Core
open! Async
open Vcaml

val main : Command.t

module For_testing : sig
  val run_plugin_for_testing : client:Client.t -> unit Deferred.Or_error.t
end
