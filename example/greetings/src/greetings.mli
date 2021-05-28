open! Core
open! Async
open Vcaml

val main : Core.Command.t

module For_testing : sig
  val run
    :  ?during_plugin:(chan_id:int -> state:unit -> unit Deferred.Or_error.t)
    -> Client.t
    -> unit Deferred.Or_error.t
end
