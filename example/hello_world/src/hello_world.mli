open! Core
open! Async
open Vcaml

val main : Command.t

val test
  :  ?before_plugin:(client:Client.t -> unit Deferred.Or_error.t)
  -> ?during_plugin:
       (client:Client.t -> chan_id:int -> state:unit -> unit Deferred.Or_error.t)
  -> ?after_plugin:(client:Client.t -> state:unit -> unit Deferred.Or_error.t)
  -> unit
  -> unit Deferred.t
