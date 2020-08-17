open! Core
open Async
open Vcaml

module State : sig
  type t =
    { window : Window.t
    ; buffer : Buf.t
    }
end

val main : Command.t

val test
  :  time_source:Time_source.t
  -> ?before_plugin:(client:Client.t -> unit Deferred.Or_error.t)
  -> ?during_plugin:
       (client:Client.t -> chan_id:int -> state:State.t -> unit Deferred.Or_error.t)
  -> ?after_plugin:(client:Client.t -> state:State.t -> unit Deferred.Or_error.t)
  -> unit
  -> unit Deferred.t
