open! Core
open Async
open Vcaml

module State : sig
  type t =
    { window : Window.t
    ; buffer : Buffer.t
    }
end

val main : Core.Command.t

module For_testing : sig
  val run
    :  time_source:Time_source.t
    -> ?during_plugin:(chan_id:int -> state:State.t -> unit Deferred.Or_error.t)
    -> Client.t
    -> State.t Deferred.Or_error.t
end
