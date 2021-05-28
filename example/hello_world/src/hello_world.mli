open! Core
open! Async
open Vcaml

val main : Core.Command.t

module For_testing : sig
  val run : Client.t -> unit Deferred.Or_error.t
end
