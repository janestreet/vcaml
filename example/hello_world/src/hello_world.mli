open! Core
open! Async
open Vcaml

val main : Core.Command.t

module For_testing : sig
  val echo_hello_world : [ `connected ] Client.t -> unit Deferred.Or_error.t
end
