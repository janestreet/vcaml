open! Core
open! Async
open! Vcaml

val main : Command.t

module For_testing : sig
  val next : Client.t -> unit Deferred.Or_error.t
  val prev : Client.t -> unit Deferred.Or_error.t
  val list_raw : Client.t -> unit Deferred.Or_error.t
  val list_fzf : Client.t -> unit Deferred.Or_error.t
end
