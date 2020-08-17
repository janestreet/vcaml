open! Core_kernel
open! Async

val with_client : f:(Vcaml.Client.t -> unit Deferred.Or_error.t) -> unit Deferred.t
val simple : 'a Or_error.t Vcaml.api_call -> ('a -> Sexplib0.Sexp.t) -> unit Deferred.t
