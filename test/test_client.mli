open! Core_kernel
open! Async

val with_client : (Vcaml.Client.t -> 'a Deferred.Or_error.t) -> 'a Deferred.t
val simple : 'a Or_error.t Vcaml.api_call -> ('a -> Sexplib0.Sexp.t) -> unit Deferred.t
