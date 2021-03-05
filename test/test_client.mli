open! Core_kernel
open! Async

val with_client : (Vcaml.Client.t -> 'a Deferred.Or_error.t) -> 'a Deferred.t
val simple : 'a Vcaml.Api_call.Or_error.t -> ('a -> Sexp.t) -> unit Deferred.t
