open! Core
open! Async

val with_client
  :  ?on_error:(Error.t -> unit)
  -> (Vcaml.Client.t -> 'a Deferred.Or_error.t)
  -> 'a Deferred.t

val simple : 'a Vcaml.Api_call.Or_error.t -> ('a -> Sexp.t) -> unit Deferred.t

val get_screen_contents
  :  ?width:int
  -> ?height:int
  -> Vcaml.Client.t
  -> string Async.Deferred.Or_error.t

val wait_until_text
  :  ?timeout:Time_ns.Span.t
  -> Vcaml.Client.t
  -> f:(string -> bool)
  -> string Or_error.t Deferred.t
