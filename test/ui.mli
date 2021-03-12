open! Core_kernel
open! Async
open! Import
open! Vcaml

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