open! Core
open Async
open Vcaml

val main : Command.t

module For_testing : sig
  val run_plugin_for_testing
    :  client:Client.t
    -> terminate_var:unit Ivar.t
    -> time_source:Time_source.t
    -> (Buf.t * Window.t) Deferred.Or_error.t
end
