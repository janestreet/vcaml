open! Core
open Async
module Error_type := Nvim_internal.Error_type

val attach
  :  Msgpack_rpc.t
  -> on_error:(Error.t -> unit)
  -> on_error_event:(Error_type.t -> message:string -> unit)
  -> time_source:Time_source.t
  -> Client.t Deferred.Or_error.t
