open Core

val convert_msgpack_error : ('a, Msgpack.t) Result.t -> 'a Or_error.t
val value : ?err_msg:string -> 'a Nvim_internal.Phantom.t -> Msgpack.t -> 'a Or_error.t
val inject : 'a Nvim_internal.Phantom.t -> 'a -> Msgpack.t
val string : ?err_msg:string -> Msgpack.t -> string Or_error.t
val int : ?err_msg:string -> Msgpack.t -> int Or_error.t
val bool : ?err_msg:string -> Msgpack.t -> bool Or_error.t
val map_of_msgpack_map : Msgpack.t -> Msgpack.t String.Map.t Or_error.t

val and_convert_optional
  :  Msgpack.t String.Map.t
  -> string
  -> (Msgpack.t -> 'a Or_error.t)
  -> 'a option Or_error.t
