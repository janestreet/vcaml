open Core

val convert_msgpack_error
  :  ('a, Msgpack.t) Result.t
  -> on_keyboard_interrupt:(unit -> unit)
  -> 'a Or_error.t

val value : 'a Nvim_internal.Phantom.t -> Msgpack.t -> 'a Or_error.t
val inject : 'a Nvim_internal.Phantom.t -> 'a -> Msgpack.t
val string : Msgpack.t -> string Or_error.t
val int : Msgpack.t -> int Or_error.t
val bool : Msgpack.t -> bool Or_error.t
val float : Msgpack.t -> float Or_error.t
val map_of_msgpack_map : Msgpack.t -> Msgpack.t String.Map.t Or_error.t
val map_to_msgpack_map : Msgpack.t String.Map.t -> Msgpack.t

val map_of_msgpack_alist
  :  (Msgpack.t * Msgpack.t) list
  -> Msgpack.t String.Map.t Or_error.t

val map_to_msgpack_alist : Msgpack.t String.Map.t -> (Msgpack.t * Msgpack.t) list

val and_convert_optional
  :  Msgpack.t String.Map.t
  -> string
  -> (Msgpack.t -> 'a Or_error.t)
  -> 'a option Or_error.t
