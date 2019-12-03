open Core

type t = Types.Tabpage.t

val of_msgpack : Msgpack.t -> t Or_error.t
val to_msgpack : t -> Msgpack.t

module Untested : sig
  val list_wins : tabpage:t -> Types.Window.t list Or_error.t Api_call.t
  val get_var : tabpage:t -> name:string -> Msgpack.t Or_error.t Api_call.t
  val set_var : tabpage:t -> name:string -> value:Msgpack.t -> unit Or_error.t Api_call.t
  val del_var : tabpage:t -> name:string -> unit Or_error.t Api_call.t
  val get_win : tabpage:t -> Types.Window.t Or_error.t Api_call.t
  val get_number : tabpage:t -> int Or_error.t Api_call.t
  val is_valid : tabpage:t -> bool Or_error.t Api_call.t
end
