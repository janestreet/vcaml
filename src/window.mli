open Core

type t = Types.Window.t [@@deriving sexp]

type position =
  { row : int
  ; col : int
  }

val of_msgpack : Msgpack.t -> t Or_error.t
val to_msgpack : t -> Msgpack.t
val get_height : window:t -> int Or_error.t Api_call.t
val set_height : window:t -> height:int -> unit Or_error.t Api_call.t

module Untested : sig
  val get_buf : window:t -> Buf.t Or_error.t Api_call.t
  val get_cursor : window:t -> position Or_error.t Api_call.t
  val set_cursor : window:t -> row:int -> col:int -> unit Or_error.t Api_call.t
  val get_width : window:t -> int Or_error.t Api_call.t
  val set_width : window:t -> width:int -> unit Or_error.t Api_call.t
  val get_var : window:t -> name:string -> Msgpack.t Or_error.t Api_call.t
  val set_var : window:t -> name:string -> value:Msgpack.t -> unit Or_error.t Api_call.t
  val del_var : window:t -> name:string -> unit Or_error.t Api_call.t
  val get_option : window:t -> name:string -> Msgpack.t Or_error.t Api_call.t

  val set_option
    :  window:t
    -> name:string
    -> value:Msgpack.t
    -> unit Or_error.t Api_call.t

  val get_position : window:t -> position Or_error.t Api_call.t
  val get_tabpage : window:t -> Types.Tabpage.t Or_error.t Api_call.t
  val get_number : window:t -> int Or_error.t Api_call.t
  val is_valid : window:t -> bool Or_error.t Api_call.t
end
