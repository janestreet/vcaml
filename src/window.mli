include module type of struct
  include Types.Window
end

type position =
  { row : int
  ; col : int
  }

val get_height : window:t -> int Api_call.Or_error.t
val set_height : window:t -> height:int -> unit Api_call.Or_error.t
val get_cursor : window:t -> position Api_call.Or_error.t
val set_cursor : window:t -> row:int -> col:int -> unit Api_call.Or_error.t

module Untested : sig
  val get_buf : window:t -> Types.Buffer.t Api_call.Or_error.t
  val get_width : window:t -> int Api_call.Or_error.t
  val set_width : window:t -> width:int -> unit Api_call.Or_error.t
  val get_var : window:t -> name:string -> Msgpack.t Api_call.Or_error.t
  val set_var : window:t -> name:string -> value:Msgpack.t -> unit Api_call.Or_error.t
  val del_var : window:t -> name:string -> unit Api_call.Or_error.t
  val get_option : window:t -> name:string -> Msgpack.t Api_call.Or_error.t
  val set_option : window:t -> name:string -> value:Msgpack.t -> unit Api_call.Or_error.t
  val get_position : window:t -> position Api_call.Or_error.t
  val get_tabpage : window:t -> Types.Tabpage.t Api_call.Or_error.t
  val get_number : window:t -> int Api_call.Or_error.t
  val is_valid : window:t -> bool Api_call.Or_error.t
end
