open Core
module Type := Nvim_internal.Phantom

include module type of struct
  include Nvim_internal.Window
end

val get_height : window:t -> int Api_call.Or_error.t
val set_height : window:t -> height:int -> unit Api_call.Or_error.t
val get_cursor : window:t -> Position.One_indexed_row.t Api_call.Or_error.t
val set_cursor : window:t -> Position.One_indexed_row.t -> unit Api_call.Or_error.t

module Untested : sig
  val open_
    :  buffer:Nvim_internal.Buffer.t
    -> enter:bool
    -> config:Msgpack.t String.Map.t
    -> t Api_call.Or_error.t

  module When_this_is_the_buffer's_last_window : sig
    (** When [Unload { if_modified = `Abort }] is specified, the buffer will be hidden
        anyway if ['hidden'] is set, if ['bufhidden'] is ['hide'], or if the [:hide]
        command modifier is used. If ['confirm'] is set or the [:confirm] command modifier
        is used, the user will be prompted to see if they want to hide the buffer instead
        of failing. *)
    type t =
      | Hide
      | Unload of { if_modified : [ `Hide | `Abort ] }
  end

  (** Because closing the window in this way does not involve a cursor move, no WinLeave
      event will be triggered. *)
  val close
    :  window:t
    -> when_this_is_the_buffer's_last_window:When_this_is_the_buffer's_last_window.t
    -> unit Api_call.Or_error.t

  val get_config : window:t -> Msgpack.t String.Map.t Api_call.Or_error.t
  val set_config : window:t -> config:Msgpack.t String.Map.t -> unit Api_call.Or_error.t
  val get_buf : window:t -> Nvim_internal.Buffer.t Api_call.Or_error.t
  val get_width : window:t -> int Api_call.Or_error.t
  val set_width : window:t -> width:int -> unit Api_call.Or_error.t
  val get_var : window:t -> name:string -> type_:'a Type.t -> 'a Api_call.Or_error.t

  val set_var
    :  window:t
    -> name:string
    -> type_:'a Type.t
    -> value:'a
    -> unit Api_call.Or_error.t

  val del_var : window:t -> name:string -> unit Api_call.Or_error.t
  val get_option : window:t -> name:string -> type_:'a Type.t -> 'a Api_call.Or_error.t

  val set_option
    :  window:t
    -> scope:[ `Local | `Global ]
    -> name:string
    -> type_:'a Type.t
    -> value:'a
    -> unit Api_call.Or_error.t

  val get_position : window:t -> Position.t Api_call.Or_error.t
  val get_tabpage : window:t -> Nvim_internal.Tabpage.t Api_call.Or_error.t
  val get_number : window:t -> int Api_call.Or_error.t
  val is_valid : window:t -> bool Api_call.Or_error.t

  module Expert : sig

    (** This is a low-level function that sets a window's buffer without any side effects.
        The cursor is not moved and no autocommands are triggered. This means you are
        bypassing the user's intent to run certain logic when certain events happen. You
        should be very sure this is what you want before calling this function, and you
        should likely only ever invoke it on buffers / windows that your plugin owns. *)
    val set_buf : window:t -> buffer:Nvim_internal.Buffer.t -> unit Api_call.Or_error.t


    (** Call a lua function from the given window. If this is actually useful we can
        create a wrapper similar to [wrap_viml_function] and expose it in a more type-safe
        way. *)
    val win_call
      :  window:t
      -> lua_callback:Nvim_internal.Luaref.t
      -> Msgpack.t Api_call.Or_error.t
  end
end
