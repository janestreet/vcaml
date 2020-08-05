open Core

val command : command:string -> unit Or_error.t Api_call.t
val command_output : command:string -> string Or_error.t Api_call.t
val get_chan_info : chan:int -> Channel_info.t Or_error.t Api_call.t
val list_bufs : Buf.t list Or_error.t Api_call.t
val list_chans : Channel_info.t list Or_error.t Api_call.t
val get_current_buf : Buf.t Or_error.t Api_call.t
val set_current_buf : buffer:Buf.t -> unit Or_error.t Api_call.t
val get_current_win : Window.t Or_error.t Api_call.t
val set_current_win : window:Window.t -> unit Or_error.t Api_call.t
val list_wins : Window.t list Or_error.t Api_call.t

(**
   Calls many API methods atomically.

   This has two main usages:
   1. To perform several requests from an async context
   atomically, i.e. without interleaving redraws, RPC requests
   from other clients, or user interactions (however API
   methods may trigger autocommands or event processing which
   have such side-effects, e.g. |:sleep| may wake timers).
   2. To minimize RPC overhead (roundtrips) of a sequence of many
   requests.

   Parameters:
   {calls}  an array of calls, where each call is described
   by an array with two elements: the request name,
   and an array of arguments.

   Return:
   Array of two elements. The first is an array of return
   values. The second is NIL if all calls succeeded. If a
   call resulted in an error, it is a three-element array
   with the zero-based index of the call which resulted in an
   error, the error type and the error message. If an error
   occurred, the values from all preceding calls will still
   be returned.
*)
val call_atomic : calls:Msgpack.t list -> Msgpack.t list Or_error.t Api_call.t

val eval : expr:string -> Msgpack.t Or_error.t Api_call.t
val call_function : fn:string -> args:Msgpack.t list -> Msgpack.t Or_error.t Api_call.t

val feedkeys
  :  keys:string
  -> mode:string
  -> escape_csi:bool
  -> unit Or_error.t Api_call.t

val set_client_info
  :  ?version:Client_info.Version.t
  -> ?methods:Client_info.Client_method.t String.Map.t
  -> ?attributes:string String.Map.t
  -> name:string
  -> type_:Client_info.Client_type.t
  -> unit
  -> unit Or_error.t Api_call.t

module Untested : sig
  val ui_attach
    :  width:int
    -> height:int
    -> options:(Msgpack.t * Msgpack.t) list
    -> unit Or_error.t Api_call.t

  val ui_detach : unit Or_error.t Api_call.t
  val ui_try_resize : width:int -> height:int -> unit Or_error.t Api_call.t
  val ui_set_option : name:string -> value:Msgpack.t -> unit Or_error.t Api_call.t

  val get_hl_by_name
    :  name:string
    -> rgb:bool
    -> (Msgpack.t * Msgpack.t) list Or_error.t Api_call.t

  val get_hl_by_id
    :  hl_id:int
    -> rgb:bool
    -> (Msgpack.t * Msgpack.t) list Or_error.t Api_call.t

  val input : keys:string -> int Or_error.t Api_call.t

  val replace_termcodes
    :  str:string
    -> from_part:bool
    -> do_lt:bool
    -> special:bool
    -> string Or_error.t Api_call.t

  val execute_lua : code:string -> args:Msgpack.t list -> Msgpack.t Or_error.t Api_call.t

  val call_dict_function
    :  dict:Msgpack.t
    -> fn:string
    -> args:Msgpack.t list
    -> Msgpack.t Or_error.t Api_call.t

  val strwidth : text:string -> int Or_error.t Api_call.t
  val list_runtime_paths : string list Or_error.t Api_call.t
  val set_current_dir : dir:string -> unit Or_error.t Api_call.t
  val get_current_line : string Or_error.t Api_call.t
  val set_current_line : line:string -> unit Or_error.t Api_call.t
  val del_current_line : unit Or_error.t Api_call.t
  val get_var : name:string -> Msgpack.t Or_error.t Api_call.t
  val set_var : name:string -> value:Msgpack.t -> unit Or_error.t Api_call.t
  val del_var : name:string -> unit Or_error.t Api_call.t
  val get_vvar : name:string -> Msgpack.t Or_error.t Api_call.t
  val get_option : name:string -> Msgpack.t Or_error.t Api_call.t
  val set_option : name:string -> value:Msgpack.t -> unit Or_error.t Api_call.t
  val out_write : str:string -> unit Or_error.t Api_call.t
  val err_write : str:string -> unit Or_error.t Api_call.t
  val err_writeln : str:string -> unit Or_error.t Api_call.t
  val list_tabpages : Tabpage.t list Or_error.t Api_call.t
  val get_current_tabpage : Tabpage.t Or_error.t Api_call.t
  val set_current_tabpage : tabpage:Tabpage.t -> unit Or_error.t Api_call.t
  val subscribe : event:string -> unit Or_error.t Api_call.t
  val unsubscribe : event:string -> unit Or_error.t Api_call.t
  val get_color_by_name : name:string -> int Or_error.t Api_call.t
  val get_color_map : (Msgpack.t * Msgpack.t) list Or_error.t Api_call.t
  val get_mode : (Msgpack.t * Msgpack.t) list Or_error.t Api_call.t
  val get_keymap : mode:string -> Keymap.t list Or_error.t Api_call.t

  val get_commands
    :  opts:(Msgpack.t * Msgpack.t) list
    -> Nvim_command.t String.Map.t Or_error.t Api_call.t

  val get_api_info : Msgpack.t list Or_error.t Api_call.t

  val parse_expression
    :  expr:string
    -> flags:string
    -> highlight:bool
    -> (Msgpack.t * Msgpack.t) list Or_error.t Api_call.t

  val list_uis : Msgpack.t list Or_error.t Api_call.t
  val get_proc_children : pid:int -> Msgpack.t list Or_error.t Api_call.t
  val get_proc : pid:int -> Msgpack.t Or_error.t Api_call.t

  val keymap
    :  lhs:string
    -> rhs:string
    -> mode:string
    -> opts:bool String.Map.t
    -> unit Or_error.t Api_call.t
end
