module Unshadow_command := Command
open Core
module Command := Unshadow_command
module Type := Nvim_internal.Phantom

(* Neovim API functions that are not bound to a particular Neovim object. *)

val command : command:string -> unit Api_call.Or_error.t
val source : code:string -> string Api_call.Or_error.t
val get_chan_info : chan:int -> Channel_info.t Api_call.Or_error.t
val list_bufs : Nvim_internal.Buffer.t list Api_call.Or_error.t
val list_chans : Channel_info.t list Api_call.Or_error.t
val get_current_buf : Nvim_internal.Buffer.t Api_call.Or_error.t
val set_current_buf : buffer:Nvim_internal.Buffer.t -> unit Api_call.Or_error.t
val get_current_win : Window.t Api_call.Or_error.t
val set_current_win : window:Window.t -> unit Api_call.Or_error.t
val list_wins : Window.t list Api_call.Or_error.t
val eval : expr:string -> result_type:'a Nvim_internal.Phantom.t -> 'a Api_call.Or_error.t

val feedkeys : keys:string -> mode:string -> escape_csi:bool -> unit Api_call.Or_error.t

val set_client_info
  :  ?version:Client_info.Version.t
  -> ?methods:Client_info.Client_method.t String.Map.t
  -> ?attributes:string String.Map.t
  -> name:string
  -> type_:Client_info.Client_type.t
  -> unit
  -> unit Api_call.Or_error.t

val replace_termcodes : str:string -> replace_keycodes:bool -> string Api_call.Or_error.t
val get_color_map : Color.True_color.t String.Map.t Api_call.Or_error.t
val get_color_by_name : name:string -> Color.True_color.t Api_call.Or_error.t

val get_hl_by_name
  :  name:string
  -> color:'a Color.Kind.t
  -> 'a Color.Highlight.t Api_call.Or_error.t

val get_hl_by_id
  :  hl_id:int
  -> color:'a Color.Kind.t
  -> 'a Color.Highlight.t Api_call.Or_error.t

val get_var : name:string -> type_:'a Type.t -> 'a Api_call.Or_error.t
val set_var : name:string -> type_:'a Type.t -> value:'a -> unit Api_call.Or_error.t
val list_runtime_paths : string list Api_call.Or_error.t
val out_write : str:string -> unit Api_call.Or_error.t
val out_writeln : str:string -> unit Api_call.Or_error.t
val err_write : str:string -> unit Api_call.Or_error.t
val err_writeln : str:string -> unit Api_call.Or_error.t
val echo : Highlighted_text.t -> add_to_history:bool -> unit Api_call.Or_error.t

(** As of this writing messages echoed during an [rpcrequest] are not displayed until the
    request completes. This function hacks around that limitation. A side-effect of this
    hack is that if the user mashes the keyboard during the [rpcrequest] those keys will
    be printed after the message. [inputsave] + [inputrestore] does not seem to help
    mitigate this. For more details about this echoing limitation, see
    https://github.com/neovim/neovim/issues/14449.

    Note that [err_write] and [err_writeln] do work inside [rpcrequest] but the error
    message is treated as an exception, so they aren't suitable for regular echoing
    purposes. *)
val echo_in_rpcrequest : string -> unit Api_call.Or_error.t

module Mouse : sig
  module Button : sig
    type t =
      | Left
      | Right
      | Middle
      | Wheel
    [@@deriving sexp_of]
  end

  module Action : sig
    type t =
      | Press
      | Drag
      | Release
      | Wheel_up
      | Wheel_down
      | Wheel_left
      | Wheel_right
    [@@deriving sexp_of]
  end
end

module Key_modifier : sig
  type t =
    | Shift
    | Ctrl
    | Alt
    | Super
  [@@deriving compare, sexp_of]

  include Comparable.S_plain with type t := t
end


(** These API functions are served immediately without waiting in the input queue. *)
module Fast : sig
  (** There is a Neovim bug where calling [get_mode] during an [rpcrequest] will hang.
      See https://github.com/neovim/neovim/issues/14451. *)
  val get_mode : Mode.With_blocking_info.t Api_call.Or_error.t

  val input : keys:string -> int Api_call.Or_error.t

  module Untested : sig
    val input_mouse
      :  button:Mouse.Button.t
      -> action:Mouse.Action.t
      -> modifiers:Key_modifier.Set.t
      -> grid:int
      -> row:int
      -> col:int
      -> unit Api_call.Or_error.t
  end

  val paste : string list -> unit Api_call.Or_error.t

  (** Only one paste stream should be open for a client at a time. The returned deferred
      is filled after the writer is closed and the paste stream has flushed to Neovim. *)
  val paste_stream
    :  Source_code_position.t
    -> [ `connected ] Client.t
    -> string Async.Pipe.Writer.t * unit Async.Deferred.Or_error.t
end

module Untested : sig
  module Log_level : sig
    type t =
      | Trace
      | Debug
      | Info
      | Warn
      | Error
  end

  val notify : Log_level.t -> string -> unit Api_call.Or_error.t
  val get_display_width : text:string -> int Api_call.Or_error.t
  val set_current_dir : dir:string -> unit Api_call.Or_error.t
  val get_current_line : string Api_call.Or_error.t
  val set_current_line : line:string -> unit Api_call.Or_error.t
  val del_current_line : unit Api_call.Or_error.t
  val del_var : name:string -> unit Api_call.Or_error.t
  val get_vvar : name:string -> type_:'a Type.t -> 'a Api_call.Or_error.t
  val set_vvar : name:string -> type_:'a Type.t -> value:'a -> unit Api_call.Or_error.t
  val get_option : name:string -> type_:'a Type.t -> 'a Api_call.Or_error.t
  val set_option : name:string -> type_:'a Type.t -> value:'a -> unit Api_call.Or_error.t
  val list_tabpages : Tabpage.t list Api_call.Or_error.t
  val get_current_tabpage : Tabpage.t Api_call.Or_error.t
  val set_current_tabpage : tabpage:Tabpage.t -> unit Api_call.Or_error.t

  (** Neovim supports both signal and broadcast asynchronous notifications. In signal mode
      a channel is given to [rpcnotify] and the channel will receive the event regardless
      its subscriptions. In broadcast mode (when the channel argument is 0) all channels
      that have subscribed to [event] will receive the notification. *)
  val subscribe : event:string -> unit Api_call.Or_error.t

  val unsubscribe : event:string -> unit Api_call.Or_error.t
  val get_user_defined_commands : Command.t String.Map.t Api_call.Or_error.t

  val put
    :  string list
    -> how:[ `Blockwise | `Linewise | `Charwise ]
    -> where:[ `Before_cursor | `After_cursor ]
    -> place_cursor:[ `At_start_of_text | `At_end_of_text ]
    -> unit Api_call.Or_error.t

  val get_context
    :  opts:Msgpack.t String.Map.t
    -> Msgpack.t String.Map.t Api_call.Or_error.t

  val load_context : dict:Msgpack.t String.Map.t -> Msgpack.t Api_call.Or_error.t
  val get_hl_id_by_name : name:string -> int Api_call.Or_error.t

  val nvim_find_runtime_file_matching
    :  pattern:string
    -> string option Api_call.Or_error.t

  val nvim_all_runtime_files_matching : pattern:string -> string list Api_call.Or_error.t
  val get_option_info : name:string -> Option_info.packed Api_call.Or_error.t
  val get_all_options_info : Option_info.packed String.Map.t Api_call.Or_error.t

  (** Send raw bytes to channel. If the channel is a terminal and you want to display
      multiple lines you will need to follow your newlines with carriage returns. See also
      [Buffer.Untested.open_term]. *)
  val chan_send : channel_id:int -> string -> unit Api_call.Or_error.t

  module Expert : sig
    val execute_lua : code:string -> args:Msgpack.t list -> Msgpack.t Api_call.Or_error.t

    val set_decoration_provider
      :  namespace:
           Namespace.t
      -> ?on_start:Nvim_internal.Luaref.t
      -> ?on_buf:Nvim_internal.Luaref.t
      -> ?on_win:Nvim_internal.Luaref.t
      -> ?on_line:Nvim_internal.Luaref.t
      -> ?on_end:Nvim_internal.Luaref.t
      -> unit
      -> unit Api_call.Or_error.t
  end
end
