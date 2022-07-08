module Unshadow_buffer := Buffer
module Unshadow_command := Command
open Core
module Buffer := Unshadow_buffer
module Command := Unshadow_command
module Type := Nvim_internal.Phantom
module Luaref := Nvim_internal.Luaref

(* Neovim API functions that are not bound to a particular Neovim object. *)

val command : string -> unit Api_call.Or_error.t
val source : string -> string Api_call.Or_error.t
val get_channel_info : int -> Channel_info.t Api_call.Or_error.t
val list_bufs : Buffer.t list Api_call.Or_error.t
val channels : Channel_info.t list Api_call.Or_error.t
val get_current_buf : Buffer.t Api_call.Or_error.t
val set_current_buf : Buffer.t -> unit Api_call.Or_error.t
val get_current_win : Window.t Api_call.Or_error.t
val set_current_win : Window.t -> unit Api_call.Or_error.t
val list_wins : Window.t list Api_call.Or_error.t
val eval : string -> result_type:'a Type.t -> 'a Api_call.Or_error.t

type keys_with_replaced_keycodes = private string

val replace_termcodes_only : string -> string Api_call.Or_error.t

val replace_termcodes_and_keycodes
  :  string
  -> keys_with_replaced_keycodes Api_call.Or_error.t

val feedkeys
  :  [ `Escape_k_special_bytes of string
     | `Already_escaped of keys_with_replaced_keycodes
     ]
  -> mode:string
  -> unit Api_call.Or_error.t

val set_client_info
  :  ?version:Client_info.Version.t
  -> ?methods:Client_info.Client_method.t String.Map.t
  -> ?attributes:string String.Map.t
  -> name:string
  -> type_:Client_info.Client_type.t
  -> unit
  -> unit Api_call.Or_error.t

val get_color_map : Color.True_color.t String.Map.t Api_call.Or_error.t
val get_color_by_name : string -> Color.True_color.t Api_call.Or_error.t

val get_hl_by_name
  :  string
  -> color:'a Color.Kind.t
  -> 'a Color.Highlight.t Api_call.Or_error.t

val get_hl_by_id
  :  int
  -> color:'a Color.Kind.t
  -> 'a Color.Highlight.t Api_call.Or_error.t

val get_var : string -> type_:'a Type.t -> 'a Api_call.Or_error.t
val set_var : string -> type_:'a Type.t -> value:'a -> unit Api_call.Or_error.t
val list_runtime_paths : string list Api_call.Or_error.t
val out_write : string -> unit Api_call.Or_error.t
val out_writeln : string -> unit Api_call.Or_error.t
val err_write : string -> unit Api_call.Or_error.t
val err_writeln : string -> unit Api_call.Or_error.t
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

  val input : string -> int Api_call.Or_error.t

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
  val get_display_width : string -> int Api_call.Or_error.t
  val set_current_dir : string -> unit Api_call.Or_error.t
  val get_current_line : string Api_call.Or_error.t
  val set_current_line : string -> unit Api_call.Or_error.t
  val delete_current_line : unit Api_call.Or_error.t
  val delete_var : string -> unit Api_call.Or_error.t
  val get_vvar : string -> type_:'a Type.t -> 'a Api_call.Or_error.t
  val set_vvar : string -> type_:'a Type.t -> value:'a -> unit Api_call.Or_error.t
  val get_option : string -> type_:'a Type.t -> 'a Api_call.Or_error.t
  val set_option : string -> type_:'a Type.t -> value:'a -> unit Api_call.Or_error.t
  val list_tabpages : Tabpage.t list Api_call.Or_error.t
  val get_current_tabpage : Tabpage.t Api_call.Or_error.t
  val set_current_tabpage : Tabpage.t -> unit Api_call.Or_error.t

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
  val get_hl_id_by_name : string -> int Api_call.Or_error.t

  val nvim_find_runtime_file_matching
    :  pattern:string
    -> string option Api_call.Or_error.t

  val nvim_all_runtime_files_matching : pattern:string -> string list Api_call.Or_error.t
  val get_option_info : string -> Option_info.packed Api_call.Or_error.t
  val get_all_options_info : Option_info.packed String.Map.t Api_call.Or_error.t

  (** Send raw bytes to channel. If the channel is a terminal and you want to display
      multiple lines you will need to follow your newlines with carriage returns. See also
      [Buffer.Untested.open_term]. *)
  val send_to_channel : channel:int -> string -> unit Api_call.Or_error.t

  val get_mark
    :  char
    -> (Buffer.t * [ `Buffer_name of string ] * Mark.t) Api_call.Or_error.t

  val delete_mark : char -> unit Or_error.t Api_call.t

  val eval_statusline
    :  ?window:Window.t
    -> ?max_width:int
    -> ?fill_char:char
    -> include_highlights:bool
    -> string
    -> Msgpack.t String.Map.t Api_call.Or_error.t

  val eval_tabline
    :  ?max_width:int
    -> ?fill_char:char
    -> include_highlights:bool
    -> string
    -> Msgpack.t String.Map.t Api_call.Or_error.t

  module Expert : sig
    val execute_lua : string -> args:Msgpack.t list -> Msgpack.t Api_call.Or_error.t

    val set_decoration_provider
      :  namespace:
           Namespace.t
      -> ?on_start:Luaref.t
      -> ?on_buf:Luaref.t
      -> ?on_win:Luaref.t
      -> ?on_line:Luaref.t
      -> ?on_end:Luaref.t
      -> unit
      -> unit Api_call.Or_error.t
  end
end
