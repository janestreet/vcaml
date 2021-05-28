module Unshadow_command := Command
open Core
module Command := Unshadow_command
module Type := Nvim_internal.Phantom

(* Neovim API functions that are not bound to a particular Neovim object. *)

val command : command:string -> unit Api_call.Or_error.t
val command_output : command:string -> string Api_call.Or_error.t
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

val replace_termcodes
  :  str:string
  -> from_part:bool
  -> do_lt:bool
  -> special:bool
  -> string Api_call.Or_error.t

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


(** These API functions are served immediately without waiting in the input queue. *)
module Fast : sig
  val get_mode : Mode.With_blocking_info.t Api_call.Or_error.t
  val input : keys:string -> int Api_call.Or_error.t
end

module Untested : sig
  val strwidth : text:string -> int Api_call.Or_error.t
  val list_runtime_paths : string list Api_call.Or_error.t
  val set_current_dir : dir:string -> unit Api_call.Or_error.t
  val get_current_line : string Api_call.Or_error.t
  val set_current_line : line:string -> unit Api_call.Or_error.t
  val del_current_line : unit Api_call.Or_error.t
  val del_var : name:string -> unit Api_call.Or_error.t
  val get_vvar : name:string -> type_:'a Type.t -> 'a Api_call.Or_error.t
  val get_option : name:string -> type_:'a Type.t -> 'a Api_call.Or_error.t
  val set_option : name:string -> type_:'a Type.t -> value:'a -> unit Api_call.Or_error.t
  val out_write : str:string -> unit Api_call.Or_error.t
  val err_write : str:string -> unit Api_call.Or_error.t
  val err_writeln : str:string -> unit Api_call.Or_error.t
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

  module Expert : sig
    val execute_lua : code:string -> args:Msgpack.t list -> Msgpack.t Api_call.Or_error.t
  end
end
