open! Core
open Runtime

module Api_version : sig
  type t =
    { api_compatible : int
    ; api_level : int
    ; major : int
    ; minor : int
    ; patch : int
    }
  [@@deriving sexp_of]

  val to_string : t -> string
end

val api_version : Api_version.t

module Ui_options : sig
  type t =
    { ext_cmdline : bool
    ; ext_hlstate : bool
    ; ext_linegrid : bool
    ; ext_messages : bool
    ; ext_multigrid : bool
    ; ext_popupmenu : bool
    ; ext_tabline : bool
    ; ext_termcolors : bool
    ; ext_wildmenu : bool
    ; rgb : bool
    }
  [@@deriving fields ~iterators:(make_creator, to_list), sexp_of]

  val empty : t
end

type buffer = private int
type window = private int
type tabpage = private int

(** Pretty-print MessagePack messages with Neovim extensions. *)
val pp : Formatter.t -> Msgpack.t -> unit

module Phantom : sig
  (** For use in pattern matching. For example, if you have a ['a Phantom.t * 'a], if a
      pattern match on the phantom succeeds on [Int], then ['a] unifies with [int]. Use
      [Custom] for serialization & deserialization of custom types. *)
  type _ t =
    | Nil : unit t
    | Int : int t
    | Bool : bool t
    | Float : float t
    | Array : 'a t -> 'a list t
    | Tuple2 : 'a t * 'b t -> ('a * 'b) t
    | Dict : Msgpack.t String.Map.t t
    | String : string t
    | Buffer : buffer t
    | Window : window t
    | Tabpage : tabpage t
    | Object : Msgpack.t t
    | Custom : (module Msgpack.Msgpackable with type t = 'a) -> 'a t
  [@@deriving sexp_of]

  val of_msgpack : 'a t -> Msgpack.t -> 'a Or_error.t
  val to_msgpack : 'a t -> 'a -> Msgpack.t
end

module Buffer : Nvim_id with type t = buffer and type 'a phantom := 'a Phantom.t
module Window : Nvim_id with type t = window and type 'a phantom := 'a Phantom.t
module Tabpage : Nvim_id with type t = tabpage and type 'a phantom := 'a Phantom.t

module Api_result : sig
  type 'result t =
    { name : string
    ; params : Msgpack.t list
    ; witness : 'result Phantom.t
    }
  [@@deriving sexp_of]
end

module Error_type : sig
  type t =
    | Exception
    | Validation
    | Unknown of int
  [@@deriving equal, sexp]

  val of_int : int -> t
end

module Ui_event : sig
  (** See `:h ui.txt` for details about these events. To be forwards-compatible with more
      recent versions of Neovim than the version against which VCaml was tested, unknown
      events are modeled with [Unknown_event] and fields added to known events are modeled
      with [unparsed_fields]. See `:h api-contract` for compatibility details. *)
  type t =
    | Mode_info_set of
        { enabled : bool
        ; cursor_styles : Msgpack.t list
        ; unparsed_fields : Msgpack.t list
        }
    | Update_menu of { unparsed_fields : Msgpack.t list }
    | Busy_start of { unparsed_fields : Msgpack.t list }
    | Busy_stop of { unparsed_fields : Msgpack.t list }
    | Mouse_on of { unparsed_fields : Msgpack.t list }
    | Mouse_off of { unparsed_fields : Msgpack.t list }
    | Mode_change of
        { mode : string
        ; mode_idx : int
        ; unparsed_fields : Msgpack.t list
        }
    | Bell of { unparsed_fields : Msgpack.t list }
    | Visual_bell of { unparsed_fields : Msgpack.t list }
    | Flush of { unparsed_fields : Msgpack.t list }
    | Suspend of { unparsed_fields : Msgpack.t list }
    | Set_title of
        { title : string
        ; unparsed_fields : Msgpack.t list
        }
    | Set_icon of
        { icon : string
        ; unparsed_fields : Msgpack.t list
        }
    | Screenshot of
        { path : string
        ; unparsed_fields : Msgpack.t list
        }
    | Option_set of
        { name : string
        ; value : Msgpack.t
        ; unparsed_fields : Msgpack.t list
        }
    | Update_fg of
        { fg : int
        ; unparsed_fields : Msgpack.t list
        }
    | Update_bg of
        { bg : int
        ; unparsed_fields : Msgpack.t list
        }
    | Update_sp of
        { sp : int
        ; unparsed_fields : Msgpack.t list
        }
    | Resize of
        { width : int
        ; height : int
        ; unparsed_fields : Msgpack.t list
        }
    | Clear of { unparsed_fields : Msgpack.t list }
    | Eol_clear of { unparsed_fields : Msgpack.t list }
    | Cursor_goto of
        { row : int
        ; col : int
        ; unparsed_fields : Msgpack.t list
        }
    | Highlight_set of
        { attrs : Msgpack.t String.Map.t
        ; unparsed_fields : Msgpack.t list
        }
    | Put of
        { str : string
        ; unparsed_fields : Msgpack.t list
        }
    | Set_scroll_region of
        { top : int
        ; bot : int
        ; left : int
        ; right : int
        ; unparsed_fields : Msgpack.t list
        }
    | Scroll of
        { count : int
        ; unparsed_fields : Msgpack.t list
        }
    | Default_colors_set of
        { rgb_fg : int
        ; rgb_bg : int
        ; rgb_sp : int
        ; cterm_fg : int
        ; cterm_bg : int
        ; unparsed_fields : Msgpack.t list
        }
    | Hl_attr_define of
        { id : int
        ; rgb_attrs : Msgpack.t String.Map.t
        ; cterm_attrs : Msgpack.t String.Map.t
        ; info : Msgpack.t list
        ; unparsed_fields : Msgpack.t list
        }
    | Hl_group_set of
        { name : string
        ; id : int
        ; unparsed_fields : Msgpack.t list
        }
    | Grid_resize of
        { grid : int
        ; width : int
        ; height : int
        ; unparsed_fields : Msgpack.t list
        }
    | Grid_clear of
        { grid : int
        ; unparsed_fields : Msgpack.t list
        }
    | Grid_cursor_goto of
        { grid : int
        ; row : int
        ; col : int
        ; unparsed_fields : Msgpack.t list
        }
    | Grid_line of
        { grid : int
        ; row : int
        ; col_start : int
        ; data : Msgpack.t list
        ; unparsed_fields : Msgpack.t list
        }
    | Grid_scroll of
        { grid : int
        ; top : int
        ; bot : int
        ; left : int
        ; right : int
        ; rows : int
        ; cols : int
        ; unparsed_fields : Msgpack.t list
        }
    | Grid_destroy of
        { grid : int
        ; unparsed_fields : Msgpack.t list
        }
    | Win_pos of
        { grid : int
        ; win : Window.t
        ; startrow : int
        ; startcol : int
        ; width : int
        ; height : int
        ; unparsed_fields : Msgpack.t list
        }
    | Win_float_pos of
        { grid : int
        ; win : Window.t
        ; anchor : string
        ; anchor_grid : int
        ; anchor_row : float
        ; anchor_col : float
        ; focusable : bool
        ; zindex : int
        ; unparsed_fields : Msgpack.t list
        }
    | Win_external_pos of
        { grid : int
        ; win : Window.t
        ; unparsed_fields : Msgpack.t list
        }
    | Win_hide of
        { grid : int
        ; unparsed_fields : Msgpack.t list
        }
    | Win_close of
        { grid : int
        ; unparsed_fields : Msgpack.t list
        }
    | Msg_set_pos of
        { grid : int
        ; row : int
        ; scrolled : bool
        ; sep_char : string
        ; unparsed_fields : Msgpack.t list
        }
    | Win_viewport of
        { grid : int
        ; win : Window.t
        ; topline : int
        ; botline : int
        ; curline : int
        ; curcol : int
        ; line_count : int
        ; scroll_delta : int
        ; unparsed_fields : Msgpack.t list
        }
    | Win_extmark of
        { grid : int
        ; win : Window.t
        ; ns_id : int
        ; mark_id : int
        ; row : int
        ; col : int
        ; unparsed_fields : Msgpack.t list
        }
    | Popupmenu_show of
        { items : Msgpack.t list
        ; selected : int
        ; row : int
        ; col : int
        ; grid : int
        ; unparsed_fields : Msgpack.t list
        }
    | Popupmenu_hide of { unparsed_fields : Msgpack.t list }
    | Popupmenu_select of
        { selected : int
        ; unparsed_fields : Msgpack.t list
        }
    | Tabline_update of
        { current : Tabpage.t
        ; tabs : Msgpack.t list
        ; current_buffer : Buffer.t
        ; buffers : Msgpack.t list
        ; unparsed_fields : Msgpack.t list
        }
    | Cmdline_show of
        { content : Msgpack.t list
        ; pos : int
        ; firstc : string
        ; prompt : string
        ; indent : int
        ; level : int
        ; unparsed_fields : Msgpack.t list
        }
    | Cmdline_pos of
        { pos : int
        ; level : int
        ; unparsed_fields : Msgpack.t list
        }
    | Cmdline_special_char of
        { c : string
        ; shift : bool
        ; level : int
        ; unparsed_fields : Msgpack.t list
        }
    | Cmdline_hide of
        { level : int
        ; unparsed_fields : Msgpack.t list
        }
    | Cmdline_block_show of
        { lines : Msgpack.t list
        ; unparsed_fields : Msgpack.t list
        }
    | Cmdline_block_append of
        { lines : Msgpack.t list
        ; unparsed_fields : Msgpack.t list
        }
    | Cmdline_block_hide of { unparsed_fields : Msgpack.t list }
    | Wildmenu_show of
        { items : Msgpack.t list
        ; unparsed_fields : Msgpack.t list
        }
    | Wildmenu_select of
        { selected : int
        ; unparsed_fields : Msgpack.t list
        }
    | Wildmenu_hide of { unparsed_fields : Msgpack.t list }
    | Msg_show of
        { kind : string
        ; content : Msgpack.t list
        ; replace_last : bool
        ; unparsed_fields : Msgpack.t list
        }
    | Msg_clear of { unparsed_fields : Msgpack.t list }
    | Msg_showcmd of
        { content : Msgpack.t list
        ; unparsed_fields : Msgpack.t list
        }
    | Msg_showmode of
        { content : Msgpack.t list
        ; unparsed_fields : Msgpack.t list
        }
    | Msg_ruler of
        { content : Msgpack.t list
        ; unparsed_fields : Msgpack.t list
        }
    | Msg_history_show of
        { entries : Msgpack.t list
        ; unparsed_fields : Msgpack.t list
        }
    | Msg_history_clear of { unparsed_fields : Msgpack.t list }
    | Unknown_event of
        { name : string
        ; unparsed_fields : Msgpack.t list
        }
  [@@deriving sexp_of]

  val of_msgpack : Msgpack.t -> t list Or_error.t
end

val nvim_get_autocmds : opts:Msgpack.t String.Map.t -> Msgpack.t list Api_result.t

val nvim_create_autocmd
  :  event:Msgpack.t
  -> opts:Msgpack.t String.Map.t
  -> int Api_result.t

val nvim_del_autocmd : id:int -> unit Api_result.t
val nvim_clear_autocmds : opts:Msgpack.t String.Map.t -> unit Api_result.t
val nvim_create_augroup : name:string -> opts:Msgpack.t String.Map.t -> int Api_result.t
val nvim_del_augroup_by_id : id:int -> unit Api_result.t
val nvim_del_augroup_by_name : name:string -> unit Api_result.t

val nvim_exec_autocmds
  :  event:Msgpack.t
  -> opts:Msgpack.t String.Map.t
  -> unit Api_result.t

val nvim_buf_line_count : buffer:Buffer.Or_current.t -> int Api_result.t

val nvim_buf_attach
  :  buffer:Buffer.Or_current.t
  -> send_buffer:bool
  -> opts:Msgpack.t String.Map.t
  -> bool Api_result.t

val nvim_buf_detach : buffer:Buffer.Or_current.t -> bool Api_result.t

val nvim_buf_get_lines
  :  buffer:Buffer.Or_current.t
  -> start:int
  -> end_:int
  -> strict_indexing:bool
  -> string list Api_result.t

val nvim_buf_set_lines
  :  buffer:Buffer.Or_current.t
  -> start:int
  -> end_:int
  -> strict_indexing:bool
  -> replacement:string list
  -> unit Api_result.t

val nvim_buf_set_text
  :  buffer:Buffer.Or_current.t
  -> start_row:int
  -> start_col:int
  -> end_row:int
  -> end_col:int
  -> replacement:string list
  -> unit Api_result.t

val nvim_buf_get_text
  :  buffer:Buffer.Or_current.t
  -> start_row:int
  -> start_col:int
  -> end_row:int
  -> end_col:int
  -> opts:Msgpack.t String.Map.t
  -> string list Api_result.t

val nvim_buf_get_offset : buffer:Buffer.Or_current.t -> index:int -> int Api_result.t
val nvim_buf_get_var : buffer:Buffer.Or_current.t -> name:string -> Msgpack.t Api_result.t
val nvim_buf_get_changedtick : buffer:Buffer.Or_current.t -> int Api_result.t

val nvim_buf_get_keymap
  :  buffer:Buffer.Or_current.t
  -> mode:string
  -> Msgpack.t String.Map.t list Api_result.t

val nvim_buf_set_keymap
  :  buffer:Buffer.Or_current.t
  -> mode:string
  -> lhs:string
  -> rhs:string
  -> opts:Msgpack.t String.Map.t
  -> unit Api_result.t

val nvim_buf_del_keymap
  :  buffer:Buffer.Or_current.t
  -> mode:string
  -> lhs:string
  -> unit Api_result.t

val nvim_buf_set_var
  :  buffer:Buffer.Or_current.t
  -> name:string
  -> value:Msgpack.t
  -> unit Api_result.t

val nvim_buf_del_var : buffer:Buffer.Or_current.t -> name:string -> unit Api_result.t
val nvim_buf_get_name : buffer:Buffer.Or_current.t -> string Api_result.t
val nvim_buf_set_name : buffer:Buffer.Or_current.t -> name:string -> unit Api_result.t
val nvim_buf_is_loaded : buffer:Buffer.t -> bool Api_result.t

val nvim_buf_delete
  :  buffer:Buffer.Or_current.t
  -> opts:Msgpack.t String.Map.t
  -> unit Api_result.t

val nvim_buf_is_valid : buffer:Buffer.t -> bool Api_result.t
val nvim_buf_del_mark : buffer:Buffer.Or_current.t -> name:string -> bool Api_result.t

val nvim_buf_set_mark
  :  buffer:Buffer.Or_current.t
  -> name:string
  -> line:int
  -> col:int
  -> opts:Msgpack.t String.Map.t
  -> bool Api_result.t

val nvim_buf_get_mark
  :  buffer:Buffer.Or_current.t
  -> name:string
  -> (int * int) Api_result.t

val nvim_parse_cmd
  :  str:string
  -> opts:Msgpack.t String.Map.t
  -> Msgpack.t String.Map.t Api_result.t

val nvim_cmd
  :  cmd:Msgpack.t String.Map.t
  -> opts:Msgpack.t String.Map.t
  -> string Api_result.t

val nvim_create_user_command
  :  name:string
  -> command:Msgpack.t
  -> opts:Msgpack.t String.Map.t
  -> unit Api_result.t

val nvim_del_user_command : name:string -> unit Api_result.t

val nvim_buf_create_user_command
  :  buffer:Buffer.Or_current.t
  -> name:string
  -> command:Msgpack.t
  -> opts:Msgpack.t String.Map.t
  -> unit Api_result.t

val nvim_buf_del_user_command
  :  buffer:Buffer.Or_current.t
  -> name:string
  -> unit Api_result.t

val nvim_get_commands : opts:Msgpack.t String.Map.t -> Msgpack.t String.Map.t Api_result.t

val nvim_buf_get_commands
  :  buffer:Buffer.Or_current.t
  -> opts:Msgpack.t String.Map.t
  -> Msgpack.t String.Map.t Api_result.t

val nvim_get_option_info : name:string -> Msgpack.t String.Map.t Api_result.t
val nvim_create_namespace : name:string -> int Api_result.t
val nvim_get_namespaces : Msgpack.t String.Map.t Api_result.t

val nvim_buf_get_extmark_by_id
  :  buffer:Buffer.Or_current.t
  -> ns_id:int
  -> id:int
  -> opts:Msgpack.t String.Map.t
  -> int list Api_result.t

val nvim_buf_get_extmarks
  :  buffer:Buffer.Or_current.t
  -> ns_id:int
  -> start:Msgpack.t
  -> end_:Msgpack.t
  -> opts:Msgpack.t String.Map.t
  -> Msgpack.t list Api_result.t

val nvim_buf_set_extmark
  :  buffer:Buffer.Or_current.t
  -> ns_id:int
  -> line:int
  -> col:int
  -> opts:Msgpack.t String.Map.t
  -> int Api_result.t

val nvim_buf_del_extmark
  :  buffer:Buffer.Or_current.t
  -> ns_id:int
  -> id:int
  -> bool Api_result.t

val nvim_buf_add_highlight
  :  buffer:Buffer.Or_current.t
  -> ns_id:int
  -> hl_group:string
  -> line:int
  -> col_start:int
  -> col_end:int
  -> int Api_result.t

val nvim_buf_clear_namespace
  :  buffer:Buffer.Or_current.t
  -> ns_id:int
  -> line_start:int
  -> line_end:int
  -> unit Api_result.t

val nvim_get_option_value
  :  name:string
  -> opts:Msgpack.t String.Map.t
  -> Msgpack.t Api_result.t

val nvim_set_option_value
  :  name:string
  -> value:Msgpack.t
  -> opts:Msgpack.t String.Map.t
  -> unit Api_result.t

val nvim_get_all_options_info : Msgpack.t String.Map.t Api_result.t

val nvim_get_option_info2
  :  name:string
  -> opts:Msgpack.t String.Map.t
  -> Msgpack.t String.Map.t Api_result.t

val nvim_set_option : name:string -> value:Msgpack.t -> unit Api_result.t
val nvim_get_option : name:string -> Msgpack.t Api_result.t

val nvim_buf_get_option
  :  buffer:Buffer.Or_current.t
  -> name:string
  -> Msgpack.t Api_result.t

val nvim_buf_set_option
  :  buffer:Buffer.Or_current.t
  -> name:string
  -> value:Msgpack.t
  -> unit Api_result.t

val nvim_win_get_option
  :  window:Window.Or_current.t
  -> name:string
  -> Msgpack.t Api_result.t

val nvim_win_set_option
  :  window:Window.Or_current.t
  -> name:string
  -> value:Msgpack.t
  -> unit Api_result.t

val nvim_tabpage_list_wins : tabpage:Tabpage.Or_current.t -> Window.t list Api_result.t

val nvim_tabpage_get_var
  :  tabpage:Tabpage.Or_current.t
  -> name:string
  -> Msgpack.t Api_result.t

val nvim_tabpage_set_var
  :  tabpage:Tabpage.Or_current.t
  -> name:string
  -> value:Msgpack.t
  -> unit Api_result.t

val nvim_tabpage_del_var
  :  tabpage:Tabpage.Or_current.t
  -> name:string
  -> unit Api_result.t

val nvim_tabpage_get_win : tabpage:Tabpage.Or_current.t -> Window.t Api_result.t
val nvim_tabpage_get_number : tabpage:Tabpage.Or_current.t -> int Api_result.t
val nvim_tabpage_is_valid : tabpage:Tabpage.t -> bool Api_result.t

val nvim_ui_attach
  :  width:int
  -> height:int
  -> options:Msgpack.t String.Map.t
  -> unit Api_result.t

val nvim_ui_set_focus : gained:bool -> unit Api_result.t
val nvim_ui_detach : unit Api_result.t
val nvim_ui_try_resize : width:int -> height:int -> unit Api_result.t
val nvim_ui_set_option : name:string -> value:Msgpack.t -> unit Api_result.t
val nvim_ui_try_resize_grid : grid:int -> width:int -> height:int -> unit Api_result.t
val nvim_ui_pum_set_height : height:int -> unit Api_result.t

val nvim_ui_pum_set_bounds
  :  width:float
  -> height:float
  -> row:float
  -> col:float
  -> unit Api_result.t

val nvim_get_hl_id_by_name : name:string -> int Api_result.t

val nvim_get_hl
  :  ns_id:int
  -> opts:Msgpack.t String.Map.t
  -> Msgpack.t String.Map.t Api_result.t

val nvim_set_hl
  :  ns_id:int
  -> name:string
  -> val_:Msgpack.t String.Map.t
  -> unit Api_result.t

val nvim_set_hl_ns : ns_id:int -> unit Api_result.t
val nvim_feedkeys : keys:string -> mode:string -> escape_ks:bool -> unit Api_result.t
val nvim_input : keys:string -> int Api_result.t

val nvim_input_mouse
  :  button:string
  -> action:string
  -> modifier:string
  -> grid:int
  -> row:int
  -> col:int
  -> unit Api_result.t

val nvim_replace_termcodes
  :  str:string
  -> from_part:bool
  -> do_lt:bool
  -> special:bool
  -> string Api_result.t

val nvim_exec_lua : code:string -> args:Msgpack.t list -> Msgpack.t Api_result.t

val nvim_notify
  :  msg:string
  -> log_level:int
  -> opts:Msgpack.t String.Map.t
  -> Msgpack.t Api_result.t

val nvim_strwidth : text:string -> int Api_result.t
val nvim_list_runtime_paths : string list Api_result.t
val nvim_get_runtime_file : name:string -> all:bool -> string list Api_result.t
val nvim_set_current_dir : dir:string -> unit Api_result.t
val nvim_get_current_line : string Api_result.t
val nvim_set_current_line : line:string -> unit Api_result.t
val nvim_del_current_line : unit Api_result.t
val nvim_get_var : name:string -> Msgpack.t Api_result.t
val nvim_set_var : name:string -> value:Msgpack.t -> unit Api_result.t
val nvim_del_var : name:string -> unit Api_result.t
val nvim_get_vvar : name:string -> Msgpack.t Api_result.t
val nvim_set_vvar : name:string -> value:Msgpack.t -> unit Api_result.t

val nvim_echo
  :  chunks:Msgpack.t list
  -> history:bool
  -> opts:Msgpack.t String.Map.t
  -> unit Api_result.t

val nvim_out_write : str:string -> unit Api_result.t
val nvim_err_write : str:string -> unit Api_result.t
val nvim_err_writeln : str:string -> unit Api_result.t
val nvim_list_bufs : Buffer.t list Api_result.t
val nvim_get_current_buf : Buffer.t Api_result.t
val nvim_set_current_buf : buffer:Buffer.t -> unit Api_result.t
val nvim_list_wins : Window.t list Api_result.t
val nvim_get_current_win : Window.t Api_result.t
val nvim_set_current_win : window:Window.t -> unit Api_result.t
val nvim_create_buf : listed:bool -> scratch:bool -> Buffer.t Api_result.t

val nvim_open_term
  :  buffer:Buffer.Or_current.t
  -> opts:Msgpack.t String.Map.t
  -> int Api_result.t

val nvim_chan_send : chan:int -> data:string -> unit Api_result.t
val nvim_list_tabpages : Tabpage.t list Api_result.t
val nvim_get_current_tabpage : Tabpage.t Api_result.t
val nvim_set_current_tabpage : tabpage:Tabpage.t -> unit Api_result.t
val nvim_paste : data:string -> crlf:bool -> phase:int -> bool Api_result.t

val nvim_put
  :  lines:string list
  -> type_:string
  -> after:bool
  -> follow:bool
  -> unit Api_result.t

val nvim_subscribe : event:string -> unit Api_result.t
val nvim_unsubscribe : event:string -> unit Api_result.t
val nvim_get_color_by_name : name:string -> int Api_result.t
val nvim_get_color_map : Msgpack.t String.Map.t Api_result.t
val nvim_get_context : opts:Msgpack.t String.Map.t -> Msgpack.t String.Map.t Api_result.t
val nvim_load_context : dict:Msgpack.t String.Map.t -> Msgpack.t Api_result.t
val nvim_get_mode : Msgpack.t String.Map.t Api_result.t
val nvim_get_keymap : mode:string -> Msgpack.t String.Map.t list Api_result.t

val nvim_set_keymap
  :  mode:string
  -> lhs:string
  -> rhs:string
  -> opts:Msgpack.t String.Map.t
  -> unit Api_result.t

val nvim_del_keymap : mode:string -> lhs:string -> unit Api_result.t
val nvim_get_api_info : Msgpack.t list Api_result.t

val nvim_set_client_info
  :  name:string
  -> version:Msgpack.t String.Map.t
  -> type_:string
  -> methods:Msgpack.t String.Map.t
  -> attributes:Msgpack.t String.Map.t
  -> unit Api_result.t

val nvim_get_chan_info : chan:int -> Msgpack.t String.Map.t Api_result.t
val nvim_list_chans : Msgpack.t list Api_result.t
val nvim_call_atomic : calls:Msgpack.t list -> Msgpack.t list Api_result.t
val nvim_list_uis : Msgpack.t list Api_result.t
val nvim_get_proc_children : pid:int -> Msgpack.t list Api_result.t
val nvim_get_proc : pid:int -> Msgpack.t Api_result.t

val nvim_select_popupmenu_item
  :  item:int
  -> insert:bool
  -> finish:bool
  -> opts:Msgpack.t String.Map.t
  -> unit Api_result.t

val nvim_del_mark : name:string -> bool Api_result.t

val nvim_get_mark
  :  name:string
  -> opts:Msgpack.t String.Map.t
  -> Msgpack.t list Api_result.t

val nvim_eval_statusline
  :  str:string
  -> opts:Msgpack.t String.Map.t
  -> Msgpack.t String.Map.t Api_result.t

val nvim_exec2
  :  src:string
  -> opts:Msgpack.t String.Map.t
  -> Msgpack.t String.Map.t Api_result.t

val nvim_command : command:string -> unit Api_result.t
val nvim_eval : expr:string -> Msgpack.t Api_result.t
val nvim_call_function : fn:string -> args:Msgpack.t list -> Msgpack.t Api_result.t

val nvim_call_dict_function
  :  dict:Msgpack.t
  -> fn:string
  -> args:Msgpack.t list
  -> Msgpack.t Api_result.t

val nvim_parse_expression
  :  expr:string
  -> flags:string
  -> highlight:bool
  -> Msgpack.t String.Map.t Api_result.t

val nvim_open_win
  :  buffer:Buffer.Or_current.t
  -> enter:bool
  -> config:Msgpack.t String.Map.t
  -> Window.t Api_result.t

val nvim_win_set_config
  :  window:Window.Or_current.t
  -> config:Msgpack.t String.Map.t
  -> unit Api_result.t

val nvim_win_get_config
  :  window:Window.Or_current.t
  -> Msgpack.t String.Map.t Api_result.t

val nvim_win_get_buf : window:Window.Or_current.t -> Buffer.t Api_result.t
val nvim_win_set_buf : window:Window.Or_current.t -> buffer:Buffer.t -> unit Api_result.t
val nvim_win_get_cursor : window:Window.Or_current.t -> (int * int) Api_result.t
val nvim_win_set_cursor : window:Window.Or_current.t -> pos:int * int -> unit Api_result.t
val nvim_win_get_height : window:Window.Or_current.t -> int Api_result.t
val nvim_win_set_height : window:Window.Or_current.t -> height:int -> unit Api_result.t
val nvim_win_get_width : window:Window.Or_current.t -> int Api_result.t
val nvim_win_set_width : window:Window.Or_current.t -> width:int -> unit Api_result.t
val nvim_win_get_var : window:Window.Or_current.t -> name:string -> Msgpack.t Api_result.t

val nvim_win_set_var
  :  window:Window.Or_current.t
  -> name:string
  -> value:Msgpack.t
  -> unit Api_result.t

val nvim_win_del_var : window:Window.Or_current.t -> name:string -> unit Api_result.t
val nvim_win_get_position : window:Window.Or_current.t -> (int * int) Api_result.t
val nvim_win_get_tabpage : window:Window.Or_current.t -> Tabpage.t Api_result.t
val nvim_win_get_number : window:Window.Or_current.t -> int Api_result.t
val nvim_win_is_valid : window:Window.t -> bool Api_result.t
val nvim_win_hide : window:Window.Or_current.t -> unit Api_result.t
val nvim_win_close : window:Window.Or_current.t -> force:bool -> unit Api_result.t
val nvim_win_set_hl_ns : window:Window.Or_current.t -> ns_id:int -> unit Api_result.t

module Options : sig
  module Data : sig
    module Type : sig
      type t = private
        | String
        | Int
        | Bool
        | Char_list of { commalist : bool }
        | String_list
    end

    type t = private
      { name : string
      ; global_local : bool
      ; type_ : Type.t
      }
  end

  module Global : sig
    val options : Data.t list
  end

  module Buffer : sig
    val options : Data.t list
  end

  module Window : sig
    val options : Data.t list
  end
end
