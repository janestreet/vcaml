open! Core
open Runtime

module Version : sig
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

val version : Version.t

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
  [@@deriving fields, sexp_of]

  val empty : t
end

module Luaref : Msgpack.Msgpackable
module Buffer : Nvim_id
module Window : Nvim_id
module Tabpage : Nvim_id

module Phantom : sig
  (** For use in pattern matching. For example, if you have a ['a Phantom.t * 'a], if a
      pattern match on the phantom succeeds on [Integer], then ['a] unifies with [int].
      Use [Custom] for serialization & deserialization of custom types. *)
  type _ t =
    | Nil : unit t
    | Integer : int t
    | Boolean : bool t
    | Float : float t
    | Array : 'a t -> 'a list t
    | Tuple : 'a t * int -> 'a list t
    | Dict : (Msgpack.t * Msgpack.t) list t
    | String : string t
    | Buffer : Buffer.t t
    | Window : Window.t t
    | Tabpage : Tabpage.t t
    | Luaref : Luaref.t t
    | Object : Msgpack.t t
    | Custom : (module Msgpack.Msgpackable with type t = 'a) -> 'a t
  [@@deriving sexp_of]
end

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
  [@@deriving sexp_of]

  val of_int : int -> t Or_error.t
end

module Ui_event : sig
  type t =
    | Mode_info_set of
        { enabled : bool
        ; cursor_styles : Msgpack.t list
        }
    | Update_menu
    | Busy_start
    | Busy_stop
    | Mouse_on
    | Mouse_off
    | Mode_change of
        { mode : string
        ; mode_idx : int
        }
    | Bell
    | Visual_bell
    | Flush
    | Suspend
    | Set_title of { title : string }
    | Set_icon of { icon : string }
    | Screenshot of { path : string }
    | Option_set of
        { name : string
        ; value : Msgpack.t
        }
    | Update_fg of { fg : int }
    | Update_bg of { bg : int }
    | Update_sp of { sp : int }
    | Resize of
        { width : int
        ; height : int
        }
    | Clear
    | Eol_clear
    | Cursor_goto of
        { row : int
        ; col : int
        }
    | Highlight_set of { attrs : (Msgpack.t * Msgpack.t) list }
    | Put of { str : string }
    | Set_scroll_region of
        { top : int
        ; bot : int
        ; left : int
        ; right : int
        }
    | Scroll of { count : int }
    | Default_colors_set of
        { rgb_fg : int
        ; rgb_bg : int
        ; rgb_sp : int
        ; cterm_fg : int
        ; cterm_bg : int
        }
    | Hl_attr_define of
        { id : int
        ; rgb_attrs : (Msgpack.t * Msgpack.t) list
        ; cterm_attrs : (Msgpack.t * Msgpack.t) list
        ; info : Msgpack.t list
        }
    | Hl_group_set of
        { name : string
        ; id : int
        }
    | Grid_resize of
        { grid : int
        ; width : int
        ; height : int
        }
    | Grid_clear of { grid : int }
    | Grid_cursor_goto of
        { grid : int
        ; row : int
        ; col : int
        }
    | Grid_line of
        { grid : int
        ; row : int
        ; col_start : int
        ; data : Msgpack.t list
        }
    | Grid_scroll of
        { grid : int
        ; top : int
        ; bot : int
        ; left : int
        ; right : int
        ; rows : int
        ; cols : int
        }
    | Grid_destroy of { grid : int }
    | Win_pos of
        { grid : int
        ; win : Window.t
        ; startrow : int
        ; startcol : int
        ; width : int
        ; height : int
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
        }
    | Win_external_pos of
        { grid : int
        ; win : Window.t
        }
    | Win_hide of { grid : int }
    | Win_close of { grid : int }
    | Msg_set_pos of
        { grid : int
        ; row : int
        ; scrolled : bool
        ; sep_char : string
        }
    | Win_viewport of
        { grid : int
        ; win : Window.t
        ; topline : int
        ; botline : int
        ; curline : int
        ; curcol : int
        }
    | Popupmenu_show of
        { items : Msgpack.t list
        ; selected : int
        ; row : int
        ; col : int
        ; grid : int
        }
    | Popupmenu_hide
    | Popupmenu_select of { selected : int }
    | Tabline_update of
        { current : Tabpage.t
        ; tabs : Msgpack.t list
        ; current_buffer : Buffer.t
        ; buffers : Msgpack.t list
        }
    | Cmdline_show of
        { content : Msgpack.t list
        ; pos : int
        ; firstc : string
        ; prompt : string
        ; indent : int
        ; level : int
        }
    | Cmdline_pos of
        { pos : int
        ; level : int
        }
    | Cmdline_special_char of
        { c : string
        ; shift : bool
        ; level : int
        }
    | Cmdline_hide of { level : int }
    | Cmdline_block_show of { lines : Msgpack.t list }
    | Cmdline_block_append of { lines : Msgpack.t list }
    | Cmdline_block_hide
    | Wildmenu_show of { items : Msgpack.t list }
    | Wildmenu_select of { selected : int }
    | Wildmenu_hide
    | Msg_show of
        { kind : string
        ; content : Msgpack.t list
        ; replace_last : bool
        }
    | Msg_clear
    | Msg_showcmd of { content : Msgpack.t list }
    | Msg_showmode of { content : Msgpack.t list }
    | Msg_ruler of { content : Msgpack.t list }
    | Msg_history_show of { entries : Msgpack.t list }
  [@@deriving sexp_of]

  val of_msgpack : Msgpack.t -> t list Or_error.t
end

val nvim_buf_line_count : buffer:Buffer.t -> int Api_result.t

val nvim_buf_attach
  :  buffer:Buffer.t
  -> send_buffer:bool
  -> opts:(Msgpack.t * Msgpack.t) list
  -> bool Api_result.t

val nvim_buf_detach : buffer:Buffer.t -> bool Api_result.t

val nvim_buf_get_lines
  :  buffer:Buffer.t
  -> start:int
  -> end_:int
  -> strict_indexing:bool
  -> Msgpack.t list Api_result.t

val nvim_buf_set_lines
  :  buffer:Buffer.t
  -> start:int
  -> end_:int
  -> strict_indexing:bool
  -> replacement:Msgpack.t list
  -> unit Api_result.t

val nvim_buf_set_text
  :  buffer:Buffer.t
  -> start_row:int
  -> start_col:int
  -> end_row:int
  -> end_col:int
  -> replacement:Msgpack.t list
  -> unit Api_result.t

val nvim_buf_get_offset : buffer:Buffer.t -> index:int -> int Api_result.t
val nvim_buf_get_var : buffer:Buffer.t -> name:string -> Msgpack.t Api_result.t
val nvim_buf_get_changedtick : buffer:Buffer.t -> int Api_result.t
val nvim_buf_get_keymap : buffer:Buffer.t -> mode:string -> Msgpack.t list Api_result.t

val nvim_buf_set_keymap
  :  buffer:Buffer.t
  -> mode:string
  -> lhs:string
  -> rhs:string
  -> opts:(Msgpack.t * Msgpack.t) list
  -> unit Api_result.t

val nvim_buf_del_keymap
  :  buffer:Buffer.t
  -> mode:string
  -> lhs:string
  -> unit Api_result.t

val nvim_buf_get_commands
  :  buffer:Buffer.t
  -> opts:(Msgpack.t * Msgpack.t) list
  -> (Msgpack.t * Msgpack.t) list Api_result.t

val nvim_buf_set_var
  :  buffer:Buffer.t
  -> name:string
  -> value:Msgpack.t
  -> unit Api_result.t

val nvim_buf_del_var : buffer:Buffer.t -> name:string -> unit Api_result.t
val nvim_buf_get_option : buffer:Buffer.t -> name:string -> Msgpack.t Api_result.t

val nvim_buf_set_option
  :  buffer:Buffer.t
  -> name:string
  -> value:Msgpack.t
  -> unit Api_result.t

val nvim_buf_get_name : buffer:Buffer.t -> string Api_result.t
val nvim_buf_set_name : buffer:Buffer.t -> name:string -> unit Api_result.t
val nvim_buf_is_loaded : buffer:Buffer.t -> bool Api_result.t

val nvim_buf_delete
  :  buffer:Buffer.t
  -> opts:(Msgpack.t * Msgpack.t) list
  -> unit Api_result.t

val nvim_buf_is_valid : buffer:Buffer.t -> bool Api_result.t
val nvim_buf_get_mark : buffer:Buffer.t -> name:string -> Msgpack.t list Api_result.t

val nvim_buf_get_extmark_by_id
  :  buffer:Buffer.t
  -> ns_id:int
  -> id:int
  -> opts:(Msgpack.t * Msgpack.t) list
  -> Msgpack.t list Api_result.t

val nvim_buf_get_extmarks
  :  buffer:Buffer.t
  -> ns_id:int
  -> start:Msgpack.t
  -> end_:Msgpack.t
  -> opts:(Msgpack.t * Msgpack.t) list
  -> Msgpack.t list Api_result.t

val nvim_buf_set_extmark
  :  buffer:Buffer.t
  -> ns_id:int
  -> line:int
  -> col:int
  -> opts:(Msgpack.t * Msgpack.t) list
  -> int Api_result.t

val nvim_buf_del_extmark : buffer:Buffer.t -> ns_id:int -> id:int -> bool Api_result.t

val nvim_buf_add_highlight
  :  buffer:Buffer.t
  -> ns_id:int
  -> hl_group:string
  -> line:int
  -> col_start:int
  -> col_end:int
  -> int Api_result.t

val nvim_buf_clear_namespace
  :  buffer:Buffer.t
  -> ns_id:int
  -> line_start:int
  -> line_end:int
  -> unit Api_result.t

val nvim_buf_set_virtual_text
  :  buffer:Buffer.t
  -> src_id:int
  -> line:int
  -> chunks:Msgpack.t list
  -> opts:(Msgpack.t * Msgpack.t) list
  -> int Api_result.t

val nvim_buf_call : buffer:Buffer.t -> fun_:Luaref.t -> Msgpack.t Api_result.t

val nvim_command_output : command:string -> string Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 7 "]

val nvim_execute_lua : code:string -> args:Msgpack.t list -> Msgpack.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 7 "]

val nvim_buf_get_number : buffer:Buffer.t -> int Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 2 "]

val nvim_buf_clear_highlight
  :  buffer:Buffer.t
  -> ns_id:int
  -> line_start:int
  -> line_end:int
  -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 7 "]

val buffer_insert
  :  buffer:Buffer.t
  -> lnum:int
  -> lines:Msgpack.t list
  -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val buffer_get_line : buffer:Buffer.t -> index:int -> string Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val buffer_set_line : buffer:Buffer.t -> index:int -> line:string -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val buffer_del_line : buffer:Buffer.t -> index:int -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val buffer_get_line_slice
  :  buffer:Buffer.t
  -> start:int
  -> end_:int
  -> include_start:bool
  -> include_end:bool
  -> Msgpack.t list Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val buffer_set_line_slice
  :  buffer:Buffer.t
  -> start:int
  -> end_:int
  -> include_start:bool
  -> include_end:bool
  -> replacement:Msgpack.t list
  -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val buffer_set_var
  :  buffer:Buffer.t
  -> name:string
  -> value:Msgpack.t
  -> Msgpack.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val buffer_del_var : buffer:Buffer.t -> name:string -> Msgpack.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val window_set_var
  :  window:Window.t
  -> name:string
  -> value:Msgpack.t
  -> Msgpack.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val window_del_var : window:Window.t -> name:string -> Msgpack.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val tabpage_set_var
  :  tabpage:Tabpage.t
  -> name:string
  -> value:Msgpack.t
  -> Msgpack.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val tabpage_del_var : tabpage:Tabpage.t -> name:string -> Msgpack.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_set_var : name:string -> value:Msgpack.t -> Msgpack.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_del_var : name:string -> Msgpack.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val nvim_tabpage_list_wins : tabpage:Tabpage.t -> Msgpack.t list Api_result.t
val nvim_tabpage_get_var : tabpage:Tabpage.t -> name:string -> Msgpack.t Api_result.t

val nvim_tabpage_set_var
  :  tabpage:Tabpage.t
  -> name:string
  -> value:Msgpack.t
  -> unit Api_result.t

val nvim_tabpage_del_var : tabpage:Tabpage.t -> name:string -> unit Api_result.t
val nvim_tabpage_get_win : tabpage:Tabpage.t -> Window.t Api_result.t
val nvim_tabpage_get_number : tabpage:Tabpage.t -> int Api_result.t
val nvim_tabpage_is_valid : tabpage:Tabpage.t -> bool Api_result.t

val nvim_ui_attach
  :  width:int
  -> height:int
  -> options:(Msgpack.t * Msgpack.t) list
  -> unit Api_result.t

val ui_attach : width:int -> height:int -> enable_rgb:bool -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

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

val nvim_exec : src:string -> output:bool -> string Api_result.t
val nvim_command : command:string -> unit Api_result.t

val nvim_get_hl_by_name
  :  name:string
  -> rgb:bool
  -> (Msgpack.t * Msgpack.t) list Api_result.t

val nvim_get_hl_by_id : hl_id:int -> rgb:bool -> (Msgpack.t * Msgpack.t) list Api_result.t
val nvim_get_hl_id_by_name : name:string -> int Api_result.t

val nvim_set_hl
  :  ns_id:int
  -> name:string
  -> val_:(Msgpack.t * Msgpack.t) list
  -> unit Api_result.t

val nvim_feedkeys : keys:string -> mode:string -> escape_csi:bool -> unit Api_result.t
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

val nvim_eval : expr:string -> Msgpack.t Api_result.t
val nvim_exec_lua : code:string -> args:Msgpack.t list -> Msgpack.t Api_result.t

val nvim_notify
  :  msg:string
  -> log_level:int
  -> opts:(Msgpack.t * Msgpack.t) list
  -> Msgpack.t Api_result.t

val nvim_call_function : fn:string -> args:Msgpack.t list -> Msgpack.t Api_result.t

val nvim_call_dict_function
  :  dict:Msgpack.t
  -> fn:string
  -> args:Msgpack.t list
  -> Msgpack.t Api_result.t

val nvim_strwidth : text:string -> int Api_result.t
val nvim_list_runtime_paths : Msgpack.t list Api_result.t
val nvim_get_runtime_file : name:string -> all:bool -> Msgpack.t list Api_result.t
val nvim_set_current_dir : dir:string -> unit Api_result.t
val nvim_get_current_line : string Api_result.t
val nvim_set_current_line : line:string -> unit Api_result.t
val nvim_del_current_line : unit Api_result.t
val nvim_get_var : name:string -> Msgpack.t Api_result.t
val nvim_set_var : name:string -> value:Msgpack.t -> unit Api_result.t
val nvim_del_var : name:string -> unit Api_result.t
val nvim_get_vvar : name:string -> Msgpack.t Api_result.t
val nvim_set_vvar : name:string -> value:Msgpack.t -> unit Api_result.t
val nvim_get_option : name:string -> Msgpack.t Api_result.t
val nvim_get_all_options_info : (Msgpack.t * Msgpack.t) list Api_result.t
val nvim_get_option_info : name:string -> (Msgpack.t * Msgpack.t) list Api_result.t
val nvim_set_option : name:string -> value:Msgpack.t -> unit Api_result.t

val nvim_echo
  :  chunks:Msgpack.t list
  -> history:bool
  -> opts:(Msgpack.t * Msgpack.t) list
  -> unit Api_result.t

val nvim_out_write : str:string -> unit Api_result.t
val nvim_err_write : str:string -> unit Api_result.t
val nvim_err_writeln : str:string -> unit Api_result.t
val nvim_list_bufs : Msgpack.t list Api_result.t
val nvim_get_current_buf : Buffer.t Api_result.t
val nvim_set_current_buf : buffer:Buffer.t -> unit Api_result.t
val nvim_list_wins : Msgpack.t list Api_result.t
val nvim_get_current_win : Window.t Api_result.t
val nvim_set_current_win : window:Window.t -> unit Api_result.t
val nvim_create_buf : listed:bool -> scratch:bool -> Buffer.t Api_result.t

val nvim_open_term
  :  buffer:Buffer.t
  -> opts:(Msgpack.t * Msgpack.t) list
  -> int Api_result.t

val nvim_chan_send : chan:int -> data:string -> unit Api_result.t

val nvim_open_win
  :  buffer:Buffer.t
  -> enter:bool
  -> config:(Msgpack.t * Msgpack.t) list
  -> Window.t Api_result.t

val nvim_list_tabpages : Msgpack.t list Api_result.t
val nvim_get_current_tabpage : Tabpage.t Api_result.t
val nvim_set_current_tabpage : tabpage:Tabpage.t -> unit Api_result.t
val nvim_create_namespace : name:string -> int Api_result.t
val nvim_get_namespaces : (Msgpack.t * Msgpack.t) list Api_result.t
val nvim_paste : data:string -> crlf:bool -> phase:int -> bool Api_result.t

val nvim_put
  :  lines:Msgpack.t list
  -> type_:string
  -> after:bool
  -> follow:bool
  -> unit Api_result.t

val nvim_subscribe : event:string -> unit Api_result.t
val nvim_unsubscribe : event:string -> unit Api_result.t
val nvim_get_color_by_name : name:string -> int Api_result.t
val nvim_get_color_map : (Msgpack.t * Msgpack.t) list Api_result.t

val nvim_get_context
  :  opts:(Msgpack.t * Msgpack.t) list
  -> (Msgpack.t * Msgpack.t) list Api_result.t

val nvim_load_context : dict:(Msgpack.t * Msgpack.t) list -> Msgpack.t Api_result.t
val nvim_get_mode : (Msgpack.t * Msgpack.t) list Api_result.t
val nvim_get_keymap : mode:string -> Msgpack.t list Api_result.t

val nvim_set_keymap
  :  mode:string
  -> lhs:string
  -> rhs:string
  -> opts:(Msgpack.t * Msgpack.t) list
  -> unit Api_result.t

val nvim_del_keymap : mode:string -> lhs:string -> unit Api_result.t

val nvim_get_commands
  :  opts:(Msgpack.t * Msgpack.t) list
  -> (Msgpack.t * Msgpack.t) list Api_result.t

val nvim_get_api_info : Msgpack.t list Api_result.t

val nvim_set_client_info
  :  name:string
  -> version:(Msgpack.t * Msgpack.t) list
  -> type_:string
  -> methods:(Msgpack.t * Msgpack.t) list
  -> attributes:(Msgpack.t * Msgpack.t) list
  -> unit Api_result.t

val nvim_get_chan_info : chan:int -> (Msgpack.t * Msgpack.t) list Api_result.t
val nvim_list_chans : Msgpack.t list Api_result.t
val nvim_call_atomic : calls:Msgpack.t list -> Msgpack.t list Api_result.t

val nvim_parse_expression
  :  expr:string
  -> flags:string
  -> highlight:bool
  -> (Msgpack.t * Msgpack.t) list Api_result.t

val nvim_list_uis : Msgpack.t list Api_result.t
val nvim_get_proc_children : pid:int -> Msgpack.t list Api_result.t
val nvim_get_proc : pid:int -> Msgpack.t Api_result.t

val nvim_select_popupmenu_item
  :  item:int
  -> insert:bool
  -> finish:bool
  -> opts:(Msgpack.t * Msgpack.t) list
  -> unit Api_result.t

val nvim_set_decoration_provider
  :  ns_id:int
  -> opts:(Msgpack.t * Msgpack.t) list
  -> unit Api_result.t

val nvim_win_get_buf : window:Window.t -> Buffer.t Api_result.t
val nvim_win_set_buf : window:Window.t -> buffer:Buffer.t -> unit Api_result.t
val nvim_win_get_cursor : window:Window.t -> Msgpack.t list Api_result.t
val nvim_win_set_cursor : window:Window.t -> pos:Msgpack.t list -> unit Api_result.t
val nvim_win_get_height : window:Window.t -> int Api_result.t
val nvim_win_set_height : window:Window.t -> height:int -> unit Api_result.t
val nvim_win_get_width : window:Window.t -> int Api_result.t
val nvim_win_set_width : window:Window.t -> width:int -> unit Api_result.t
val nvim_win_get_var : window:Window.t -> name:string -> Msgpack.t Api_result.t

val nvim_win_set_var
  :  window:Window.t
  -> name:string
  -> value:Msgpack.t
  -> unit Api_result.t

val nvim_win_del_var : window:Window.t -> name:string -> unit Api_result.t
val nvim_win_get_option : window:Window.t -> name:string -> Msgpack.t Api_result.t

val nvim_win_set_option
  :  window:Window.t
  -> name:string
  -> value:Msgpack.t
  -> unit Api_result.t

val nvim_win_get_position : window:Window.t -> Msgpack.t list Api_result.t
val nvim_win_get_tabpage : window:Window.t -> Tabpage.t Api_result.t
val nvim_win_get_number : window:Window.t -> int Api_result.t
val nvim_win_is_valid : window:Window.t -> bool Api_result.t

val nvim_win_set_config
  :  window:Window.t
  -> config:(Msgpack.t * Msgpack.t) list
  -> unit Api_result.t

val nvim_win_get_config : window:Window.t -> (Msgpack.t * Msgpack.t) list Api_result.t
val nvim_win_hide : window:Window.t -> unit Api_result.t
val nvim_win_close : window:Window.t -> force:bool -> unit Api_result.t
val nvim_win_call : window:Window.t -> fun_:Luaref.t -> Msgpack.t Api_result.t

val buffer_line_count : buffer:Buffer.t -> int Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val buffer_get_lines
  :  buffer:Buffer.t
  -> start:int
  -> end_:int
  -> strict_indexing:bool
  -> Msgpack.t list Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val buffer_set_lines
  :  buffer:Buffer.t
  -> start:int
  -> end_:int
  -> strict_indexing:bool
  -> replacement:Msgpack.t list
  -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val buffer_get_var : buffer:Buffer.t -> name:string -> Msgpack.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val buffer_get_option : buffer:Buffer.t -> name:string -> Msgpack.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val buffer_set_option
  :  buffer:Buffer.t
  -> name:string
  -> value:Msgpack.t
  -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val buffer_get_name : buffer:Buffer.t -> string Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val buffer_set_name : buffer:Buffer.t -> name:string -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val buffer_is_valid : buffer:Buffer.t -> bool Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val buffer_get_mark : buffer:Buffer.t -> name:string -> Msgpack.t list Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val buffer_add_highlight
  :  buffer:Buffer.t
  -> ns_id:int
  -> hl_group:string
  -> line:int
  -> col_start:int
  -> col_end:int
  -> int Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_command_output : command:string -> string Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val buffer_get_number : buffer:Buffer.t -> int Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val buffer_clear_highlight
  :  buffer:Buffer.t
  -> ns_id:int
  -> line_start:int
  -> line_end:int
  -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val tabpage_get_windows : tabpage:Tabpage.t -> Msgpack.t list Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val tabpage_get_var : tabpage:Tabpage.t -> name:string -> Msgpack.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val tabpage_get_window : tabpage:Tabpage.t -> Window.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val tabpage_is_valid : tabpage:Tabpage.t -> bool Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val ui_detach : unit Api_result.t [@@deprecated "[since 1111-11] neovim_version: 1 "]

val ui_try_resize : width:int -> height:int -> Msgpack.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_command : command:string -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_feedkeys : keys:string -> mode:string -> escape_csi:bool -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_input : keys:string -> int Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_replace_termcodes
  :  str:string
  -> from_part:bool
  -> do_lt:bool
  -> special:bool
  -> string Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_eval : expr:string -> Msgpack.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_call_function : fn:string -> args:Msgpack.t list -> Msgpack.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_strwidth : text:string -> int Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_list_runtime_paths : Msgpack.t list Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_change_directory : dir:string -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_get_current_line : string Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_set_current_line : line:string -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_del_current_line : unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_get_var : name:string -> Msgpack.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_get_vvar : name:string -> Msgpack.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_get_option : name:string -> Msgpack.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_set_option : name:string -> value:Msgpack.t -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_out_write : str:string -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_err_write : str:string -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_report_error : str:string -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_get_buffers : Msgpack.t list Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_get_current_buffer : Buffer.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_set_current_buffer : buffer:Buffer.t -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_get_windows : Msgpack.t list Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_get_current_window : Window.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_set_current_window : window:Window.t -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_get_tabpages : Msgpack.t list Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_get_current_tabpage : Tabpage.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_set_current_tabpage : tabpage:Tabpage.t -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_subscribe : event:string -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_unsubscribe : event:string -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_name_to_color : name:string -> int Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_get_color_map : (Msgpack.t * Msgpack.t) list Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val vim_get_api_info : Msgpack.t list Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val window_get_buffer : window:Window.t -> Buffer.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val window_get_cursor : window:Window.t -> Msgpack.t list Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val window_set_cursor : window:Window.t -> pos:Msgpack.t list -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val window_get_height : window:Window.t -> int Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val window_set_height : window:Window.t -> height:int -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val window_get_width : window:Window.t -> int Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val window_set_width : window:Window.t -> width:int -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val window_get_var : window:Window.t -> name:string -> Msgpack.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val window_get_option : window:Window.t -> name:string -> Msgpack.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val window_set_option
  :  window:Window.t
  -> name:string
  -> value:Msgpack.t
  -> unit Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val window_get_position : window:Window.t -> Msgpack.t list Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val window_get_tabpage : window:Window.t -> Tabpage.t Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]

val window_is_valid : window:Window.t -> bool Api_result.t
[@@deprecated "[since 1111-11] neovim_version: 1 "]
