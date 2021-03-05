open! Base
open Types

val nvim_buf_line_count : buffer:Buffer.t -> int Api_result.t
val buffer_get_line : buffer:Buffer.t -> index:int -> string Api_result.t

val nvim_buf_attach
  :  buffer:Buffer.t
  -> send_buffer:bool
  -> opts:(Msgpack.t * Msgpack.t) list
  -> bool Api_result.t

val nvim_buf_detach : buffer:Buffer.t -> bool Api_result.t
val buffer_set_line : buffer:Buffer.t -> index:int -> line:string -> unit Api_result.t
val buffer_del_line : buffer:Buffer.t -> index:int -> unit Api_result.t

val buffer_get_line_slice
  :  buffer:Buffer.t
  -> start:int
  -> end_:int
  -> include_start:bool
  -> include_end:bool
  -> Msgpack.t list Api_result.t

val nvim_buf_get_lines
  :  buffer:Buffer.t
  -> start:int
  -> end_:int
  -> strict_indexing:bool
  -> Msgpack.t list Api_result.t

val buffer_set_line_slice
  :  buffer:Buffer.t
  -> start:int
  -> end_:int
  -> include_start:bool
  -> include_end:bool
  -> replacement:Msgpack.t list
  -> unit Api_result.t

val nvim_buf_set_lines
  :  buffer:Buffer.t
  -> start:int
  -> end_:int
  -> strict_indexing:bool
  -> replacement:Msgpack.t list
  -> unit Api_result.t

val nvim_buf_get_offset : buffer:Buffer.t -> index:int -> int Api_result.t
val nvim_buf_get_var : buffer:Buffer.t -> name:string -> Msgpack.t Api_result.t
val nvim_buf_get_changedtick : buffer:Buffer.t -> int Api_result.t
val nvim_buf_get_keymap : buffer:Buffer.t -> mode:string -> Msgpack.t list Api_result.t

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

val buffer_set_var
  :  buffer:Buffer.t
  -> name:string
  -> value:Msgpack.t
  -> Msgpack.t Api_result.t

val buffer_del_var : buffer:Buffer.t -> name:string -> Msgpack.t Api_result.t
val nvim_buf_get_option : buffer:Buffer.t -> name:string -> Msgpack.t Api_result.t

val nvim_buf_set_option
  :  buffer:Buffer.t
  -> name:string
  -> value:Msgpack.t
  -> unit Api_result.t

val nvim_buf_get_number : buffer:Buffer.t -> int Api_result.t
val nvim_buf_get_name : buffer:Buffer.t -> string Api_result.t
val nvim_buf_set_name : buffer:Buffer.t -> name:string -> unit Api_result.t
val nvim_buf_is_loaded : buffer:Buffer.t -> bool Api_result.t
val nvim_buf_is_valid : buffer:Buffer.t -> bool Api_result.t

val buffer_insert
  :  buffer:Buffer.t
  -> lnum:int
  -> lines:Msgpack.t list
  -> unit Api_result.t

val nvim_buf_get_mark : buffer:Buffer.t -> name:string -> Msgpack.t list Api_result.t

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

val nvim_buf_clear_highlight
  :  buffer:Buffer.t
  -> ns_id:int
  -> line_start:int
  -> line_end:int
  -> unit Api_result.t

val nvim_buf_set_virtual_text
  :  buffer:Buffer.t
  -> ns_id:int
  -> line:int
  -> chunks:Msgpack.t list
  -> opts:(Msgpack.t * Msgpack.t) list
  -> int Api_result.t

val nvim_tabpage_list_wins : tabpage:Tabpage.t -> Msgpack.t list Api_result.t
val nvim_tabpage_get_var : tabpage:Tabpage.t -> name:string -> Msgpack.t Api_result.t

val nvim_tabpage_set_var
  :  tabpage:Tabpage.t
  -> name:string
  -> value:Msgpack.t
  -> unit Api_result.t

val nvim_tabpage_del_var : tabpage:Tabpage.t -> name:string -> unit Api_result.t

val tabpage_set_var
  :  tabpage:Tabpage.t
  -> name:string
  -> value:Msgpack.t
  -> Msgpack.t Api_result.t

val tabpage_del_var : tabpage:Tabpage.t -> name:string -> Msgpack.t Api_result.t
val nvim_tabpage_get_win : tabpage:Tabpage.t -> Window.t Api_result.t
val nvim_tabpage_get_number : tabpage:Tabpage.t -> int Api_result.t
val nvim_tabpage_is_valid : tabpage:Tabpage.t -> bool Api_result.t

val nvim_ui_attach
  :  width:int
  -> height:int
  -> options:(Msgpack.t * Msgpack.t) list
  -> unit Api_result.t

val ui_attach : width:int -> height:int -> enable_rgb:bool -> unit Api_result.t
val nvim_ui_detach : unit Api_result.t
val nvim_ui_try_resize : width:int -> height:int -> unit Api_result.t
val nvim_ui_set_option : name:string -> value:Msgpack.t -> unit Api_result.t
val nvim_command : command:string -> unit Api_result.t

val nvim_get_hl_by_name
  :  name:string
  -> rgb:bool
  -> (Msgpack.t * Msgpack.t) list Api_result.t

val nvim_get_hl_by_id : hl_id:int -> rgb:bool -> (Msgpack.t * Msgpack.t) list Api_result.t
val nvim_feedkeys : keys:string -> mode:string -> escape_csi:bool -> unit Api_result.t
val nvim_input : keys:string -> int Api_result.t

val nvim_replace_termcodes
  :  str:string
  -> from_part:bool
  -> do_lt:bool
  -> special:bool
  -> string Api_result.t

val nvim_command_output : command:string -> string Api_result.t
val nvim_eval : expr:string -> Msgpack.t Api_result.t
val nvim_execute_lua : code:string -> args:Msgpack.t list -> Msgpack.t Api_result.t
val nvim_call_function : fn:string -> args:Msgpack.t list -> Msgpack.t Api_result.t

val nvim_call_dict_function
  :  dict:Msgpack.t
  -> fn:string
  -> args:Msgpack.t list
  -> Msgpack.t Api_result.t

val nvim_strwidth : text:string -> int Api_result.t
val nvim_list_runtime_paths : Msgpack.t list Api_result.t
val nvim_set_current_dir : dir:string -> unit Api_result.t
val nvim_get_current_line : string Api_result.t
val nvim_set_current_line : line:string -> unit Api_result.t
val nvim_del_current_line : unit Api_result.t
val nvim_get_var : name:string -> Msgpack.t Api_result.t
val nvim_set_var : name:string -> value:Msgpack.t -> unit Api_result.t
val nvim_del_var : name:string -> unit Api_result.t
val vim_set_var : name:string -> value:Msgpack.t -> Msgpack.t Api_result.t
val vim_del_var : name:string -> Msgpack.t Api_result.t
val nvim_get_vvar : name:string -> Msgpack.t Api_result.t
val nvim_get_option : name:string -> Msgpack.t Api_result.t
val nvim_set_option : name:string -> value:Msgpack.t -> unit Api_result.t
val nvim_out_write : str:string -> unit Api_result.t
val nvim_err_write : str:string -> unit Api_result.t
val nvim_err_writeln : str:string -> unit Api_result.t
val nvim_list_bufs : Msgpack.t list Api_result.t
val nvim_get_current_buf : Buffer.t Api_result.t
val nvim_set_current_buf : buffer:Buffer.t -> unit Api_result.t
val nvim_list_wins : Msgpack.t list Api_result.t
val nvim_get_current_win : Window.t Api_result.t
val nvim_set_current_win : window:Window.t -> unit Api_result.t
val nvim_list_tabpages : Msgpack.t list Api_result.t
val nvim_get_current_tabpage : Tabpage.t Api_result.t
val nvim_set_current_tabpage : tabpage:Tabpage.t -> unit Api_result.t
val nvim_create_namespace : name:string -> int Api_result.t
val nvim_get_namespaces : (Msgpack.t * Msgpack.t) list Api_result.t
val nvim_subscribe : event:string -> unit Api_result.t
val nvim_unsubscribe : event:string -> unit Api_result.t
val nvim_get_color_by_name : name:string -> int Api_result.t
val nvim_get_color_map : (Msgpack.t * Msgpack.t) list Api_result.t
val nvim_get_mode : (Msgpack.t * Msgpack.t) list Api_result.t
val nvim_get_keymap : mode:string -> Msgpack.t list Api_result.t

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

val window_set_var
  :  window:Window.t
  -> name:string
  -> value:Msgpack.t
  -> Msgpack.t Api_result.t

val window_del_var : window:Window.t -> name:string -> Msgpack.t Api_result.t
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
val buffer_line_count : buffer:Buffer.t -> int Api_result.t

val buffer_get_lines
  :  buffer:Buffer.t
  -> start:int
  -> end_:int
  -> strict_indexing:bool
  -> Msgpack.t list Api_result.t

val buffer_set_lines
  :  buffer:Buffer.t
  -> start:int
  -> end_:int
  -> strict_indexing:bool
  -> replacement:Msgpack.t list
  -> unit Api_result.t

val buffer_get_var : buffer:Buffer.t -> name:string -> Msgpack.t Api_result.t
val buffer_get_option : buffer:Buffer.t -> name:string -> Msgpack.t Api_result.t

val buffer_set_option
  :  buffer:Buffer.t
  -> name:string
  -> value:Msgpack.t
  -> unit Api_result.t

val buffer_get_number : buffer:Buffer.t -> int Api_result.t
val buffer_get_name : buffer:Buffer.t -> string Api_result.t
val buffer_set_name : buffer:Buffer.t -> name:string -> unit Api_result.t
val buffer_is_valid : buffer:Buffer.t -> bool Api_result.t
val buffer_get_mark : buffer:Buffer.t -> name:string -> Msgpack.t list Api_result.t

val buffer_add_highlight
  :  buffer:Buffer.t
  -> ns_id:int
  -> hl_group:string
  -> line:int
  -> col_start:int
  -> col_end:int
  -> int Api_result.t

val buffer_clear_highlight
  :  buffer:Buffer.t
  -> ns_id:int
  -> line_start:int
  -> line_end:int
  -> unit Api_result.t

val tabpage_get_windows : tabpage:Tabpage.t -> Msgpack.t list Api_result.t
val tabpage_get_var : tabpage:Tabpage.t -> name:string -> Msgpack.t Api_result.t
val tabpage_get_window : tabpage:Tabpage.t -> Window.t Api_result.t
val tabpage_is_valid : tabpage:Tabpage.t -> bool Api_result.t
val ui_detach : unit Api_result.t
val ui_try_resize : width:int -> height:int -> Msgpack.t Api_result.t
val vim_command : command:string -> unit Api_result.t
val vim_feedkeys : keys:string -> mode:string -> escape_csi:bool -> unit Api_result.t
val vim_input : keys:string -> int Api_result.t

val vim_replace_termcodes
  :  str:string
  -> from_part:bool
  -> do_lt:bool
  -> special:bool
  -> string Api_result.t

val vim_command_output : command:string -> string Api_result.t
val vim_eval : expr:string -> Msgpack.t Api_result.t
val vim_call_function : fn:string -> args:Msgpack.t list -> Msgpack.t Api_result.t
val vim_strwidth : text:string -> int Api_result.t
val vim_list_runtime_paths : Msgpack.t list Api_result.t
val vim_change_directory : dir:string -> unit Api_result.t
val vim_get_current_line : string Api_result.t
val vim_set_current_line : line:string -> unit Api_result.t
val vim_del_current_line : unit Api_result.t
val vim_get_var : name:string -> Msgpack.t Api_result.t
val vim_get_vvar : name:string -> Msgpack.t Api_result.t
val vim_get_option : name:string -> Msgpack.t Api_result.t
val vim_set_option : name:string -> value:Msgpack.t -> unit Api_result.t
val vim_out_write : str:string -> unit Api_result.t
val vim_err_write : str:string -> unit Api_result.t
val vim_report_error : str:string -> unit Api_result.t
val vim_get_buffers : Msgpack.t list Api_result.t
val vim_get_current_buffer : Buffer.t Api_result.t
val vim_set_current_buffer : buffer:Buffer.t -> unit Api_result.t
val vim_get_windows : Msgpack.t list Api_result.t
val vim_get_current_window : Window.t Api_result.t
val vim_set_current_window : window:Window.t -> unit Api_result.t
val vim_get_tabpages : Msgpack.t list Api_result.t
val vim_get_current_tabpage : Tabpage.t Api_result.t
val vim_set_current_tabpage : tabpage:Tabpage.t -> unit Api_result.t
val vim_subscribe : event:string -> unit Api_result.t
val vim_unsubscribe : event:string -> unit Api_result.t
val vim_name_to_color : name:string -> int Api_result.t
val vim_get_color_map : (Msgpack.t * Msgpack.t) list Api_result.t
val vim_get_api_info : Msgpack.t list Api_result.t
val window_get_buffer : window:Window.t -> Buffer.t Api_result.t
val window_get_cursor : window:Window.t -> Msgpack.t list Api_result.t
val window_set_cursor : window:Window.t -> pos:Msgpack.t list -> unit Api_result.t
val window_get_height : window:Window.t -> int Api_result.t
val window_set_height : window:Window.t -> height:int -> unit Api_result.t
val window_get_width : window:Window.t -> int Api_result.t
val window_set_width : window:Window.t -> width:int -> unit Api_result.t
val window_get_var : window:Window.t -> name:string -> Msgpack.t Api_result.t
val window_get_option : window:Window.t -> name:string -> Msgpack.t Api_result.t

val window_set_option
  :  window:Window.t
  -> name:string
  -> value:Msgpack.t
  -> unit Api_result.t

val window_get_position : window:Window.t -> Msgpack.t list Api_result.t
val window_get_tabpage : window:Window.t -> Tabpage.t Api_result.t
val window_is_valid : window:Window.t -> bool Api_result.t
