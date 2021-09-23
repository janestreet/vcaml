open! Core
open Runtime

module Version = struct
  type t =
    { api_compatible : int
    ; api_level : int
    ; major : int
    ; minor : int
    ; patch : int
    }
  [@@deriving sexp_of]

  let to_string t = sprintf "%d.%d.%d" t.major t.minor t.patch
end

let version =
  { Version.api_compatible = 0; api_level = 7; major = 0; minor = 5; patch = 0 }
;;

module Ui_options = struct
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

  let empty =
    { ext_cmdline = false
    ; ext_hlstate = false
    ; ext_linegrid = false
    ; ext_messages = false
    ; ext_multigrid = false
    ; ext_popupmenu = false
    ; ext_tabline = false
    ; ext_termcolors = false
    ; ext_wildmenu = false
    ; rgb = false
    }
  ;;
end

module Luaref = struct
  type t = Msgpack.t

  let to_msgpack = Fn.id
  let of_msgpack t = Ok t
end

module Buffer = (val make_nvim_id ~name:"buffer" ~type_id:0)
module Window = (val make_nvim_id ~name:"window" ~type_id:1)
module Tabpage = (val make_nvim_id ~name:"tabpage" ~type_id:2)

module Phantom = struct
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

  let rec sexp_of_t : type a. (a -> Sexp.t) -> a t -> Sexp.t =
    fun _ t ->
    let ignore _ : Sexp.t = List [] in
    match t with
    | Nil -> Sexp.Atom "Nil"
    | Integer -> Atom "Integer"
    | Boolean -> Atom "Boolean"
    | Float -> Atom "Float"
    | Array arr -> List [ sexp_of_t ignore arr; Atom "ArrayN" ]
    | Tuple (arr, n) -> List [ sexp_of_t ignore arr; Atom (sprintf "Array%d" n) ]
    | Dict -> Atom "Dict"
    | String -> Atom "String"
    | Buffer -> Atom "Buffer"
    | Window -> Atom "Window"
    | Tabpage -> Atom "Tabpage"
    | Luaref -> Atom "Luaref"
    | Object -> Atom "Object"
    | Custom _ -> Atom "Custom"
  ;;
end

module Api_result = struct
  type 'result t =
    { name : string
    ; params : Msgpack.t list
    ; witness : 'result Phantom.t
    }
  [@@deriving sexp_of]
end

module Error_type = struct
  type t =
    | Exception
    | Validation
  [@@deriving sexp_of]

  let of_int = function
    | 0 -> Ok Exception
    | 1 -> Ok Validation
    | id -> Or_error.error_s [%message "Unrecognized error type" (id : int)]
  ;;
end

module Ui_event = struct
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

  let of_msgpack msg =
    match (msg : Msgpack.t) with
    | Array (String name :: calls) ->
      List.map calls ~f:(function
        | Array params ->
          (match name, params with
           | "mode_info_set", [ Boolean enabled; Array cursor_styles ] ->
             Ok (Mode_info_set { enabled; cursor_styles })
           | "update_menu", [] -> Ok Update_menu
           | "busy_start", [] -> Ok Busy_start
           | "busy_stop", [] -> Ok Busy_stop
           | "mouse_on", [] -> Ok Mouse_on
           | "mouse_off", [] -> Ok Mouse_off
           | "mode_change", [ String mode; Integer mode_idx ] ->
             Ok (Mode_change { mode; mode_idx })
           | "bell", [] -> Ok Bell
           | "visual_bell", [] -> Ok Visual_bell
           | "flush", [] -> Ok Flush
           | "suspend", [] -> Ok Suspend
           | "set_title", [ String title ] -> Ok (Set_title { title })
           | "set_icon", [ String icon ] -> Ok (Set_icon { icon })
           | "screenshot", [ String path ] -> Ok (Screenshot { path })
           | "option_set", [ String name; value ] -> Ok (Option_set { name; value })
           | "update_fg", [ Integer fg ] -> Ok (Update_fg { fg })
           | "update_bg", [ Integer bg ] -> Ok (Update_bg { bg })
           | "update_sp", [ Integer sp ] -> Ok (Update_sp { sp })
           | "resize", [ Integer width; Integer height ] -> Ok (Resize { width; height })
           | "clear", [] -> Ok Clear
           | "eol_clear", [] -> Ok Eol_clear
           | "cursor_goto", [ Integer row; Integer col ] -> Ok (Cursor_goto { row; col })
           | "highlight_set", [ Map attrs ] -> Ok (Highlight_set { attrs })
           | "put", [ String str ] -> Ok (Put { str })
           | ( "set_scroll_region"
             , [ Integer top; Integer bot; Integer left; Integer right ] ) ->
             Ok (Set_scroll_region { top; bot; left; right })
           | "scroll", [ Integer count ] -> Ok (Scroll { count })
           | ( "default_colors_set"
             , [ Integer rgb_fg
               ; Integer rgb_bg
               ; Integer rgb_sp
               ; Integer cterm_fg
               ; Integer cterm_bg
               ] ) ->
             Ok (Default_colors_set { rgb_fg; rgb_bg; rgb_sp; cterm_fg; cterm_bg })
           | "hl_attr_define", [ Integer id; Map rgb_attrs; Map cterm_attrs; Array info ]
             -> Ok (Hl_attr_define { id; rgb_attrs; cterm_attrs; info })
           | "hl_group_set", [ String name; Integer id ] ->
             Ok (Hl_group_set { name; id })
           | "grid_resize", [ Integer grid; Integer width; Integer height ] ->
             Ok (Grid_resize { grid; width; height })
           | "grid_clear", [ Integer grid ] -> Ok (Grid_clear { grid })
           | "grid_cursor_goto", [ Integer grid; Integer row; Integer col ] ->
             Ok (Grid_cursor_goto { grid; row; col })
           | "grid_line", [ Integer grid; Integer row; Integer col_start; Array data ] ->
             Ok (Grid_line { grid; row; col_start; data })
           | ( "grid_scroll"
             , [ Integer grid
               ; Integer top
               ; Integer bot
               ; Integer left
               ; Integer right
               ; Integer rows
               ; Integer cols
               ] ) -> Ok (Grid_scroll { grid; top; bot; left; right; rows; cols })
           | "grid_destroy", [ Integer grid ] -> Ok (Grid_destroy { grid })
           | ( "win_pos"
             , [ Integer grid
               ; win
               ; Integer startrow
               ; Integer startcol
               ; Integer width
               ; Integer height
               ] ) ->
             let open Or_error.Let_syntax in
             let%bind win = Window.of_msgpack win in
             return (Win_pos { grid; win; startrow; startcol; width; height })
           | ( "win_float_pos"
             , [ Integer grid
               ; win
               ; String anchor
               ; Integer anchor_grid
               ; Floating anchor_row
               ; Floating anchor_col
               ; Boolean focusable
               ; Integer zindex
               ] ) ->
             let open Or_error.Let_syntax in
             let%bind win = Window.of_msgpack win in
             return
               (Win_float_pos
                  { grid
                  ; win
                  ; anchor
                  ; anchor_grid
                  ; anchor_row
                  ; anchor_col
                  ; focusable
                  ; zindex
                  })
           | "win_external_pos", [ Integer grid; win ] ->
             let open Or_error.Let_syntax in
             let%bind win = Window.of_msgpack win in
             return (Win_external_pos { grid; win })
           | "win_hide", [ Integer grid ] -> Ok (Win_hide { grid })
           | "win_close", [ Integer grid ] -> Ok (Win_close { grid })
           | ( "msg_set_pos"
             , [ Integer grid; Integer row; Boolean scrolled; String sep_char ] ) ->
             Ok (Msg_set_pos { grid; row; scrolled; sep_char })
           | ( "win_viewport"
             , [ Integer grid
               ; win
               ; Integer topline
               ; Integer botline
               ; Integer curline
               ; Integer curcol
               ] ) ->
             let open Or_error.Let_syntax in
             let%bind win = Window.of_msgpack win in
             return (Win_viewport { grid; win; topline; botline; curline; curcol })
           | ( "popupmenu_show"
             , [ Array items; Integer selected; Integer row; Integer col; Integer grid ]
             ) -> Ok (Popupmenu_show { items; selected; row; col; grid })
           | "popupmenu_hide", [] -> Ok Popupmenu_hide
           | "popupmenu_select", [ Integer selected ] ->
             Ok (Popupmenu_select { selected })
           | "tabline_update", [ current; Array tabs; current_buffer; Array buffers ] ->
             let open Or_error.Let_syntax in
             let%bind current = Tabpage.of_msgpack current in
             let%bind current_buffer = Buffer.of_msgpack current_buffer in
             return (Tabline_update { current; tabs; current_buffer; buffers })
           | ( "cmdline_show"
             , [ Array content
               ; Integer pos
               ; String firstc
               ; String prompt
               ; Integer indent
               ; Integer level
               ] ) -> Ok (Cmdline_show { content; pos; firstc; prompt; indent; level })
           | "cmdline_pos", [ Integer pos; Integer level ] ->
             Ok (Cmdline_pos { pos; level })
           | "cmdline_special_char", [ String c; Boolean shift; Integer level ] ->
             Ok (Cmdline_special_char { c; shift; level })
           | "cmdline_hide", [ Integer level ] -> Ok (Cmdline_hide { level })
           | "cmdline_block_show", [ Array lines ] -> Ok (Cmdline_block_show { lines })
           | "cmdline_block_append", [ Array lines ] ->
             Ok (Cmdline_block_append { lines })
           | "cmdline_block_hide", [] -> Ok Cmdline_block_hide
           | "wildmenu_show", [ Array items ] -> Ok (Wildmenu_show { items })
           | "wildmenu_select", [ Integer selected ] -> Ok (Wildmenu_select { selected })
           | "wildmenu_hide", [] -> Ok Wildmenu_hide
           | "msg_show", [ String kind; Array content; Boolean replace_last ] ->
             Ok (Msg_show { kind; content; replace_last })
           | "msg_clear", [] -> Ok Msg_clear
           | "msg_showcmd", [ Array content ] -> Ok (Msg_showcmd { content })
           | "msg_showmode", [ Array content ] -> Ok (Msg_showmode { content })
           | "msg_ruler", [ Array content ] -> Ok (Msg_ruler { content })
           | "msg_history_show", [ Array entries ] -> Ok (Msg_history_show { entries })
           | _ ->
             Or_error.error_s [%message "Failed to parse UI event" (msg : Msgpack.t)])
        | _ -> Or_error.error_s [%message "Failed to parse UI event" (msg : Msgpack.t)])
      |> Or_error.combine_errors
    | _ -> Or_error.error_s [%message "Failed to parse UI event" (msg : Msgpack.t)]
  ;;
end

let nvim_buf_line_count ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "nvim_buf_line_count"; params = [ buffer ]; witness = Integer }
;;

let nvim_buf_attach ~buffer ~send_buffer ~opts =
  let buffer = Buffer.to_msgpack buffer in
  let send_buffer = Msgpack.Boolean send_buffer in
  let opts = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) opts) in
  { Api_result.name = "nvim_buf_attach"
  ; params = [ buffer; send_buffer; opts ]
  ; witness = Boolean
  }
;;

let nvim_buf_detach ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "nvim_buf_detach"; params = [ buffer ]; witness = Boolean }
;;

let nvim_buf_get_lines ~buffer ~start ~end_ ~strict_indexing =
  let buffer = Buffer.to_msgpack buffer in
  let start = Msgpack.Integer start in
  let end_ = Msgpack.Integer end_ in
  let strict_indexing = Msgpack.Boolean strict_indexing in
  { Api_result.name = "nvim_buf_get_lines"
  ; params = [ buffer; start; end_; strict_indexing ]
  ; witness = Array Object
  }
;;

let nvim_buf_set_lines ~buffer ~start ~end_ ~strict_indexing ~replacement =
  let buffer = Buffer.to_msgpack buffer in
  let start = Msgpack.Integer start in
  let end_ = Msgpack.Integer end_ in
  let strict_indexing = Msgpack.Boolean strict_indexing in
  let replacement = Msgpack.Array (List.map ~f:(fun v -> v) replacement) in
  { Api_result.name = "nvim_buf_set_lines"
  ; params = [ buffer; start; end_; strict_indexing; replacement ]
  ; witness = Nil
  }
;;

let nvim_buf_set_text ~buffer ~start_row ~start_col ~end_row ~end_col ~replacement =
  let buffer = Buffer.to_msgpack buffer in
  let start_row = Msgpack.Integer start_row in
  let start_col = Msgpack.Integer start_col in
  let end_row = Msgpack.Integer end_row in
  let end_col = Msgpack.Integer end_col in
  let replacement = Msgpack.Array (List.map ~f:(fun v -> v) replacement) in
  { Api_result.name = "nvim_buf_set_text"
  ; params = [ buffer; start_row; start_col; end_row; end_col; replacement ]
  ; witness = Nil
  }
;;

let nvim_buf_get_offset ~buffer ~index =
  let buffer = Buffer.to_msgpack buffer in
  let index = Msgpack.Integer index in
  { Api_result.name = "nvim_buf_get_offset"
  ; params = [ buffer; index ]
  ; witness = Integer
  }
;;

let nvim_buf_get_var ~buffer ~name =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  { Api_result.name = "nvim_buf_get_var"; params = [ buffer; name ]; witness = Object }
;;

let nvim_buf_get_changedtick ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "nvim_buf_get_changedtick"; params = [ buffer ]; witness = Integer }
;;

let nvim_buf_get_keymap ~buffer ~mode =
  let buffer = Buffer.to_msgpack buffer in
  let mode = Msgpack.String mode in
  { Api_result.name = "nvim_buf_get_keymap"
  ; params = [ buffer; mode ]
  ; witness = Array Object
  }
;;

let nvim_buf_set_keymap ~buffer ~mode ~lhs ~rhs ~opts =
  let buffer = Buffer.to_msgpack buffer in
  let mode = Msgpack.String mode in
  let lhs = Msgpack.String lhs in
  let rhs = Msgpack.String rhs in
  let opts = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) opts) in
  { Api_result.name = "nvim_buf_set_keymap"
  ; params = [ buffer; mode; lhs; rhs; opts ]
  ; witness = Nil
  }
;;

let nvim_buf_del_keymap ~buffer ~mode ~lhs =
  let buffer = Buffer.to_msgpack buffer in
  let mode = Msgpack.String mode in
  let lhs = Msgpack.String lhs in
  { Api_result.name = "nvim_buf_del_keymap"
  ; params = [ buffer; mode; lhs ]
  ; witness = Nil
  }
;;

let nvim_buf_get_commands ~buffer ~opts =
  let buffer = Buffer.to_msgpack buffer in
  let opts = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) opts) in
  { Api_result.name = "nvim_buf_get_commands"; params = [ buffer; opts ]; witness = Dict }
;;

let nvim_buf_set_var ~buffer ~name ~value =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "nvim_buf_set_var"
  ; params = [ buffer; name; value ]
  ; witness = Nil
  }
;;

let nvim_buf_del_var ~buffer ~name =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  { Api_result.name = "nvim_buf_del_var"; params = [ buffer; name ]; witness = Nil }
;;

let nvim_buf_get_option ~buffer ~name =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  { Api_result.name = "nvim_buf_get_option"; params = [ buffer; name ]; witness = Object }
;;

let nvim_buf_set_option ~buffer ~name ~value =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "nvim_buf_set_option"
  ; params = [ buffer; name; value ]
  ; witness = Nil
  }
;;

let nvim_buf_get_name ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "nvim_buf_get_name"; params = [ buffer ]; witness = String }
;;

let nvim_buf_set_name ~buffer ~name =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  { Api_result.name = "nvim_buf_set_name"; params = [ buffer; name ]; witness = Nil }
;;

let nvim_buf_is_loaded ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "nvim_buf_is_loaded"; params = [ buffer ]; witness = Boolean }
;;

let nvim_buf_delete ~buffer ~opts =
  let buffer = Buffer.to_msgpack buffer in
  let opts = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) opts) in
  { Api_result.name = "nvim_buf_delete"; params = [ buffer; opts ]; witness = Nil }
;;

let nvim_buf_is_valid ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "nvim_buf_is_valid"; params = [ buffer ]; witness = Boolean }
;;

let nvim_buf_get_mark ~buffer ~name =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  { Api_result.name = "nvim_buf_get_mark"
  ; params = [ buffer; name ]
  ; witness = Array Object
  }
;;

let nvim_buf_get_extmark_by_id ~buffer ~ns_id ~id ~opts =
  let buffer = Buffer.to_msgpack buffer in
  let ns_id = Msgpack.Integer ns_id in
  let id = Msgpack.Integer id in
  let opts = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) opts) in
  { Api_result.name = "nvim_buf_get_extmark_by_id"
  ; params = [ buffer; ns_id; id; opts ]
  ; witness = Array Object
  }
;;

let nvim_buf_get_extmarks ~buffer ~ns_id ~start ~end_ ~opts =
  let buffer = Buffer.to_msgpack buffer in
  let ns_id = Msgpack.Integer ns_id in
  let start = start in
  let end_ = end_ in
  let opts = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) opts) in
  { Api_result.name = "nvim_buf_get_extmarks"
  ; params = [ buffer; ns_id; start; end_; opts ]
  ; witness = Array Object
  }
;;

let nvim_buf_set_extmark ~buffer ~ns_id ~line ~col ~opts =
  let buffer = Buffer.to_msgpack buffer in
  let ns_id = Msgpack.Integer ns_id in
  let line = Msgpack.Integer line in
  let col = Msgpack.Integer col in
  let opts = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) opts) in
  { Api_result.name = "nvim_buf_set_extmark"
  ; params = [ buffer; ns_id; line; col; opts ]
  ; witness = Integer
  }
;;

let nvim_buf_del_extmark ~buffer ~ns_id ~id =
  let buffer = Buffer.to_msgpack buffer in
  let ns_id = Msgpack.Integer ns_id in
  let id = Msgpack.Integer id in
  { Api_result.name = "nvim_buf_del_extmark"
  ; params = [ buffer; ns_id; id ]
  ; witness = Boolean
  }
;;

let nvim_buf_add_highlight ~buffer ~ns_id ~hl_group ~line ~col_start ~col_end =
  let buffer = Buffer.to_msgpack buffer in
  let ns_id = Msgpack.Integer ns_id in
  let hl_group = Msgpack.String hl_group in
  let line = Msgpack.Integer line in
  let col_start = Msgpack.Integer col_start in
  let col_end = Msgpack.Integer col_end in
  { Api_result.name = "nvim_buf_add_highlight"
  ; params = [ buffer; ns_id; hl_group; line; col_start; col_end ]
  ; witness = Integer
  }
;;

let nvim_buf_clear_namespace ~buffer ~ns_id ~line_start ~line_end =
  let buffer = Buffer.to_msgpack buffer in
  let ns_id = Msgpack.Integer ns_id in
  let line_start = Msgpack.Integer line_start in
  let line_end = Msgpack.Integer line_end in
  { Api_result.name = "nvim_buf_clear_namespace"
  ; params = [ buffer; ns_id; line_start; line_end ]
  ; witness = Nil
  }
;;

let nvim_buf_set_virtual_text ~buffer ~src_id ~line ~chunks ~opts =
  let buffer = Buffer.to_msgpack buffer in
  let src_id = Msgpack.Integer src_id in
  let line = Msgpack.Integer line in
  let chunks = Msgpack.Array (List.map ~f:(fun v -> v) chunks) in
  let opts = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) opts) in
  { Api_result.name = "nvim_buf_set_virtual_text"
  ; params = [ buffer; src_id; line; chunks; opts ]
  ; witness = Integer
  }
;;

let nvim_buf_call ~buffer ~fun_ =
  let buffer = Buffer.to_msgpack buffer in
  let fun_ = Luaref.to_msgpack fun_ in
  { Api_result.name = "nvim_buf_call"; params = [ buffer; fun_ ]; witness = Object }
;;

let nvim_command_output ~command =
  let command = Msgpack.String command in
  { Api_result.name = "nvim_command_output"; params = [ command ]; witness = String }
;;

let nvim_execute_lua ~code ~args =
  let code = Msgpack.String code in
  let args = Msgpack.Array (List.map ~f:(fun v -> v) args) in
  { Api_result.name = "nvim_execute_lua"; params = [ code; args ]; witness = Object }
;;

let nvim_buf_get_number ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "nvim_buf_get_number"; params = [ buffer ]; witness = Integer }
;;

let nvim_buf_clear_highlight ~buffer ~ns_id ~line_start ~line_end =
  let buffer = Buffer.to_msgpack buffer in
  let ns_id = Msgpack.Integer ns_id in
  let line_start = Msgpack.Integer line_start in
  let line_end = Msgpack.Integer line_end in
  { Api_result.name = "nvim_buf_clear_highlight"
  ; params = [ buffer; ns_id; line_start; line_end ]
  ; witness = Nil
  }
;;

let buffer_insert ~buffer ~lnum ~lines =
  let buffer = Buffer.to_msgpack buffer in
  let lnum = Msgpack.Integer lnum in
  let lines = Msgpack.Array (List.map ~f:(fun v -> v) lines) in
  { Api_result.name = "buffer_insert"; params = [ buffer; lnum; lines ]; witness = Nil }
;;

let buffer_get_line ~buffer ~index =
  let buffer = Buffer.to_msgpack buffer in
  let index = Msgpack.Integer index in
  { Api_result.name = "buffer_get_line"; params = [ buffer; index ]; witness = String }
;;

let buffer_set_line ~buffer ~index ~line =
  let buffer = Buffer.to_msgpack buffer in
  let index = Msgpack.Integer index in
  let line = Msgpack.String line in
  { Api_result.name = "buffer_set_line"; params = [ buffer; index; line ]; witness = Nil }
;;

let buffer_del_line ~buffer ~index =
  let buffer = Buffer.to_msgpack buffer in
  let index = Msgpack.Integer index in
  { Api_result.name = "buffer_del_line"; params = [ buffer; index ]; witness = Nil }
;;

let buffer_get_line_slice ~buffer ~start ~end_ ~include_start ~include_end =
  let buffer = Buffer.to_msgpack buffer in
  let start = Msgpack.Integer start in
  let end_ = Msgpack.Integer end_ in
  let include_start = Msgpack.Boolean include_start in
  let include_end = Msgpack.Boolean include_end in
  { Api_result.name = "buffer_get_line_slice"
  ; params = [ buffer; start; end_; include_start; include_end ]
  ; witness = Array Object
  }
;;

let buffer_set_line_slice ~buffer ~start ~end_ ~include_start ~include_end ~replacement =
  let buffer = Buffer.to_msgpack buffer in
  let start = Msgpack.Integer start in
  let end_ = Msgpack.Integer end_ in
  let include_start = Msgpack.Boolean include_start in
  let include_end = Msgpack.Boolean include_end in
  let replacement = Msgpack.Array (List.map ~f:(fun v -> v) replacement) in
  { Api_result.name = "buffer_set_line_slice"
  ; params = [ buffer; start; end_; include_start; include_end; replacement ]
  ; witness = Nil
  }
;;

let buffer_set_var ~buffer ~name ~value =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "buffer_set_var"
  ; params = [ buffer; name; value ]
  ; witness = Object
  }
;;

let buffer_del_var ~buffer ~name =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  { Api_result.name = "buffer_del_var"; params = [ buffer; name ]; witness = Object }
;;

let window_set_var ~window ~name ~value =
  let window = Window.to_msgpack window in
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "window_set_var"
  ; params = [ window; name; value ]
  ; witness = Object
  }
;;

let window_del_var ~window ~name =
  let window = Window.to_msgpack window in
  let name = Msgpack.String name in
  { Api_result.name = "window_del_var"; params = [ window; name ]; witness = Object }
;;

let tabpage_set_var ~tabpage ~name ~value =
  let tabpage = Tabpage.to_msgpack tabpage in
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "tabpage_set_var"
  ; params = [ tabpage; name; value ]
  ; witness = Object
  }
;;

let tabpage_del_var ~tabpage ~name =
  let tabpage = Tabpage.to_msgpack tabpage in
  let name = Msgpack.String name in
  { Api_result.name = "tabpage_del_var"; params = [ tabpage; name ]; witness = Object }
;;

let vim_set_var ~name ~value =
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "vim_set_var"; params = [ name; value ]; witness = Object }
;;

let vim_del_var ~name =
  let name = Msgpack.String name in
  { Api_result.name = "vim_del_var"; params = [ name ]; witness = Object }
;;

let nvim_tabpage_list_wins ~tabpage =
  let tabpage = Tabpage.to_msgpack tabpage in
  { Api_result.name = "nvim_tabpage_list_wins"
  ; params = [ tabpage ]
  ; witness = Array Object
  }
;;

let nvim_tabpage_get_var ~tabpage ~name =
  let tabpage = Tabpage.to_msgpack tabpage in
  let name = Msgpack.String name in
  { Api_result.name = "nvim_tabpage_get_var"
  ; params = [ tabpage; name ]
  ; witness = Object
  }
;;

let nvim_tabpage_set_var ~tabpage ~name ~value =
  let tabpage = Tabpage.to_msgpack tabpage in
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "nvim_tabpage_set_var"
  ; params = [ tabpage; name; value ]
  ; witness = Nil
  }
;;

let nvim_tabpage_del_var ~tabpage ~name =
  let tabpage = Tabpage.to_msgpack tabpage in
  let name = Msgpack.String name in
  { Api_result.name = "nvim_tabpage_del_var"; params = [ tabpage; name ]; witness = Nil }
;;

let nvim_tabpage_get_win ~tabpage =
  let tabpage = Tabpage.to_msgpack tabpage in
  { Api_result.name = "nvim_tabpage_get_win"; params = [ tabpage ]; witness = Window }
;;

let nvim_tabpage_get_number ~tabpage =
  let tabpage = Tabpage.to_msgpack tabpage in
  { Api_result.name = "nvim_tabpage_get_number"; params = [ tabpage ]; witness = Integer }
;;

let nvim_tabpage_is_valid ~tabpage =
  let tabpage = Tabpage.to_msgpack tabpage in
  { Api_result.name = "nvim_tabpage_is_valid"; params = [ tabpage ]; witness = Boolean }
;;

let nvim_ui_attach ~width ~height ~options =
  let width = Msgpack.Integer width in
  let height = Msgpack.Integer height in
  let options = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) options) in
  { Api_result.name = "nvim_ui_attach"
  ; params = [ width; height; options ]
  ; witness = Nil
  }
;;

let ui_attach ~width ~height ~enable_rgb =
  let width = Msgpack.Integer width in
  let height = Msgpack.Integer height in
  let enable_rgb = Msgpack.Boolean enable_rgb in
  { Api_result.name = "ui_attach"; params = [ width; height; enable_rgb ]; witness = Nil }
;;

let nvim_ui_detach = { Api_result.name = "nvim_ui_detach"; params = []; witness = Nil }

let nvim_ui_try_resize ~width ~height =
  let width = Msgpack.Integer width in
  let height = Msgpack.Integer height in
  { Api_result.name = "nvim_ui_try_resize"; params = [ width; height ]; witness = Nil }
;;

let nvim_ui_set_option ~name ~value =
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "nvim_ui_set_option"; params = [ name; value ]; witness = Nil }
;;

let nvim_ui_try_resize_grid ~grid ~width ~height =
  let grid = Msgpack.Integer grid in
  let width = Msgpack.Integer width in
  let height = Msgpack.Integer height in
  { Api_result.name = "nvim_ui_try_resize_grid"
  ; params = [ grid; width; height ]
  ; witness = Nil
  }
;;

let nvim_ui_pum_set_height ~height =
  let height = Msgpack.Integer height in
  { Api_result.name = "nvim_ui_pum_set_height"; params = [ height ]; witness = Nil }
;;

let nvim_ui_pum_set_bounds ~width ~height ~row ~col =
  let width = Msgpack.Floating width in
  let height = Msgpack.Floating height in
  let row = Msgpack.Floating row in
  let col = Msgpack.Floating col in
  { Api_result.name = "nvim_ui_pum_set_bounds"
  ; params = [ width; height; row; col ]
  ; witness = Nil
  }
;;

let nvim_exec ~src ~output =
  let src = Msgpack.String src in
  let output = Msgpack.Boolean output in
  { Api_result.name = "nvim_exec"; params = [ src; output ]; witness = String }
;;

let nvim_command ~command =
  let command = Msgpack.String command in
  { Api_result.name = "nvim_command"; params = [ command ]; witness = Nil }
;;

let nvim_get_hl_by_name ~name ~rgb =
  let name = Msgpack.String name in
  let rgb = Msgpack.Boolean rgb in
  { Api_result.name = "nvim_get_hl_by_name"; params = [ name; rgb ]; witness = Dict }
;;

let nvim_get_hl_by_id ~hl_id ~rgb =
  let hl_id = Msgpack.Integer hl_id in
  let rgb = Msgpack.Boolean rgb in
  { Api_result.name = "nvim_get_hl_by_id"; params = [ hl_id; rgb ]; witness = Dict }
;;

let nvim_get_hl_id_by_name ~name =
  let name = Msgpack.String name in
  { Api_result.name = "nvim_get_hl_id_by_name"; params = [ name ]; witness = Integer }
;;

let nvim_set_hl ~ns_id ~name ~val_ =
  let ns_id = Msgpack.Integer ns_id in
  let name = Msgpack.String name in
  let val_ = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) val_) in
  { Api_result.name = "nvim_set_hl"; params = [ ns_id; name; val_ ]; witness = Nil }
;;

let nvim_feedkeys ~keys ~mode ~escape_csi =
  let keys = Msgpack.String keys in
  let mode = Msgpack.String mode in
  let escape_csi = Msgpack.Boolean escape_csi in
  { Api_result.name = "nvim_feedkeys"
  ; params = [ keys; mode; escape_csi ]
  ; witness = Nil
  }
;;

let nvim_input ~keys =
  let keys = Msgpack.String keys in
  { Api_result.name = "nvim_input"; params = [ keys ]; witness = Integer }
;;

let nvim_input_mouse ~button ~action ~modifier ~grid ~row ~col =
  let button = Msgpack.String button in
  let action = Msgpack.String action in
  let modifier = Msgpack.String modifier in
  let grid = Msgpack.Integer grid in
  let row = Msgpack.Integer row in
  let col = Msgpack.Integer col in
  { Api_result.name = "nvim_input_mouse"
  ; params = [ button; action; modifier; grid; row; col ]
  ; witness = Nil
  }
;;

let nvim_replace_termcodes ~str ~from_part ~do_lt ~special =
  let str = Msgpack.String str in
  let from_part = Msgpack.Boolean from_part in
  let do_lt = Msgpack.Boolean do_lt in
  let special = Msgpack.Boolean special in
  { Api_result.name = "nvim_replace_termcodes"
  ; params = [ str; from_part; do_lt; special ]
  ; witness = String
  }
;;

let nvim_eval ~expr =
  let expr = Msgpack.String expr in
  { Api_result.name = "nvim_eval"; params = [ expr ]; witness = Object }
;;

let nvim_exec_lua ~code ~args =
  let code = Msgpack.String code in
  let args = Msgpack.Array (List.map ~f:(fun v -> v) args) in
  { Api_result.name = "nvim_exec_lua"; params = [ code; args ]; witness = Object }
;;

let nvim_notify ~msg ~log_level ~opts =
  let msg = Msgpack.String msg in
  let log_level = Msgpack.Integer log_level in
  let opts = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) opts) in
  { Api_result.name = "nvim_notify"; params = [ msg; log_level; opts ]; witness = Object }
;;

let nvim_call_function ~fn ~args =
  let fn = Msgpack.String fn in
  let args = Msgpack.Array (List.map ~f:(fun v -> v) args) in
  { Api_result.name = "nvim_call_function"; params = [ fn; args ]; witness = Object }
;;

let nvim_call_dict_function ~dict ~fn ~args =
  let dict = dict in
  let fn = Msgpack.String fn in
  let args = Msgpack.Array (List.map ~f:(fun v -> v) args) in
  { Api_result.name = "nvim_call_dict_function"
  ; params = [ dict; fn; args ]
  ; witness = Object
  }
;;

let nvim_strwidth ~text =
  let text = Msgpack.String text in
  { Api_result.name = "nvim_strwidth"; params = [ text ]; witness = Integer }
;;

let nvim_list_runtime_paths =
  { Api_result.name = "nvim_list_runtime_paths"; params = []; witness = Array Object }
;;

let nvim_get_runtime_file ~name ~all =
  let name = Msgpack.String name in
  let all = Msgpack.Boolean all in
  { Api_result.name = "nvim_get_runtime_file"
  ; params = [ name; all ]
  ; witness = Array Object
  }
;;

let nvim_set_current_dir ~dir =
  let dir = Msgpack.String dir in
  { Api_result.name = "nvim_set_current_dir"; params = [ dir ]; witness = Nil }
;;

let nvim_get_current_line =
  { Api_result.name = "nvim_get_current_line"; params = []; witness = String }
;;

let nvim_set_current_line ~line =
  let line = Msgpack.String line in
  { Api_result.name = "nvim_set_current_line"; params = [ line ]; witness = Nil }
;;

let nvim_del_current_line =
  { Api_result.name = "nvim_del_current_line"; params = []; witness = Nil }
;;

let nvim_get_var ~name =
  let name = Msgpack.String name in
  { Api_result.name = "nvim_get_var"; params = [ name ]; witness = Object }
;;

let nvim_set_var ~name ~value =
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "nvim_set_var"; params = [ name; value ]; witness = Nil }
;;

let nvim_del_var ~name =
  let name = Msgpack.String name in
  { Api_result.name = "nvim_del_var"; params = [ name ]; witness = Nil }
;;

let nvim_get_vvar ~name =
  let name = Msgpack.String name in
  { Api_result.name = "nvim_get_vvar"; params = [ name ]; witness = Object }
;;

let nvim_set_vvar ~name ~value =
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "nvim_set_vvar"; params = [ name; value ]; witness = Nil }
;;

let nvim_get_option ~name =
  let name = Msgpack.String name in
  { Api_result.name = "nvim_get_option"; params = [ name ]; witness = Object }
;;

let nvim_get_all_options_info =
  { Api_result.name = "nvim_get_all_options_info"; params = []; witness = Dict }
;;

let nvim_get_option_info ~name =
  let name = Msgpack.String name in
  { Api_result.name = "nvim_get_option_info"; params = [ name ]; witness = Dict }
;;

let nvim_set_option ~name ~value =
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "nvim_set_option"; params = [ name; value ]; witness = Nil }
;;

let nvim_echo ~chunks ~history ~opts =
  let chunks = Msgpack.Array (List.map ~f:(fun v -> v) chunks) in
  let history = Msgpack.Boolean history in
  let opts = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) opts) in
  { Api_result.name = "nvim_echo"; params = [ chunks; history; opts ]; witness = Nil }
;;

let nvim_out_write ~str =
  let str = Msgpack.String str in
  { Api_result.name = "nvim_out_write"; params = [ str ]; witness = Nil }
;;

let nvim_err_write ~str =
  let str = Msgpack.String str in
  { Api_result.name = "nvim_err_write"; params = [ str ]; witness = Nil }
;;

let nvim_err_writeln ~str =
  let str = Msgpack.String str in
  { Api_result.name = "nvim_err_writeln"; params = [ str ]; witness = Nil }
;;

let nvim_list_bufs =
  { Api_result.name = "nvim_list_bufs"; params = []; witness = Array Object }
;;

let nvim_get_current_buf =
  { Api_result.name = "nvim_get_current_buf"; params = []; witness = Buffer }
;;

let nvim_set_current_buf ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "nvim_set_current_buf"; params = [ buffer ]; witness = Nil }
;;

let nvim_list_wins =
  { Api_result.name = "nvim_list_wins"; params = []; witness = Array Object }
;;

let nvim_get_current_win =
  { Api_result.name = "nvim_get_current_win"; params = []; witness = Window }
;;

let nvim_set_current_win ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "nvim_set_current_win"; params = [ window ]; witness = Nil }
;;

let nvim_create_buf ~listed ~scratch =
  let listed = Msgpack.Boolean listed in
  let scratch = Msgpack.Boolean scratch in
  { Api_result.name = "nvim_create_buf"; params = [ listed; scratch ]; witness = Buffer }
;;

let nvim_open_term ~buffer ~opts =
  let buffer = Buffer.to_msgpack buffer in
  let opts = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) opts) in
  { Api_result.name = "nvim_open_term"; params = [ buffer; opts ]; witness = Integer }
;;

let nvim_chan_send ~chan ~data =
  let chan = Msgpack.Integer chan in
  let data = Msgpack.String data in
  { Api_result.name = "nvim_chan_send"; params = [ chan; data ]; witness = Nil }
;;

let nvim_open_win ~buffer ~enter ~config =
  let buffer = Buffer.to_msgpack buffer in
  let enter = Msgpack.Boolean enter in
  let config = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) config) in
  { Api_result.name = "nvim_open_win"
  ; params = [ buffer; enter; config ]
  ; witness = Window
  }
;;

let nvim_list_tabpages =
  { Api_result.name = "nvim_list_tabpages"; params = []; witness = Array Object }
;;

let nvim_get_current_tabpage =
  { Api_result.name = "nvim_get_current_tabpage"; params = []; witness = Tabpage }
;;

let nvim_set_current_tabpage ~tabpage =
  let tabpage = Tabpage.to_msgpack tabpage in
  { Api_result.name = "nvim_set_current_tabpage"; params = [ tabpage ]; witness = Nil }
;;

let nvim_create_namespace ~name =
  let name = Msgpack.String name in
  { Api_result.name = "nvim_create_namespace"; params = [ name ]; witness = Integer }
;;

let nvim_get_namespaces =
  { Api_result.name = "nvim_get_namespaces"; params = []; witness = Dict }
;;

let nvim_paste ~data ~crlf ~phase =
  let data = Msgpack.String data in
  let crlf = Msgpack.Boolean crlf in
  let phase = Msgpack.Integer phase in
  { Api_result.name = "nvim_paste"; params = [ data; crlf; phase ]; witness = Boolean }
;;

let nvim_put ~lines ~type_ ~after ~follow =
  let lines = Msgpack.Array (List.map ~f:(fun v -> v) lines) in
  let type_ = Msgpack.String type_ in
  let after = Msgpack.Boolean after in
  let follow = Msgpack.Boolean follow in
  { Api_result.name = "nvim_put"
  ; params = [ lines; type_; after; follow ]
  ; witness = Nil
  }
;;

let nvim_subscribe ~event =
  let event = Msgpack.String event in
  { Api_result.name = "nvim_subscribe"; params = [ event ]; witness = Nil }
;;

let nvim_unsubscribe ~event =
  let event = Msgpack.String event in
  { Api_result.name = "nvim_unsubscribe"; params = [ event ]; witness = Nil }
;;

let nvim_get_color_by_name ~name =
  let name = Msgpack.String name in
  { Api_result.name = "nvim_get_color_by_name"; params = [ name ]; witness = Integer }
;;

let nvim_get_color_map =
  { Api_result.name = "nvim_get_color_map"; params = []; witness = Dict }
;;

let nvim_get_context ~opts =
  let opts = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) opts) in
  { Api_result.name = "nvim_get_context"; params = [ opts ]; witness = Dict }
;;

let nvim_load_context ~dict =
  let dict = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) dict) in
  { Api_result.name = "nvim_load_context"; params = [ dict ]; witness = Object }
;;

let nvim_get_mode = { Api_result.name = "nvim_get_mode"; params = []; witness = Dict }

let nvim_get_keymap ~mode =
  let mode = Msgpack.String mode in
  { Api_result.name = "nvim_get_keymap"; params = [ mode ]; witness = Array Object }
;;

let nvim_set_keymap ~mode ~lhs ~rhs ~opts =
  let mode = Msgpack.String mode in
  let lhs = Msgpack.String lhs in
  let rhs = Msgpack.String rhs in
  let opts = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) opts) in
  { Api_result.name = "nvim_set_keymap"
  ; params = [ mode; lhs; rhs; opts ]
  ; witness = Nil
  }
;;

let nvim_del_keymap ~mode ~lhs =
  let mode = Msgpack.String mode in
  let lhs = Msgpack.String lhs in
  { Api_result.name = "nvim_del_keymap"; params = [ mode; lhs ]; witness = Nil }
;;

let nvim_get_commands ~opts =
  let opts = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) opts) in
  { Api_result.name = "nvim_get_commands"; params = [ opts ]; witness = Dict }
;;

let nvim_get_api_info =
  { Api_result.name = "nvim_get_api_info"; params = []; witness = Array Object }
;;

let nvim_set_client_info ~name ~version ~type_ ~methods ~attributes =
  let name = Msgpack.String name in
  let version = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) version) in
  let type_ = Msgpack.String type_ in
  let methods = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) methods) in
  let attributes = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) attributes) in
  { Api_result.name = "nvim_set_client_info"
  ; params = [ name; version; type_; methods; attributes ]
  ; witness = Nil
  }
;;

let nvim_get_chan_info ~chan =
  let chan = Msgpack.Integer chan in
  { Api_result.name = "nvim_get_chan_info"; params = [ chan ]; witness = Dict }
;;

let nvim_list_chans =
  { Api_result.name = "nvim_list_chans"; params = []; witness = Array Object }
;;

let nvim_call_atomic ~calls =
  let calls = Msgpack.Array (List.map ~f:(fun v -> v) calls) in
  { Api_result.name = "nvim_call_atomic"; params = [ calls ]; witness = Array Object }
;;

let nvim_parse_expression ~expr ~flags ~highlight =
  let expr = Msgpack.String expr in
  let flags = Msgpack.String flags in
  let highlight = Msgpack.Boolean highlight in
  { Api_result.name = "nvim_parse_expression"
  ; params = [ expr; flags; highlight ]
  ; witness = Dict
  }
;;

let nvim_list_uis =
  { Api_result.name = "nvim_list_uis"; params = []; witness = Array Object }
;;

let nvim_get_proc_children ~pid =
  let pid = Msgpack.Integer pid in
  { Api_result.name = "nvim_get_proc_children"; params = [ pid ]; witness = Array Object }
;;

let nvim_get_proc ~pid =
  let pid = Msgpack.Integer pid in
  { Api_result.name = "nvim_get_proc"; params = [ pid ]; witness = Object }
;;

let nvim_select_popupmenu_item ~item ~insert ~finish ~opts =
  let item = Msgpack.Integer item in
  let insert = Msgpack.Boolean insert in
  let finish = Msgpack.Boolean finish in
  let opts = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) opts) in
  { Api_result.name = "nvim_select_popupmenu_item"
  ; params = [ item; insert; finish; opts ]
  ; witness = Nil
  }
;;

let nvim_set_decoration_provider ~ns_id ~opts =
  let ns_id = Msgpack.Integer ns_id in
  let opts = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) opts) in
  { Api_result.name = "nvim_set_decoration_provider"
  ; params = [ ns_id; opts ]
  ; witness = Nil
  }
;;

let nvim_win_get_buf ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "nvim_win_get_buf"; params = [ window ]; witness = Buffer }
;;

let nvim_win_set_buf ~window ~buffer =
  let window = Window.to_msgpack window in
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "nvim_win_set_buf"; params = [ window; buffer ]; witness = Nil }
;;

let nvim_win_get_cursor ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "nvim_win_get_cursor"; params = [ window ]; witness = Array Object }
;;

let nvim_win_set_cursor ~window ~pos =
  let window = Window.to_msgpack window in
  let pos = Msgpack.Array (List.map ~f:(fun v -> v) pos) in
  { Api_result.name = "nvim_win_set_cursor"; params = [ window; pos ]; witness = Nil }
;;

let nvim_win_get_height ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "nvim_win_get_height"; params = [ window ]; witness = Integer }
;;

let nvim_win_set_height ~window ~height =
  let window = Window.to_msgpack window in
  let height = Msgpack.Integer height in
  { Api_result.name = "nvim_win_set_height"; params = [ window; height ]; witness = Nil }
;;

let nvim_win_get_width ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "nvim_win_get_width"; params = [ window ]; witness = Integer }
;;

let nvim_win_set_width ~window ~width =
  let window = Window.to_msgpack window in
  let width = Msgpack.Integer width in
  { Api_result.name = "nvim_win_set_width"; params = [ window; width ]; witness = Nil }
;;

let nvim_win_get_var ~window ~name =
  let window = Window.to_msgpack window in
  let name = Msgpack.String name in
  { Api_result.name = "nvim_win_get_var"; params = [ window; name ]; witness = Object }
;;

let nvim_win_set_var ~window ~name ~value =
  let window = Window.to_msgpack window in
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "nvim_win_set_var"
  ; params = [ window; name; value ]
  ; witness = Nil
  }
;;

let nvim_win_del_var ~window ~name =
  let window = Window.to_msgpack window in
  let name = Msgpack.String name in
  { Api_result.name = "nvim_win_del_var"; params = [ window; name ]; witness = Nil }
;;

let nvim_win_get_option ~window ~name =
  let window = Window.to_msgpack window in
  let name = Msgpack.String name in
  { Api_result.name = "nvim_win_get_option"; params = [ window; name ]; witness = Object }
;;

let nvim_win_set_option ~window ~name ~value =
  let window = Window.to_msgpack window in
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "nvim_win_set_option"
  ; params = [ window; name; value ]
  ; witness = Nil
  }
;;

let nvim_win_get_position ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "nvim_win_get_position"
  ; params = [ window ]
  ; witness = Array Object
  }
;;

let nvim_win_get_tabpage ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "nvim_win_get_tabpage"; params = [ window ]; witness = Tabpage }
;;

let nvim_win_get_number ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "nvim_win_get_number"; params = [ window ]; witness = Integer }
;;

let nvim_win_is_valid ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "nvim_win_is_valid"; params = [ window ]; witness = Boolean }
;;

let nvim_win_set_config ~window ~config =
  let window = Window.to_msgpack window in
  let config = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) config) in
  { Api_result.name = "nvim_win_set_config"; params = [ window; config ]; witness = Nil }
;;

let nvim_win_get_config ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "nvim_win_get_config"; params = [ window ]; witness = Dict }
;;

let nvim_win_hide ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "nvim_win_hide"; params = [ window ]; witness = Nil }
;;

let nvim_win_close ~window ~force =
  let window = Window.to_msgpack window in
  let force = Msgpack.Boolean force in
  { Api_result.name = "nvim_win_close"; params = [ window; force ]; witness = Nil }
;;

let nvim_win_call ~window ~fun_ =
  let window = Window.to_msgpack window in
  let fun_ = Luaref.to_msgpack fun_ in
  { Api_result.name = "nvim_win_call"; params = [ window; fun_ ]; witness = Object }
;;

let buffer_line_count ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "buffer_line_count"; params = [ buffer ]; witness = Integer }
;;

let buffer_get_lines ~buffer ~start ~end_ ~strict_indexing =
  let buffer = Buffer.to_msgpack buffer in
  let start = Msgpack.Integer start in
  let end_ = Msgpack.Integer end_ in
  let strict_indexing = Msgpack.Boolean strict_indexing in
  { Api_result.name = "buffer_get_lines"
  ; params = [ buffer; start; end_; strict_indexing ]
  ; witness = Array Object
  }
;;

let buffer_set_lines ~buffer ~start ~end_ ~strict_indexing ~replacement =
  let buffer = Buffer.to_msgpack buffer in
  let start = Msgpack.Integer start in
  let end_ = Msgpack.Integer end_ in
  let strict_indexing = Msgpack.Boolean strict_indexing in
  let replacement = Msgpack.Array (List.map ~f:(fun v -> v) replacement) in
  { Api_result.name = "buffer_set_lines"
  ; params = [ buffer; start; end_; strict_indexing; replacement ]
  ; witness = Nil
  }
;;

let buffer_get_var ~buffer ~name =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  { Api_result.name = "buffer_get_var"; params = [ buffer; name ]; witness = Object }
;;

let buffer_get_option ~buffer ~name =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  { Api_result.name = "buffer_get_option"; params = [ buffer; name ]; witness = Object }
;;

let buffer_set_option ~buffer ~name ~value =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "buffer_set_option"
  ; params = [ buffer; name; value ]
  ; witness = Nil
  }
;;

let buffer_get_name ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "buffer_get_name"; params = [ buffer ]; witness = String }
;;

let buffer_set_name ~buffer ~name =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  { Api_result.name = "buffer_set_name"; params = [ buffer; name ]; witness = Nil }
;;

let buffer_is_valid ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "buffer_is_valid"; params = [ buffer ]; witness = Boolean }
;;

let buffer_get_mark ~buffer ~name =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  { Api_result.name = "buffer_get_mark"
  ; params = [ buffer; name ]
  ; witness = Array Object
  }
;;

let buffer_add_highlight ~buffer ~ns_id ~hl_group ~line ~col_start ~col_end =
  let buffer = Buffer.to_msgpack buffer in
  let ns_id = Msgpack.Integer ns_id in
  let hl_group = Msgpack.String hl_group in
  let line = Msgpack.Integer line in
  let col_start = Msgpack.Integer col_start in
  let col_end = Msgpack.Integer col_end in
  { Api_result.name = "buffer_add_highlight"
  ; params = [ buffer; ns_id; hl_group; line; col_start; col_end ]
  ; witness = Integer
  }
;;

let vim_command_output ~command =
  let command = Msgpack.String command in
  { Api_result.name = "vim_command_output"; params = [ command ]; witness = String }
;;

let buffer_get_number ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "buffer_get_number"; params = [ buffer ]; witness = Integer }
;;

let buffer_clear_highlight ~buffer ~ns_id ~line_start ~line_end =
  let buffer = Buffer.to_msgpack buffer in
  let ns_id = Msgpack.Integer ns_id in
  let line_start = Msgpack.Integer line_start in
  let line_end = Msgpack.Integer line_end in
  { Api_result.name = "buffer_clear_highlight"
  ; params = [ buffer; ns_id; line_start; line_end ]
  ; witness = Nil
  }
;;

let tabpage_get_windows ~tabpage =
  let tabpage = Tabpage.to_msgpack tabpage in
  { Api_result.name = "tabpage_get_windows"
  ; params = [ tabpage ]
  ; witness = Array Object
  }
;;

let tabpage_get_var ~tabpage ~name =
  let tabpage = Tabpage.to_msgpack tabpage in
  let name = Msgpack.String name in
  { Api_result.name = "tabpage_get_var"; params = [ tabpage; name ]; witness = Object }
;;

let tabpage_get_window ~tabpage =
  let tabpage = Tabpage.to_msgpack tabpage in
  { Api_result.name = "tabpage_get_window"; params = [ tabpage ]; witness = Window }
;;

let tabpage_is_valid ~tabpage =
  let tabpage = Tabpage.to_msgpack tabpage in
  { Api_result.name = "tabpage_is_valid"; params = [ tabpage ]; witness = Boolean }
;;

let ui_detach = { Api_result.name = "ui_detach"; params = []; witness = Nil }

let ui_try_resize ~width ~height =
  let width = Msgpack.Integer width in
  let height = Msgpack.Integer height in
  { Api_result.name = "ui_try_resize"; params = [ width; height ]; witness = Object }
;;

let vim_command ~command =
  let command = Msgpack.String command in
  { Api_result.name = "vim_command"; params = [ command ]; witness = Nil }
;;

let vim_feedkeys ~keys ~mode ~escape_csi =
  let keys = Msgpack.String keys in
  let mode = Msgpack.String mode in
  let escape_csi = Msgpack.Boolean escape_csi in
  { Api_result.name = "vim_feedkeys"; params = [ keys; mode; escape_csi ]; witness = Nil }
;;

let vim_input ~keys =
  let keys = Msgpack.String keys in
  { Api_result.name = "vim_input"; params = [ keys ]; witness = Integer }
;;

let vim_replace_termcodes ~str ~from_part ~do_lt ~special =
  let str = Msgpack.String str in
  let from_part = Msgpack.Boolean from_part in
  let do_lt = Msgpack.Boolean do_lt in
  let special = Msgpack.Boolean special in
  { Api_result.name = "vim_replace_termcodes"
  ; params = [ str; from_part; do_lt; special ]
  ; witness = String
  }
;;

let vim_eval ~expr =
  let expr = Msgpack.String expr in
  { Api_result.name = "vim_eval"; params = [ expr ]; witness = Object }
;;

let vim_call_function ~fn ~args =
  let fn = Msgpack.String fn in
  let args = Msgpack.Array (List.map ~f:(fun v -> v) args) in
  { Api_result.name = "vim_call_function"; params = [ fn; args ]; witness = Object }
;;

let vim_strwidth ~text =
  let text = Msgpack.String text in
  { Api_result.name = "vim_strwidth"; params = [ text ]; witness = Integer }
;;

let vim_list_runtime_paths =
  { Api_result.name = "vim_list_runtime_paths"; params = []; witness = Array Object }
;;

let vim_change_directory ~dir =
  let dir = Msgpack.String dir in
  { Api_result.name = "vim_change_directory"; params = [ dir ]; witness = Nil }
;;

let vim_get_current_line =
  { Api_result.name = "vim_get_current_line"; params = []; witness = String }
;;

let vim_set_current_line ~line =
  let line = Msgpack.String line in
  { Api_result.name = "vim_set_current_line"; params = [ line ]; witness = Nil }
;;

let vim_del_current_line =
  { Api_result.name = "vim_del_current_line"; params = []; witness = Nil }
;;

let vim_get_var ~name =
  let name = Msgpack.String name in
  { Api_result.name = "vim_get_var"; params = [ name ]; witness = Object }
;;

let vim_get_vvar ~name =
  let name = Msgpack.String name in
  { Api_result.name = "vim_get_vvar"; params = [ name ]; witness = Object }
;;

let vim_get_option ~name =
  let name = Msgpack.String name in
  { Api_result.name = "vim_get_option"; params = [ name ]; witness = Object }
;;

let vim_set_option ~name ~value =
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "vim_set_option"; params = [ name; value ]; witness = Nil }
;;

let vim_out_write ~str =
  let str = Msgpack.String str in
  { Api_result.name = "vim_out_write"; params = [ str ]; witness = Nil }
;;

let vim_err_write ~str =
  let str = Msgpack.String str in
  { Api_result.name = "vim_err_write"; params = [ str ]; witness = Nil }
;;

let vim_report_error ~str =
  let str = Msgpack.String str in
  { Api_result.name = "vim_report_error"; params = [ str ]; witness = Nil }
;;

let vim_get_buffers =
  { Api_result.name = "vim_get_buffers"; params = []; witness = Array Object }
;;

let vim_get_current_buffer =
  { Api_result.name = "vim_get_current_buffer"; params = []; witness = Buffer }
;;

let vim_set_current_buffer ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "vim_set_current_buffer"; params = [ buffer ]; witness = Nil }
;;

let vim_get_windows =
  { Api_result.name = "vim_get_windows"; params = []; witness = Array Object }
;;

let vim_get_current_window =
  { Api_result.name = "vim_get_current_window"; params = []; witness = Window }
;;

let vim_set_current_window ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "vim_set_current_window"; params = [ window ]; witness = Nil }
;;

let vim_get_tabpages =
  { Api_result.name = "vim_get_tabpages"; params = []; witness = Array Object }
;;

let vim_get_current_tabpage =
  { Api_result.name = "vim_get_current_tabpage"; params = []; witness = Tabpage }
;;

let vim_set_current_tabpage ~tabpage =
  let tabpage = Tabpage.to_msgpack tabpage in
  { Api_result.name = "vim_set_current_tabpage"; params = [ tabpage ]; witness = Nil }
;;

let vim_subscribe ~event =
  let event = Msgpack.String event in
  { Api_result.name = "vim_subscribe"; params = [ event ]; witness = Nil }
;;

let vim_unsubscribe ~event =
  let event = Msgpack.String event in
  { Api_result.name = "vim_unsubscribe"; params = [ event ]; witness = Nil }
;;

let vim_name_to_color ~name =
  let name = Msgpack.String name in
  { Api_result.name = "vim_name_to_color"; params = [ name ]; witness = Integer }
;;

let vim_get_color_map =
  { Api_result.name = "vim_get_color_map"; params = []; witness = Dict }
;;

let vim_get_api_info =
  { Api_result.name = "vim_get_api_info"; params = []; witness = Array Object }
;;

let window_get_buffer ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "window_get_buffer"; params = [ window ]; witness = Buffer }
;;

let window_get_cursor ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "window_get_cursor"; params = [ window ]; witness = Array Object }
;;

let window_set_cursor ~window ~pos =
  let window = Window.to_msgpack window in
  let pos = Msgpack.Array (List.map ~f:(fun v -> v) pos) in
  { Api_result.name = "window_set_cursor"; params = [ window; pos ]; witness = Nil }
;;

let window_get_height ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "window_get_height"; params = [ window ]; witness = Integer }
;;

let window_set_height ~window ~height =
  let window = Window.to_msgpack window in
  let height = Msgpack.Integer height in
  { Api_result.name = "window_set_height"; params = [ window; height ]; witness = Nil }
;;

let window_get_width ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "window_get_width"; params = [ window ]; witness = Integer }
;;

let window_set_width ~window ~width =
  let window = Window.to_msgpack window in
  let width = Msgpack.Integer width in
  { Api_result.name = "window_set_width"; params = [ window; width ]; witness = Nil }
;;

let window_get_var ~window ~name =
  let window = Window.to_msgpack window in
  let name = Msgpack.String name in
  { Api_result.name = "window_get_var"; params = [ window; name ]; witness = Object }
;;

let window_get_option ~window ~name =
  let window = Window.to_msgpack window in
  let name = Msgpack.String name in
  { Api_result.name = "window_get_option"; params = [ window; name ]; witness = Object }
;;

let window_set_option ~window ~name ~value =
  let window = Window.to_msgpack window in
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "window_set_option"
  ; params = [ window; name; value ]
  ; witness = Nil
  }
;;

let window_get_position ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "window_get_position"; params = [ window ]; witness = Array Object }
;;

let window_get_tabpage ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "window_get_tabpage"; params = [ window ]; witness = Tabpage }
;;

let window_is_valid ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "window_is_valid"; params = [ window ]; witness = Boolean }
;;
