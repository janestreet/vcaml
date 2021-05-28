open! Core_kernel
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
  { Version.api_compatible = 0; api_level = 5; major = 0; minor = 3; patch = 8 }
;;

module Ui_options = struct
  type t =
    { ext_cmdline : bool
    ; ext_hlstate : bool
    ; ext_linegrid : bool
    ; ext_popupmenu : bool
    ; ext_tabline : bool
    ; ext_wildmenu : bool
    ; rgb : bool
    }
  [@@deriving fields, sexp_of]

  let empty =
    { ext_cmdline = false
    ; ext_hlstate = false
    ; ext_linegrid = false
    ; ext_popupmenu = false
    ; ext_tabline = false
    ; ext_wildmenu = false
    ; rgb = false
    }
  ;;
end

module Buffer = (val make_nvim_id ~name:"buffer" ~type_id:0)
module Window = (val make_nvim_id ~name:"window" ~type_id:1)
module Tabpage = (val make_nvim_id ~name:"tabpage" ~type_id:2)

module Phantom = struct
  type _ t =
    | Nil : unit t
    | Integer : int t
    | Boolean : bool t
    | Array : 'a t -> 'a list t
    | Tuple : 'a t * int -> 'a list t
    | Dict : (Msgpack.t * Msgpack.t) list t
    | String : string t
    | Buffer : Buffer.t t
    | Window : Window.t t
    | Tabpage : Tabpage.t t
    | Object : Msgpack.t t
    | Custom : (module Msgpack.Msgpackable with type t = 'a) -> 'a t

  let rec sexp_of_t : type a. (a -> Sexp.t) -> a t -> Sexp.t =
    fun _ t ->
    let ignore _ : Sexp.t = List [] in
    match t with
    | Nil -> Sexp.Atom "Nil"
    | Integer -> Atom "Integer"
    | Boolean -> Atom "Boolean"
    | Array arr -> List [ sexp_of_t ignore arr; Atom "ArrayN" ]
    | Tuple (arr, n) -> List [ sexp_of_t ignore arr; Atom (sprintf "Array%d" n) ]
    | Dict -> Atom "Dict"
    | String -> Atom "String"
    | Buffer -> Atom "Buffer"
    | Window -> Atom "Window"
    | Tabpage -> Atom "Tabpage"
    | Object -> Atom "Object"
    | Custom _ -> Atom "Custom"
  ;;
end

module Api_result = struct
  type 'result t =
    { name : string
    ; params : Msgpack.t
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
    | Popupmenu_show of
        { items : Msgpack.t list
        ; selected : int
        ; row : int
        ; col : int
        }
    | Popupmenu_hide
    | Popupmenu_select of { selected : int }
    | Tabline_update of
        { current : Tabpage.t
        ; tabs : Msgpack.t list
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
           | ( "popupmenu_show"
             , [ Array items; Integer selected; Integer row; Integer col ] ) ->
             Ok (Popupmenu_show { items; selected; row; col })
           | "popupmenu_hide", [] -> Ok Popupmenu_hide
           | "popupmenu_select", [ Integer selected ] ->
             Ok (Popupmenu_select { selected })
           | "tabline_update", [ current; Array tabs ] ->
             let open Or_error.Let_syntax in
             let%bind current = Tabpage.of_msgpack current in
             return (Tabline_update { current; tabs })
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
           | _ ->
             Or_error.error_s [%message "Failed to parse UI event" (msg : Msgpack.t)])
        | _ -> Or_error.error_s [%message "Failed to parse UI event" (msg : Msgpack.t)])
      |> Or_error.combine_errors
    | _ -> Or_error.error_s [%message "Failed to parse UI event" (msg : Msgpack.t)]
  ;;
end

let nvim_buf_line_count ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "nvim_buf_line_count"
  ; params = Array [ buffer ]
  ; witness = Integer
  }
;;

let buffer_get_line ~buffer ~index =
  let buffer = Buffer.to_msgpack buffer in
  let index = Msgpack.Integer index in
  { Api_result.name = "buffer_get_line"
  ; params = Array [ buffer; index ]
  ; witness = String
  }
;;

let nvim_buf_attach ~buffer ~send_buffer ~opts =
  let buffer = Buffer.to_msgpack buffer in
  let send_buffer = Msgpack.Boolean send_buffer in
  let opts = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) opts) in
  { Api_result.name = "nvim_buf_attach"
  ; params = Array [ buffer; send_buffer; opts ]
  ; witness = Boolean
  }
;;

let nvim_buf_detach ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "nvim_buf_detach"; params = Array [ buffer ]; witness = Boolean }
;;

let buffer_set_line ~buffer ~index ~line =
  let buffer = Buffer.to_msgpack buffer in
  let index = Msgpack.Integer index in
  let line = Msgpack.String line in
  { Api_result.name = "buffer_set_line"
  ; params = Array [ buffer; index; line ]
  ; witness = Nil
  }
;;

let buffer_del_line ~buffer ~index =
  let buffer = Buffer.to_msgpack buffer in
  let index = Msgpack.Integer index in
  { Api_result.name = "buffer_del_line"; params = Array [ buffer; index ]; witness = Nil }
;;

let buffer_get_line_slice ~buffer ~start ~end_ ~include_start ~include_end =
  let buffer = Buffer.to_msgpack buffer in
  let start = Msgpack.Integer start in
  let end_ = Msgpack.Integer end_ in
  let include_start = Msgpack.Boolean include_start in
  let include_end = Msgpack.Boolean include_end in
  { Api_result.name = "buffer_get_line_slice"
  ; params = Array [ buffer; start; end_; include_start; include_end ]
  ; witness = Array Object
  }
;;

let nvim_buf_get_lines ~buffer ~start ~end_ ~strict_indexing =
  let buffer = Buffer.to_msgpack buffer in
  let start = Msgpack.Integer start in
  let end_ = Msgpack.Integer end_ in
  let strict_indexing = Msgpack.Boolean strict_indexing in
  { Api_result.name = "nvim_buf_get_lines"
  ; params = Array [ buffer; start; end_; strict_indexing ]
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
  ; params = Array [ buffer; start; end_; include_start; include_end; replacement ]
  ; witness = Nil
  }
;;

let nvim_buf_set_lines ~buffer ~start ~end_ ~strict_indexing ~replacement =
  let buffer = Buffer.to_msgpack buffer in
  let start = Msgpack.Integer start in
  let end_ = Msgpack.Integer end_ in
  let strict_indexing = Msgpack.Boolean strict_indexing in
  let replacement = Msgpack.Array (List.map ~f:(fun v -> v) replacement) in
  { Api_result.name = "nvim_buf_set_lines"
  ; params = Array [ buffer; start; end_; strict_indexing; replacement ]
  ; witness = Nil
  }
;;

let nvim_buf_get_offset ~buffer ~index =
  let buffer = Buffer.to_msgpack buffer in
  let index = Msgpack.Integer index in
  { Api_result.name = "nvim_buf_get_offset"
  ; params = Array [ buffer; index ]
  ; witness = Integer
  }
;;

let nvim_buf_get_var ~buffer ~name =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  { Api_result.name = "nvim_buf_get_var"
  ; params = Array [ buffer; name ]
  ; witness = Object
  }
;;

let nvim_buf_get_changedtick ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "nvim_buf_get_changedtick"
  ; params = Array [ buffer ]
  ; witness = Integer
  }
;;

let nvim_buf_get_keymap ~buffer ~mode =
  let buffer = Buffer.to_msgpack buffer in
  let mode = Msgpack.String mode in
  { Api_result.name = "nvim_buf_get_keymap"
  ; params = Array [ buffer; mode ]
  ; witness = Array Object
  }
;;

let nvim_buf_get_commands ~buffer ~opts =
  let buffer = Buffer.to_msgpack buffer in
  let opts = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) opts) in
  { Api_result.name = "nvim_buf_get_commands"
  ; params = Array [ buffer; opts ]
  ; witness = Dict
  }
;;

let nvim_buf_set_var ~buffer ~name ~value =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "nvim_buf_set_var"
  ; params = Array [ buffer; name; value ]
  ; witness = Nil
  }
;;

let nvim_buf_del_var ~buffer ~name =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  { Api_result.name = "nvim_buf_del_var"; params = Array [ buffer; name ]; witness = Nil }
;;

let buffer_set_var ~buffer ~name ~value =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "buffer_set_var"
  ; params = Array [ buffer; name; value ]
  ; witness = Object
  }
;;

let buffer_del_var ~buffer ~name =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  { Api_result.name = "buffer_del_var"
  ; params = Array [ buffer; name ]
  ; witness = Object
  }
;;

let nvim_buf_get_option ~buffer ~name =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  { Api_result.name = "nvim_buf_get_option"
  ; params = Array [ buffer; name ]
  ; witness = Object
  }
;;

let nvim_buf_set_option ~buffer ~name ~value =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "nvim_buf_set_option"
  ; params = Array [ buffer; name; value ]
  ; witness = Nil
  }
;;

let nvim_buf_get_number ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "nvim_buf_get_number"
  ; params = Array [ buffer ]
  ; witness = Integer
  }
;;

let nvim_buf_get_name ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "nvim_buf_get_name"; params = Array [ buffer ]; witness = String }
;;

let nvim_buf_set_name ~buffer ~name =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  { Api_result.name = "nvim_buf_set_name"
  ; params = Array [ buffer; name ]
  ; witness = Nil
  }
;;

let nvim_buf_is_loaded ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "nvim_buf_is_loaded"; params = Array [ buffer ]; witness = Boolean }
;;

let nvim_buf_is_valid ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "nvim_buf_is_valid"; params = Array [ buffer ]; witness = Boolean }
;;

let buffer_insert ~buffer ~lnum ~lines =
  let buffer = Buffer.to_msgpack buffer in
  let lnum = Msgpack.Integer lnum in
  let lines = Msgpack.Array (List.map ~f:(fun v -> v) lines) in
  { Api_result.name = "buffer_insert"
  ; params = Array [ buffer; lnum; lines ]
  ; witness = Nil
  }
;;

let nvim_buf_get_mark ~buffer ~name =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  { Api_result.name = "nvim_buf_get_mark"
  ; params = Array [ buffer; name ]
  ; witness = Array Object
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
  ; params = Array [ buffer; ns_id; hl_group; line; col_start; col_end ]
  ; witness = Integer
  }
;;

let nvim_buf_clear_namespace ~buffer ~ns_id ~line_start ~line_end =
  let buffer = Buffer.to_msgpack buffer in
  let ns_id = Msgpack.Integer ns_id in
  let line_start = Msgpack.Integer line_start in
  let line_end = Msgpack.Integer line_end in
  { Api_result.name = "nvim_buf_clear_namespace"
  ; params = Array [ buffer; ns_id; line_start; line_end ]
  ; witness = Nil
  }
;;

let nvim_buf_clear_highlight ~buffer ~ns_id ~line_start ~line_end =
  let buffer = Buffer.to_msgpack buffer in
  let ns_id = Msgpack.Integer ns_id in
  let line_start = Msgpack.Integer line_start in
  let line_end = Msgpack.Integer line_end in
  { Api_result.name = "nvim_buf_clear_highlight"
  ; params = Array [ buffer; ns_id; line_start; line_end ]
  ; witness = Nil
  }
;;

let nvim_buf_set_virtual_text ~buffer ~ns_id ~line ~chunks ~opts =
  let buffer = Buffer.to_msgpack buffer in
  let ns_id = Msgpack.Integer ns_id in
  let line = Msgpack.Integer line in
  let chunks = Msgpack.Array (List.map ~f:(fun v -> v) chunks) in
  let opts = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) opts) in
  { Api_result.name = "nvim_buf_set_virtual_text"
  ; params = Array [ buffer; ns_id; line; chunks; opts ]
  ; witness = Integer
  }
;;

let nvim_tabpage_list_wins ~tabpage =
  let tabpage = Tabpage.to_msgpack tabpage in
  { Api_result.name = "nvim_tabpage_list_wins"
  ; params = Array [ tabpage ]
  ; witness = Array Object
  }
;;

let nvim_tabpage_get_var ~tabpage ~name =
  let tabpage = Tabpage.to_msgpack tabpage in
  let name = Msgpack.String name in
  { Api_result.name = "nvim_tabpage_get_var"
  ; params = Array [ tabpage; name ]
  ; witness = Object
  }
;;

let nvim_tabpage_set_var ~tabpage ~name ~value =
  let tabpage = Tabpage.to_msgpack tabpage in
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "nvim_tabpage_set_var"
  ; params = Array [ tabpage; name; value ]
  ; witness = Nil
  }
;;

let nvim_tabpage_del_var ~tabpage ~name =
  let tabpage = Tabpage.to_msgpack tabpage in
  let name = Msgpack.String name in
  { Api_result.name = "nvim_tabpage_del_var"
  ; params = Array [ tabpage; name ]
  ; witness = Nil
  }
;;

let tabpage_set_var ~tabpage ~name ~value =
  let tabpage = Tabpage.to_msgpack tabpage in
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "tabpage_set_var"
  ; params = Array [ tabpage; name; value ]
  ; witness = Object
  }
;;

let tabpage_del_var ~tabpage ~name =
  let tabpage = Tabpage.to_msgpack tabpage in
  let name = Msgpack.String name in
  { Api_result.name = "tabpage_del_var"
  ; params = Array [ tabpage; name ]
  ; witness = Object
  }
;;

let nvim_tabpage_get_win ~tabpage =
  let tabpage = Tabpage.to_msgpack tabpage in
  { Api_result.name = "nvim_tabpage_get_win"
  ; params = Array [ tabpage ]
  ; witness = Window
  }
;;

let nvim_tabpage_get_number ~tabpage =
  let tabpage = Tabpage.to_msgpack tabpage in
  { Api_result.name = "nvim_tabpage_get_number"
  ; params = Array [ tabpage ]
  ; witness = Integer
  }
;;

let nvim_tabpage_is_valid ~tabpage =
  let tabpage = Tabpage.to_msgpack tabpage in
  { Api_result.name = "nvim_tabpage_is_valid"
  ; params = Array [ tabpage ]
  ; witness = Boolean
  }
;;

let nvim_ui_attach ~width ~height ~options =
  let width = Msgpack.Integer width in
  let height = Msgpack.Integer height in
  let options = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) options) in
  { Api_result.name = "nvim_ui_attach"
  ; params = Array [ width; height; options ]
  ; witness = Nil
  }
;;

let ui_attach ~width ~height ~enable_rgb =
  let width = Msgpack.Integer width in
  let height = Msgpack.Integer height in
  let enable_rgb = Msgpack.Boolean enable_rgb in
  { Api_result.name = "ui_attach"
  ; params = Array [ width; height; enable_rgb ]
  ; witness = Nil
  }
;;

let nvim_ui_detach =
  { Api_result.name = "nvim_ui_detach"; params = Array []; witness = Nil }
;;

let nvim_ui_try_resize ~width ~height =
  let width = Msgpack.Integer width in
  let height = Msgpack.Integer height in
  { Api_result.name = "nvim_ui_try_resize"
  ; params = Array [ width; height ]
  ; witness = Nil
  }
;;

let nvim_ui_set_option ~name ~value =
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "nvim_ui_set_option"
  ; params = Array [ name; value ]
  ; witness = Nil
  }
;;

let nvim_command ~command =
  let command = Msgpack.String command in
  { Api_result.name = "nvim_command"; params = Array [ command ]; witness = Nil }
;;

let nvim_get_hl_by_name ~name ~rgb =
  let name = Msgpack.String name in
  let rgb = Msgpack.Boolean rgb in
  { Api_result.name = "nvim_get_hl_by_name"
  ; params = Array [ name; rgb ]
  ; witness = Dict
  }
;;

let nvim_get_hl_by_id ~hl_id ~rgb =
  let hl_id = Msgpack.Integer hl_id in
  let rgb = Msgpack.Boolean rgb in
  { Api_result.name = "nvim_get_hl_by_id"; params = Array [ hl_id; rgb ]; witness = Dict }
;;

let nvim_feedkeys ~keys ~mode ~escape_csi =
  let keys = Msgpack.String keys in
  let mode = Msgpack.String mode in
  let escape_csi = Msgpack.Boolean escape_csi in
  { Api_result.name = "nvim_feedkeys"
  ; params = Array [ keys; mode; escape_csi ]
  ; witness = Nil
  }
;;

let nvim_input ~keys =
  let keys = Msgpack.String keys in
  { Api_result.name = "nvim_input"; params = Array [ keys ]; witness = Integer }
;;

let nvim_replace_termcodes ~str ~from_part ~do_lt ~special =
  let str = Msgpack.String str in
  let from_part = Msgpack.Boolean from_part in
  let do_lt = Msgpack.Boolean do_lt in
  let special = Msgpack.Boolean special in
  { Api_result.name = "nvim_replace_termcodes"
  ; params = Array [ str; from_part; do_lt; special ]
  ; witness = String
  }
;;

let nvim_command_output ~command =
  let command = Msgpack.String command in
  { Api_result.name = "nvim_command_output"
  ; params = Array [ command ]
  ; witness = String
  }
;;

let nvim_eval ~expr =
  let expr = Msgpack.String expr in
  { Api_result.name = "nvim_eval"; params = Array [ expr ]; witness = Object }
;;

let nvim_execute_lua ~code ~args =
  let code = Msgpack.String code in
  let args = Msgpack.Array (List.map ~f:(fun v -> v) args) in
  { Api_result.name = "nvim_execute_lua"
  ; params = Array [ code; args ]
  ; witness = Object
  }
;;

let nvim_call_function ~fn ~args =
  let fn = Msgpack.String fn in
  let args = Msgpack.Array (List.map ~f:(fun v -> v) args) in
  { Api_result.name = "nvim_call_function"
  ; params = Array [ fn; args ]
  ; witness = Object
  }
;;

let nvim_call_dict_function ~dict ~fn ~args =
  let dict = dict in
  let fn = Msgpack.String fn in
  let args = Msgpack.Array (List.map ~f:(fun v -> v) args) in
  { Api_result.name = "nvim_call_dict_function"
  ; params = Array [ dict; fn; args ]
  ; witness = Object
  }
;;

let nvim_strwidth ~text =
  let text = Msgpack.String text in
  { Api_result.name = "nvim_strwidth"; params = Array [ text ]; witness = Integer }
;;

let nvim_list_runtime_paths =
  { Api_result.name = "nvim_list_runtime_paths"
  ; params = Array []
  ; witness = Array Object
  }
;;

let nvim_set_current_dir ~dir =
  let dir = Msgpack.String dir in
  { Api_result.name = "nvim_set_current_dir"; params = Array [ dir ]; witness = Nil }
;;

let nvim_get_current_line =
  { Api_result.name = "nvim_get_current_line"; params = Array []; witness = String }
;;

let nvim_set_current_line ~line =
  let line = Msgpack.String line in
  { Api_result.name = "nvim_set_current_line"; params = Array [ line ]; witness = Nil }
;;

let nvim_del_current_line =
  { Api_result.name = "nvim_del_current_line"; params = Array []; witness = Nil }
;;

let nvim_get_var ~name =
  let name = Msgpack.String name in
  { Api_result.name = "nvim_get_var"; params = Array [ name ]; witness = Object }
;;

let nvim_set_var ~name ~value =
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "nvim_set_var"; params = Array [ name; value ]; witness = Nil }
;;

let nvim_del_var ~name =
  let name = Msgpack.String name in
  { Api_result.name = "nvim_del_var"; params = Array [ name ]; witness = Nil }
;;

let vim_set_var ~name ~value =
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "vim_set_var"; params = Array [ name; value ]; witness = Object }
;;

let vim_del_var ~name =
  let name = Msgpack.String name in
  { Api_result.name = "vim_del_var"; params = Array [ name ]; witness = Object }
;;

let nvim_get_vvar ~name =
  let name = Msgpack.String name in
  { Api_result.name = "nvim_get_vvar"; params = Array [ name ]; witness = Object }
;;

let nvim_get_option ~name =
  let name = Msgpack.String name in
  { Api_result.name = "nvim_get_option"; params = Array [ name ]; witness = Object }
;;

let nvim_set_option ~name ~value =
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "nvim_set_option"; params = Array [ name; value ]; witness = Nil }
;;

let nvim_out_write ~str =
  let str = Msgpack.String str in
  { Api_result.name = "nvim_out_write"; params = Array [ str ]; witness = Nil }
;;

let nvim_err_write ~str =
  let str = Msgpack.String str in
  { Api_result.name = "nvim_err_write"; params = Array [ str ]; witness = Nil }
;;

let nvim_err_writeln ~str =
  let str = Msgpack.String str in
  { Api_result.name = "nvim_err_writeln"; params = Array [ str ]; witness = Nil }
;;

let nvim_list_bufs =
  { Api_result.name = "nvim_list_bufs"; params = Array []; witness = Array Object }
;;

let nvim_get_current_buf =
  { Api_result.name = "nvim_get_current_buf"; params = Array []; witness = Buffer }
;;

let nvim_set_current_buf ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "nvim_set_current_buf"; params = Array [ buffer ]; witness = Nil }
;;

let nvim_list_wins =
  { Api_result.name = "nvim_list_wins"; params = Array []; witness = Array Object }
;;

let nvim_get_current_win =
  { Api_result.name = "nvim_get_current_win"; params = Array []; witness = Window }
;;

let nvim_set_current_win ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "nvim_set_current_win"; params = Array [ window ]; witness = Nil }
;;

let nvim_list_tabpages =
  { Api_result.name = "nvim_list_tabpages"; params = Array []; witness = Array Object }
;;

let nvim_get_current_tabpage =
  { Api_result.name = "nvim_get_current_tabpage"; params = Array []; witness = Tabpage }
;;

let nvim_set_current_tabpage ~tabpage =
  let tabpage = Tabpage.to_msgpack tabpage in
  { Api_result.name = "nvim_set_current_tabpage"
  ; params = Array [ tabpage ]
  ; witness = Nil
  }
;;

let nvim_create_namespace ~name =
  let name = Msgpack.String name in
  { Api_result.name = "nvim_create_namespace"
  ; params = Array [ name ]
  ; witness = Integer
  }
;;

let nvim_get_namespaces =
  { Api_result.name = "nvim_get_namespaces"; params = Array []; witness = Dict }
;;

let nvim_subscribe ~event =
  let event = Msgpack.String event in
  { Api_result.name = "nvim_subscribe"; params = Array [ event ]; witness = Nil }
;;

let nvim_unsubscribe ~event =
  let event = Msgpack.String event in
  { Api_result.name = "nvim_unsubscribe"; params = Array [ event ]; witness = Nil }
;;

let nvim_get_color_by_name ~name =
  let name = Msgpack.String name in
  { Api_result.name = "nvim_get_color_by_name"
  ; params = Array [ name ]
  ; witness = Integer
  }
;;

let nvim_get_color_map =
  { Api_result.name = "nvim_get_color_map"; params = Array []; witness = Dict }
;;

let nvim_get_mode =
  { Api_result.name = "nvim_get_mode"; params = Array []; witness = Dict }
;;

let nvim_get_keymap ~mode =
  let mode = Msgpack.String mode in
  { Api_result.name = "nvim_get_keymap"; params = Array [ mode ]; witness = Array Object }
;;

let nvim_get_commands ~opts =
  let opts = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) opts) in
  { Api_result.name = "nvim_get_commands"; params = Array [ opts ]; witness = Dict }
;;

let nvim_get_api_info =
  { Api_result.name = "nvim_get_api_info"; params = Array []; witness = Array Object }
;;

let nvim_set_client_info ~name ~version ~type_ ~methods ~attributes =
  let name = Msgpack.String name in
  let version = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) version) in
  let type_ = Msgpack.String type_ in
  let methods = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) methods) in
  let attributes = Msgpack.Map (List.map ~f:(fun (s, v) -> s, v) attributes) in
  { Api_result.name = "nvim_set_client_info"
  ; params = Array [ name; version; type_; methods; attributes ]
  ; witness = Nil
  }
;;

let nvim_get_chan_info ~chan =
  let chan = Msgpack.Integer chan in
  { Api_result.name = "nvim_get_chan_info"; params = Array [ chan ]; witness = Dict }
;;

let nvim_list_chans =
  { Api_result.name = "nvim_list_chans"; params = Array []; witness = Array Object }
;;

let nvim_call_atomic ~calls =
  let calls = Msgpack.Array (List.map ~f:(fun v -> v) calls) in
  { Api_result.name = "nvim_call_atomic"
  ; params = Array [ calls ]
  ; witness = Array Object
  }
;;

let nvim_parse_expression ~expr ~flags ~highlight =
  let expr = Msgpack.String expr in
  let flags = Msgpack.String flags in
  let highlight = Msgpack.Boolean highlight in
  { Api_result.name = "nvim_parse_expression"
  ; params = Array [ expr; flags; highlight ]
  ; witness = Dict
  }
;;

let nvim_list_uis =
  { Api_result.name = "nvim_list_uis"; params = Array []; witness = Array Object }
;;

let nvim_get_proc_children ~pid =
  let pid = Msgpack.Integer pid in
  { Api_result.name = "nvim_get_proc_children"
  ; params = Array [ pid ]
  ; witness = Array Object
  }
;;

let nvim_get_proc ~pid =
  let pid = Msgpack.Integer pid in
  { Api_result.name = "nvim_get_proc"; params = Array [ pid ]; witness = Object }
;;

let nvim_win_get_buf ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "nvim_win_get_buf"; params = Array [ window ]; witness = Buffer }
;;

let nvim_win_set_buf ~window ~buffer =
  let window = Window.to_msgpack window in
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "nvim_win_set_buf"
  ; params = Array [ window; buffer ]
  ; witness = Nil
  }
;;

let nvim_win_get_cursor ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "nvim_win_get_cursor"
  ; params = Array [ window ]
  ; witness = Array Object
  }
;;

let nvim_win_set_cursor ~window ~pos =
  let window = Window.to_msgpack window in
  let pos = Msgpack.Array (List.map ~f:(fun v -> v) pos) in
  { Api_result.name = "nvim_win_set_cursor"
  ; params = Array [ window; pos ]
  ; witness = Nil
  }
;;

let nvim_win_get_height ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "nvim_win_get_height"
  ; params = Array [ window ]
  ; witness = Integer
  }
;;

let nvim_win_set_height ~window ~height =
  let window = Window.to_msgpack window in
  let height = Msgpack.Integer height in
  { Api_result.name = "nvim_win_set_height"
  ; params = Array [ window; height ]
  ; witness = Nil
  }
;;

let nvim_win_get_width ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "nvim_win_get_width"; params = Array [ window ]; witness = Integer }
;;

let nvim_win_set_width ~window ~width =
  let window = Window.to_msgpack window in
  let width = Msgpack.Integer width in
  { Api_result.name = "nvim_win_set_width"
  ; params = Array [ window; width ]
  ; witness = Nil
  }
;;

let nvim_win_get_var ~window ~name =
  let window = Window.to_msgpack window in
  let name = Msgpack.String name in
  { Api_result.name = "nvim_win_get_var"
  ; params = Array [ window; name ]
  ; witness = Object
  }
;;

let nvim_win_set_var ~window ~name ~value =
  let window = Window.to_msgpack window in
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "nvim_win_set_var"
  ; params = Array [ window; name; value ]
  ; witness = Nil
  }
;;

let nvim_win_del_var ~window ~name =
  let window = Window.to_msgpack window in
  let name = Msgpack.String name in
  { Api_result.name = "nvim_win_del_var"; params = Array [ window; name ]; witness = Nil }
;;

let window_set_var ~window ~name ~value =
  let window = Window.to_msgpack window in
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "window_set_var"
  ; params = Array [ window; name; value ]
  ; witness = Object
  }
;;

let window_del_var ~window ~name =
  let window = Window.to_msgpack window in
  let name = Msgpack.String name in
  { Api_result.name = "window_del_var"
  ; params = Array [ window; name ]
  ; witness = Object
  }
;;

let nvim_win_get_option ~window ~name =
  let window = Window.to_msgpack window in
  let name = Msgpack.String name in
  { Api_result.name = "nvim_win_get_option"
  ; params = Array [ window; name ]
  ; witness = Object
  }
;;

let nvim_win_set_option ~window ~name ~value =
  let window = Window.to_msgpack window in
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "nvim_win_set_option"
  ; params = Array [ window; name; value ]
  ; witness = Nil
  }
;;

let nvim_win_get_position ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "nvim_win_get_position"
  ; params = Array [ window ]
  ; witness = Array Object
  }
;;

let nvim_win_get_tabpage ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "nvim_win_get_tabpage"
  ; params = Array [ window ]
  ; witness = Tabpage
  }
;;

let nvim_win_get_number ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "nvim_win_get_number"
  ; params = Array [ window ]
  ; witness = Integer
  }
;;

let nvim_win_is_valid ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "nvim_win_is_valid"; params = Array [ window ]; witness = Boolean }
;;

let buffer_line_count ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "buffer_line_count"; params = Array [ buffer ]; witness = Integer }
;;

let buffer_get_lines ~buffer ~start ~end_ ~strict_indexing =
  let buffer = Buffer.to_msgpack buffer in
  let start = Msgpack.Integer start in
  let end_ = Msgpack.Integer end_ in
  let strict_indexing = Msgpack.Boolean strict_indexing in
  { Api_result.name = "buffer_get_lines"
  ; params = Array [ buffer; start; end_; strict_indexing ]
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
  ; params = Array [ buffer; start; end_; strict_indexing; replacement ]
  ; witness = Nil
  }
;;

let buffer_get_var ~buffer ~name =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  { Api_result.name = "buffer_get_var"
  ; params = Array [ buffer; name ]
  ; witness = Object
  }
;;

let buffer_get_option ~buffer ~name =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  { Api_result.name = "buffer_get_option"
  ; params = Array [ buffer; name ]
  ; witness = Object
  }
;;

let buffer_set_option ~buffer ~name ~value =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "buffer_set_option"
  ; params = Array [ buffer; name; value ]
  ; witness = Nil
  }
;;

let buffer_get_number ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "buffer_get_number"; params = Array [ buffer ]; witness = Integer }
;;

let buffer_get_name ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "buffer_get_name"; params = Array [ buffer ]; witness = String }
;;

let buffer_set_name ~buffer ~name =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  { Api_result.name = "buffer_set_name"; params = Array [ buffer; name ]; witness = Nil }
;;

let buffer_is_valid ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "buffer_is_valid"; params = Array [ buffer ]; witness = Boolean }
;;

let buffer_get_mark ~buffer ~name =
  let buffer = Buffer.to_msgpack buffer in
  let name = Msgpack.String name in
  { Api_result.name = "buffer_get_mark"
  ; params = Array [ buffer; name ]
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
  ; params = Array [ buffer; ns_id; hl_group; line; col_start; col_end ]
  ; witness = Integer
  }
;;

let buffer_clear_highlight ~buffer ~ns_id ~line_start ~line_end =
  let buffer = Buffer.to_msgpack buffer in
  let ns_id = Msgpack.Integer ns_id in
  let line_start = Msgpack.Integer line_start in
  let line_end = Msgpack.Integer line_end in
  { Api_result.name = "buffer_clear_highlight"
  ; params = Array [ buffer; ns_id; line_start; line_end ]
  ; witness = Nil
  }
;;

let tabpage_get_windows ~tabpage =
  let tabpage = Tabpage.to_msgpack tabpage in
  { Api_result.name = "tabpage_get_windows"
  ; params = Array [ tabpage ]
  ; witness = Array Object
  }
;;

let tabpage_get_var ~tabpage ~name =
  let tabpage = Tabpage.to_msgpack tabpage in
  let name = Msgpack.String name in
  { Api_result.name = "tabpage_get_var"
  ; params = Array [ tabpage; name ]
  ; witness = Object
  }
;;

let tabpage_get_window ~tabpage =
  let tabpage = Tabpage.to_msgpack tabpage in
  { Api_result.name = "tabpage_get_window"; params = Array [ tabpage ]; witness = Window }
;;

let tabpage_is_valid ~tabpage =
  let tabpage = Tabpage.to_msgpack tabpage in
  { Api_result.name = "tabpage_is_valid"; params = Array [ tabpage ]; witness = Boolean }
;;

let ui_detach = { Api_result.name = "ui_detach"; params = Array []; witness = Nil }

let ui_try_resize ~width ~height =
  let width = Msgpack.Integer width in
  let height = Msgpack.Integer height in
  { Api_result.name = "ui_try_resize"
  ; params = Array [ width; height ]
  ; witness = Object
  }
;;

let vim_command ~command =
  let command = Msgpack.String command in
  { Api_result.name = "vim_command"; params = Array [ command ]; witness = Nil }
;;

let vim_feedkeys ~keys ~mode ~escape_csi =
  let keys = Msgpack.String keys in
  let mode = Msgpack.String mode in
  let escape_csi = Msgpack.Boolean escape_csi in
  { Api_result.name = "vim_feedkeys"
  ; params = Array [ keys; mode; escape_csi ]
  ; witness = Nil
  }
;;

let vim_input ~keys =
  let keys = Msgpack.String keys in
  { Api_result.name = "vim_input"; params = Array [ keys ]; witness = Integer }
;;

let vim_replace_termcodes ~str ~from_part ~do_lt ~special =
  let str = Msgpack.String str in
  let from_part = Msgpack.Boolean from_part in
  let do_lt = Msgpack.Boolean do_lt in
  let special = Msgpack.Boolean special in
  { Api_result.name = "vim_replace_termcodes"
  ; params = Array [ str; from_part; do_lt; special ]
  ; witness = String
  }
;;

let vim_command_output ~command =
  let command = Msgpack.String command in
  { Api_result.name = "vim_command_output"; params = Array [ command ]; witness = String }
;;

let vim_eval ~expr =
  let expr = Msgpack.String expr in
  { Api_result.name = "vim_eval"; params = Array [ expr ]; witness = Object }
;;

let vim_call_function ~fn ~args =
  let fn = Msgpack.String fn in
  let args = Msgpack.Array (List.map ~f:(fun v -> v) args) in
  { Api_result.name = "vim_call_function"; params = Array [ fn; args ]; witness = Object }
;;

let vim_strwidth ~text =
  let text = Msgpack.String text in
  { Api_result.name = "vim_strwidth"; params = Array [ text ]; witness = Integer }
;;

let vim_list_runtime_paths =
  { Api_result.name = "vim_list_runtime_paths"
  ; params = Array []
  ; witness = Array Object
  }
;;

let vim_change_directory ~dir =
  let dir = Msgpack.String dir in
  { Api_result.name = "vim_change_directory"; params = Array [ dir ]; witness = Nil }
;;

let vim_get_current_line =
  { Api_result.name = "vim_get_current_line"; params = Array []; witness = String }
;;

let vim_set_current_line ~line =
  let line = Msgpack.String line in
  { Api_result.name = "vim_set_current_line"; params = Array [ line ]; witness = Nil }
;;

let vim_del_current_line =
  { Api_result.name = "vim_del_current_line"; params = Array []; witness = Nil }
;;

let vim_get_var ~name =
  let name = Msgpack.String name in
  { Api_result.name = "vim_get_var"; params = Array [ name ]; witness = Object }
;;

let vim_get_vvar ~name =
  let name = Msgpack.String name in
  { Api_result.name = "vim_get_vvar"; params = Array [ name ]; witness = Object }
;;

let vim_get_option ~name =
  let name = Msgpack.String name in
  { Api_result.name = "vim_get_option"; params = Array [ name ]; witness = Object }
;;

let vim_set_option ~name ~value =
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "vim_set_option"; params = Array [ name; value ]; witness = Nil }
;;

let vim_out_write ~str =
  let str = Msgpack.String str in
  { Api_result.name = "vim_out_write"; params = Array [ str ]; witness = Nil }
;;

let vim_err_write ~str =
  let str = Msgpack.String str in
  { Api_result.name = "vim_err_write"; params = Array [ str ]; witness = Nil }
;;

let vim_report_error ~str =
  let str = Msgpack.String str in
  { Api_result.name = "vim_report_error"; params = Array [ str ]; witness = Nil }
;;

let vim_get_buffers =
  { Api_result.name = "vim_get_buffers"; params = Array []; witness = Array Object }
;;

let vim_get_current_buffer =
  { Api_result.name = "vim_get_current_buffer"; params = Array []; witness = Buffer }
;;

let vim_set_current_buffer ~buffer =
  let buffer = Buffer.to_msgpack buffer in
  { Api_result.name = "vim_set_current_buffer"; params = Array [ buffer ]; witness = Nil }
;;

let vim_get_windows =
  { Api_result.name = "vim_get_windows"; params = Array []; witness = Array Object }
;;

let vim_get_current_window =
  { Api_result.name = "vim_get_current_window"; params = Array []; witness = Window }
;;

let vim_set_current_window ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "vim_set_current_window"; params = Array [ window ]; witness = Nil }
;;

let vim_get_tabpages =
  { Api_result.name = "vim_get_tabpages"; params = Array []; witness = Array Object }
;;

let vim_get_current_tabpage =
  { Api_result.name = "vim_get_current_tabpage"; params = Array []; witness = Tabpage }
;;

let vim_set_current_tabpage ~tabpage =
  let tabpage = Tabpage.to_msgpack tabpage in
  { Api_result.name = "vim_set_current_tabpage"
  ; params = Array [ tabpage ]
  ; witness = Nil
  }
;;

let vim_subscribe ~event =
  let event = Msgpack.String event in
  { Api_result.name = "vim_subscribe"; params = Array [ event ]; witness = Nil }
;;

let vim_unsubscribe ~event =
  let event = Msgpack.String event in
  { Api_result.name = "vim_unsubscribe"; params = Array [ event ]; witness = Nil }
;;

let vim_name_to_color ~name =
  let name = Msgpack.String name in
  { Api_result.name = "vim_name_to_color"; params = Array [ name ]; witness = Integer }
;;

let vim_get_color_map =
  { Api_result.name = "vim_get_color_map"; params = Array []; witness = Dict }
;;

let vim_get_api_info =
  { Api_result.name = "vim_get_api_info"; params = Array []; witness = Array Object }
;;

let window_get_buffer ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "window_get_buffer"; params = Array [ window ]; witness = Buffer }
;;

let window_get_cursor ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "window_get_cursor"
  ; params = Array [ window ]
  ; witness = Array Object
  }
;;

let window_set_cursor ~window ~pos =
  let window = Window.to_msgpack window in
  let pos = Msgpack.Array (List.map ~f:(fun v -> v) pos) in
  { Api_result.name = "window_set_cursor"; params = Array [ window; pos ]; witness = Nil }
;;

let window_get_height ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "window_get_height"; params = Array [ window ]; witness = Integer }
;;

let window_set_height ~window ~height =
  let window = Window.to_msgpack window in
  let height = Msgpack.Integer height in
  { Api_result.name = "window_set_height"
  ; params = Array [ window; height ]
  ; witness = Nil
  }
;;

let window_get_width ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "window_get_width"; params = Array [ window ]; witness = Integer }
;;

let window_set_width ~window ~width =
  let window = Window.to_msgpack window in
  let width = Msgpack.Integer width in
  { Api_result.name = "window_set_width"
  ; params = Array [ window; width ]
  ; witness = Nil
  }
;;

let window_get_var ~window ~name =
  let window = Window.to_msgpack window in
  let name = Msgpack.String name in
  { Api_result.name = "window_get_var"
  ; params = Array [ window; name ]
  ; witness = Object
  }
;;

let window_get_option ~window ~name =
  let window = Window.to_msgpack window in
  let name = Msgpack.String name in
  { Api_result.name = "window_get_option"
  ; params = Array [ window; name ]
  ; witness = Object
  }
;;

let window_set_option ~window ~name ~value =
  let window = Window.to_msgpack window in
  let name = Msgpack.String name in
  let value = value in
  { Api_result.name = "window_set_option"
  ; params = Array [ window; name; value ]
  ; witness = Nil
  }
;;

let window_get_position ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "window_get_position"
  ; params = Array [ window ]
  ; witness = Array Object
  }
;;

let window_get_tabpage ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "window_get_tabpage"; params = Array [ window ]; witness = Tabpage }
;;

let window_is_valid ~window =
  let window = Window.to_msgpack window in
  { Api_result.name = "window_is_valid"; params = Array [ window ]; witness = Boolean }
;;
