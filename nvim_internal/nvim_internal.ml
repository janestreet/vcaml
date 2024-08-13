open! Core
open Runtime

module Api_version = struct
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

let api_version =
  { Api_version.api_compatible = 0; api_level = 11; major = 0; minor = 9; patch = 1 }
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
  [@@deriving fields ~iterators:(make_creator, to_list), sexp_of]

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

module Buffer0 = (val make_nvim_id ~name:"buffer" ~type_id:0)
module Window0 = (val make_nvim_id ~name:"window" ~type_id:1)
module Tabpage0 = (val make_nvim_id ~name:"tabpage" ~type_id:2)

type buffer = Buffer0.t
type window = Window0.t
type tabpage = Tabpage0.t

module Phantom = struct
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

  let rec sexp_of_t : type a. (a -> Sexp.t) -> a t -> Sexp.t =
    fun _ t ->
    let ignore _ : Sexp.t = List [] in
    match t with
    | Nil -> Sexp.Atom "Nil"
    | Int -> Atom "Int"
    | Bool -> Atom "Bool"
    | Float -> Atom "Float"
    | Array arr -> List [ sexp_of_t ignore arr; Atom "Array" ]
    | Tuple2 (x, y) ->
      List [ Atom "("; sexp_of_t ignore x; Atom "*"; sexp_of_t ignore y; Atom ")" ]
    | Dict -> Atom "Dict"
    | String -> Atom "String"
    | Buffer -> Atom "Buffer"
    | Window -> Atom "Window"
    | Tabpage -> Atom "Tabpage"
    | Object -> Atom "Object"
    | Custom _ -> Atom "Custom"
  ;;

  let rec of_msgpack : type a. a t -> Msgpack.t -> a Or_error.t =
    fun witness msgpack ->
    match witness, msgpack with
    | Nil, Nil -> Ok ()
    | Nil, Array [] -> Ok ()
    | Nil, Map [] -> Ok ()
    | Int, Int i -> Ok i
    | Bool, Bool b -> Ok b
    | Bool, Int 0 -> Ok false
    | Bool, Int _ -> Ok true
    | Float, Float f -> Ok f
    | Tuple2 (t1, t2), Array [ v1; v2 ] ->
      let result1 =
        of_msgpack t1 v1 |> Or_error.tag_s ~tag:[%message "" ~index:(0 : int)]
      in
      let result2 =
        of_msgpack t2 v2 |> Or_error.tag_s ~tag:[%message "" ~index:(1 : int)]
      in
      (match Or_error.both result1 result2 with
       | Ok _ as ok -> ok
       | Error errors ->
         Or_error.error_s
           [%message
             "Errors while parsing Msgpack tuple" (msgpack : Msgpack.t) (errors : Error.t)])
    | Array t, Array vs ->
      let oks, errors =
        List.mapi vs ~f:(fun index v ->
          match of_msgpack t v with
          | Ok ok -> First ok
          | Error error -> Second (Error.tag_s error ~tag:[%message (index : int)]))
        |> List.partition_map ~f:Fn.id
      in
      (match errors with
       | [] -> Ok oks
       | _ :: _ ->
         Or_error.error_s
           [%message
             "Errors while parsing Msgpack array"
               (msgpack : Msgpack.t)
               (errors : Error.t list)])
    | Dict, Map kvs ->
      let%bind.Or_error dict =
        List.map kvs ~f:(function
          | String k, v -> Ok (k, v)
          | key, _ ->
            Or_error.error_s
              [%message
                "Dictionary key is not a string" (key : Msgpack.t) (msgpack : Msgpack.t)])
        |> Or_error.combine_errors
      in
      (match String.Map.of_alist dict with
       | `Ok map -> Ok map
       | `Duplicate_key key ->
         Or_error.error_s
           [%message "Duplicate key in dictionary" (key : string) (msgpack : Msgpack.t)])
    | String, String s -> Ok s
    | Buffer, _ -> Buffer0.of_msgpack msgpack
    | Window, _ -> Window0.of_msgpack msgpack
    | Tabpage, _ -> Tabpage0.of_msgpack msgpack
    | Object, _ -> Ok msgpack
    | Custom (module M), obj ->
      (match M.of_msgpack obj with
       | Ok _ as ok -> ok
       | Error error ->
         Or_error.error_s [%message "" ~_:(error : Error.t) (msgpack : Msgpack.t)])
    | _ ->
      Or_error.error_s
        [%message
          "witness does not match message type" (witness : _ t) (msgpack : Msgpack.t)]
  ;;

  let rec to_msgpack : type a. a t -> a -> Msgpack.t =
    fun witness value ->
    match witness with
    | Nil -> Nil
    | Int -> Int value
    | Bool -> Bool value
    | Float -> Float value
    | Dict ->
      value |> Map.to_alist |> List.map ~f:(fun (k, v) -> Msgpack.String k, v) |> Map
    | String -> String value
    | Array typ -> Array (List.map ~f:(to_msgpack typ) value)
    | Tuple2 (ty1, ty2) ->
      let x, y = value in
      Array [ to_msgpack ty1 x; to_msgpack ty2 y ]
    | Buffer -> Buffer0.to_msgpack value
    | Window -> Window0.to_msgpack value
    | Tabpage -> Tabpage0.to_msgpack value
    | Object -> value
    | Custom (module M) -> M.to_msgpack value
  ;;
end

module Buffer = struct
  include Buffer0

  let t = Phantom.Buffer

  module Or_current = struct
    include Or_current

    let t = Phantom.Custom (module Or_current)
  end
end

module Window = struct
  include Window0

  let t = Phantom.Window

  module Or_current = struct
    include Or_current

    let t = Phantom.Custom (module Or_current)
  end
end

module Tabpage = struct
  include Tabpage0

  let t = Phantom.Tabpage

  module Or_current = struct
    include Or_current

    let t = Phantom.Custom (module Or_current)
  end
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
    | Unknown of int
  [@@deriving equal, sexp]

  let of_int = function
    | 0 -> Exception
    | 1 -> Validation
    | id -> Unknown id
  ;;
end

let pp =
  Msgpack.pp ~pp_ext:(fun formatter ext ->
    let open Stdlib.Format in
    match Buffer.of_msgpack (Ext ext) with
    | Ok buffer -> pp_print_int formatter (buffer :> int)
    | Error _ ->
      (match Window.of_msgpack (Ext ext) with
       | Ok window -> pp_print_int formatter (window :> int)
       | Error _ ->
         (match Tabpage.of_msgpack (Ext ext) with
          | Ok tabpage -> pp_print_int formatter (tabpage :> int)
          | Error _ ->
            raise_s [%message "Unrecognized extension" (ext : Msgpack.Custom.t)])))
;;

module Ui_event = struct
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

  let of_msgpack msg =
    let open Or_error.Let_syntax in
    match (msg : Msgpack.t) with
    | Array (String name :: calls) ->
      (match
         List.map calls ~f:(function
           | Array params -> params
           | _ -> raise_notrace Exit)
       with
       | calls ->
         (match name with
          | "mode_info_set" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Bool enabled :: cursor_styles :: unparsed_fields ->
                 let%bind cursor_styles =
                   Phantom.of_msgpack (Array Object) cursor_styles
                 in
                 return (Mode_info_set { enabled; cursor_styles; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "update_menu" ->
            calls
            |> List.map ~f:(fun params -> Update_menu { unparsed_fields = params })
            |> Or_error.return
          | "busy_start" ->
            calls
            |> List.map ~f:(fun params -> Busy_start { unparsed_fields = params })
            |> Or_error.return
          | "busy_stop" ->
            calls
            |> List.map ~f:(fun params -> Busy_stop { unparsed_fields = params })
            |> Or_error.return
          | "mouse_on" ->
            calls
            |> List.map ~f:(fun params -> Mouse_on { unparsed_fields = params })
            |> Or_error.return
          | "mouse_off" ->
            calls
            |> List.map ~f:(fun params -> Mouse_off { unparsed_fields = params })
            |> Or_error.return
          | "mode_change" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | String mode :: Int mode_idx :: unparsed_fields ->
                 Ok (Mode_change { mode; mode_idx; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "bell" ->
            calls
            |> List.map ~f:(fun params -> Bell { unparsed_fields = params })
            |> Or_error.return
          | "visual_bell" ->
            calls
            |> List.map ~f:(fun params -> Visual_bell { unparsed_fields = params })
            |> Or_error.return
          | "flush" ->
            calls
            |> List.map ~f:(fun params -> Flush { unparsed_fields = params })
            |> Or_error.return
          | "suspend" ->
            calls
            |> List.map ~f:(fun params -> Suspend { unparsed_fields = params })
            |> Or_error.return
          | "set_title" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | String title :: unparsed_fields ->
                 Ok (Set_title { title; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "set_icon" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | String icon :: unparsed_fields -> Ok (Set_icon { icon; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "screenshot" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | String path :: unparsed_fields ->
                 Ok (Screenshot { path; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "option_set" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | String name :: value :: unparsed_fields ->
                 Ok (Option_set { name; value; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "update_fg" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int fg :: unparsed_fields -> Ok (Update_fg { fg; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "update_bg" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int bg :: unparsed_fields -> Ok (Update_bg { bg; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "update_sp" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int sp :: unparsed_fields -> Ok (Update_sp { sp; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "resize" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int width :: Int height :: unparsed_fields ->
                 Ok (Resize { width; height; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "clear" ->
            calls
            |> List.map ~f:(fun params -> Clear { unparsed_fields = params })
            |> Or_error.return
          | "eol_clear" ->
            calls
            |> List.map ~f:(fun params -> Eol_clear { unparsed_fields = params })
            |> Or_error.return
          | "cursor_goto" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int row :: Int col :: unparsed_fields ->
                 Ok (Cursor_goto { row; col; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "highlight_set" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | attrs :: unparsed_fields ->
                 let%bind attrs = Phantom.of_msgpack Dict attrs in
                 return (Highlight_set { attrs; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "put" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | String str :: unparsed_fields -> Ok (Put { str; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "set_scroll_region" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int top :: Int bot :: Int left :: Int right :: unparsed_fields ->
                 Ok (Set_scroll_region { top; bot; left; right; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "scroll" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int count :: unparsed_fields -> Ok (Scroll { count; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "default_colors_set" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int rgb_fg
                 :: Int rgb_bg
                 :: Int rgb_sp
                 :: Int cterm_fg
                 :: Int cterm_bg
                 :: unparsed_fields ->
                 Ok
                   (Default_colors_set
                      { rgb_fg; rgb_bg; rgb_sp; cterm_fg; cterm_bg; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "hl_attr_define" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int id :: rgb_attrs :: cterm_attrs :: info :: unparsed_fields ->
                 let%bind rgb_attrs = Phantom.of_msgpack Dict rgb_attrs in
                 let%bind cterm_attrs = Phantom.of_msgpack Dict cterm_attrs in
                 let%bind info = Phantom.of_msgpack (Array Object) info in
                 return
                   (Hl_attr_define { id; rgb_attrs; cterm_attrs; info; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "hl_group_set" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | String name :: Int id :: unparsed_fields ->
                 Ok (Hl_group_set { name; id; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "grid_resize" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int grid :: Int width :: Int height :: unparsed_fields ->
                 Ok (Grid_resize { grid; width; height; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "grid_clear" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int grid :: unparsed_fields -> Ok (Grid_clear { grid; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "grid_cursor_goto" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int grid :: Int row :: Int col :: unparsed_fields ->
                 Ok (Grid_cursor_goto { grid; row; col; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "grid_line" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int grid :: Int row :: Int col_start :: data :: unparsed_fields ->
                 let%bind data = Phantom.of_msgpack (Array Object) data in
                 return (Grid_line { grid; row; col_start; data; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "grid_scroll" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int grid
                 :: Int top
                 :: Int bot
                 :: Int left
                 :: Int right
                 :: Int rows
                 :: Int cols
                 :: unparsed_fields ->
                 Ok
                   (Grid_scroll
                      { grid; top; bot; left; right; rows; cols; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "grid_destroy" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int grid :: unparsed_fields ->
                 Ok (Grid_destroy { grid; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "win_pos" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int grid
                 :: win
                 :: Int startrow
                 :: Int startcol
                 :: Int width
                 :: Int height
                 :: unparsed_fields ->
                 let%bind win = Phantom.of_msgpack Window win in
                 return
                   (Win_pos
                      { grid; win; startrow; startcol; width; height; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "win_float_pos" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int grid
                 :: win
                 :: String anchor
                 :: Int anchor_grid
                 :: Float anchor_row
                 :: Float anchor_col
                 :: Bool focusable
                 :: Int zindex
                 :: unparsed_fields ->
                 let%bind win = Phantom.of_msgpack Window win in
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
                      ; unparsed_fields
                      })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "win_external_pos" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int grid :: win :: unparsed_fields ->
                 let%bind win = Phantom.of_msgpack Window win in
                 return (Win_external_pos { grid; win; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "win_hide" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int grid :: unparsed_fields -> Ok (Win_hide { grid; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "win_close" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int grid :: unparsed_fields -> Ok (Win_close { grid; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "msg_set_pos" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int grid
                 :: Int row
                 :: Bool scrolled
                 :: String sep_char
                 :: unparsed_fields ->
                 Ok (Msg_set_pos { grid; row; scrolled; sep_char; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "win_viewport" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int grid
                 :: win
                 :: Int topline
                 :: Int botline
                 :: Int curline
                 :: Int curcol
                 :: Int line_count
                 :: Int scroll_delta
                 :: unparsed_fields ->
                 let%bind win = Phantom.of_msgpack Window win in
                 return
                   (Win_viewport
                      { grid
                      ; win
                      ; topline
                      ; botline
                      ; curline
                      ; curcol
                      ; line_count
                      ; scroll_delta
                      ; unparsed_fields
                      })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "win_extmark" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int grid
                 :: win
                 :: Int ns_id
                 :: Int mark_id
                 :: Int row
                 :: Int col
                 :: unparsed_fields ->
                 let%bind win = Phantom.of_msgpack Window win in
                 return
                   (Win_extmark { grid; win; ns_id; mark_id; row; col; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "popupmenu_show" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | items
                 :: Int selected
                 :: Int row
                 :: Int col
                 :: Int grid
                 :: unparsed_fields ->
                 let%bind items = Phantom.of_msgpack (Array Object) items in
                 return
                   (Popupmenu_show { items; selected; row; col; grid; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "popupmenu_hide" ->
            calls
            |> List.map ~f:(fun params -> Popupmenu_hide { unparsed_fields = params })
            |> Or_error.return
          | "popupmenu_select" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int selected :: unparsed_fields ->
                 Ok (Popupmenu_select { selected; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "tabline_update" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | current :: tabs :: current_buffer :: buffers :: unparsed_fields ->
                 let%bind current = Phantom.of_msgpack Tabpage current in
                 let%bind tabs = Phantom.of_msgpack (Array Object) tabs in
                 let%bind current_buffer = Phantom.of_msgpack Buffer current_buffer in
                 let%bind buffers = Phantom.of_msgpack (Array Object) buffers in
                 return
                   (Tabline_update
                      { current; tabs; current_buffer; buffers; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "cmdline_show" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | content
                 :: Int pos
                 :: String firstc
                 :: String prompt
                 :: Int indent
                 :: Int level
                 :: unparsed_fields ->
                 let%bind content = Phantom.of_msgpack (Array Object) content in
                 return
                   (Cmdline_show
                      { content; pos; firstc; prompt; indent; level; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "cmdline_pos" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int pos :: Int level :: unparsed_fields ->
                 Ok (Cmdline_pos { pos; level; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "cmdline_special_char" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | String c :: Bool shift :: Int level :: unparsed_fields ->
                 Ok (Cmdline_special_char { c; shift; level; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "cmdline_hide" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int level :: unparsed_fields ->
                 Ok (Cmdline_hide { level; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "cmdline_block_show" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | lines :: unparsed_fields ->
                 let%bind lines = Phantom.of_msgpack (Array Object) lines in
                 return (Cmdline_block_show { lines; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "cmdline_block_append" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | lines :: unparsed_fields ->
                 let%bind lines = Phantom.of_msgpack (Array Object) lines in
                 return (Cmdline_block_append { lines; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "cmdline_block_hide" ->
            calls
            |> List.map ~f:(fun params -> Cmdline_block_hide { unparsed_fields = params })
            |> Or_error.return
          | "wildmenu_show" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | items :: unparsed_fields ->
                 let%bind items = Phantom.of_msgpack (Array Object) items in
                 return (Wildmenu_show { items; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "wildmenu_select" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | Int selected :: unparsed_fields ->
                 Ok (Wildmenu_select { selected; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "wildmenu_hide" ->
            calls
            |> List.map ~f:(fun params -> Wildmenu_hide { unparsed_fields = params })
            |> Or_error.return
          | "msg_show" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | String kind :: content :: Bool replace_last :: unparsed_fields ->
                 let%bind content = Phantom.of_msgpack (Array Object) content in
                 return (Msg_show { kind; content; replace_last; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "msg_clear" ->
            calls
            |> List.map ~f:(fun params -> Msg_clear { unparsed_fields = params })
            |> Or_error.return
          | "msg_showcmd" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | content :: unparsed_fields ->
                 let%bind content = Phantom.of_msgpack (Array Object) content in
                 return (Msg_showcmd { content; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "msg_showmode" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | content :: unparsed_fields ->
                 let%bind content = Phantom.of_msgpack (Array Object) content in
                 return (Msg_showmode { content; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "msg_ruler" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | content :: unparsed_fields ->
                 let%bind content = Phantom.of_msgpack (Array Object) content in
                 return (Msg_ruler { content; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "msg_history_show" ->
            calls
            |> List.map ~f:(fun params ->
              (match params with
               | entries :: unparsed_fields ->
                 let%bind entries = Phantom.of_msgpack (Array Object) entries in
                 return (Msg_history_show { entries; unparsed_fields })
               | _ -> Or_error.error_string "Arguments have wrong type or arity")
              |> Or_error.tag_s_lazy
                   ~tag:
                     [%lazy_message
                       "Failed to parse UI event"
                         (name : string)
                         (params : Msgpack.t list)])
            |> Or_error.combine_errors
          | "msg_history_clear" ->
            calls
            |> List.map ~f:(fun params -> Msg_history_clear { unparsed_fields = params })
            |> Or_error.return
          | _ ->
            calls
            |> List.map ~f:(fun params ->
              Unknown_event { name; unparsed_fields = params })
            |> Or_error.return)
       | exception Exit ->
         Or_error.error_s [%message "Failed to parse batched UI event" (msg : Msgpack.t)])
    | _ ->
      Or_error.error_s [%message "Failed to parse batched UI event" (msg : Msgpack.t)]
  ;;
end

let nvim_get_autocmds ~opts =
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_get_autocmds"; params = [ opts ]; witness = Array Object }
;;

let nvim_create_autocmd ~event ~opts =
  let event = Phantom.to_msgpack Object event in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_create_autocmd"; params = [ event; opts ]; witness = Int }
;;

let nvim_del_autocmd ~id =
  let id = Phantom.to_msgpack Int id in
  { Api_result.name = "nvim_del_autocmd"; params = [ id ]; witness = Nil }
;;

let nvim_clear_autocmds ~opts =
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_clear_autocmds"; params = [ opts ]; witness = Nil }
;;

let nvim_create_augroup ~name ~opts =
  let name = Phantom.to_msgpack String name in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_create_augroup"; params = [ name; opts ]; witness = Int }
;;

let nvim_del_augroup_by_id ~id =
  let id = Phantom.to_msgpack Int id in
  { Api_result.name = "nvim_del_augroup_by_id"; params = [ id ]; witness = Nil }
;;

let nvim_del_augroup_by_name ~name =
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_del_augroup_by_name"; params = [ name ]; witness = Nil }
;;

let nvim_exec_autocmds ~event ~opts =
  let event = Phantom.to_msgpack Object event in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_exec_autocmds"; params = [ event; opts ]; witness = Nil }
;;

let nvim_buf_line_count ~buffer =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  { Api_result.name = "nvim_buf_line_count"; params = [ buffer ]; witness = Int }
;;

let nvim_buf_attach ~buffer ~send_buffer ~opts =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let send_buffer = Phantom.to_msgpack Bool send_buffer in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_buf_attach"
  ; params = [ buffer; send_buffer; opts ]
  ; witness = Bool
  }
;;

let nvim_buf_detach ~buffer =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  { Api_result.name = "nvim_buf_detach"; params = [ buffer ]; witness = Bool }
;;

let nvim_buf_get_lines ~buffer ~start ~end_ ~strict_indexing =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let start = Phantom.to_msgpack Int start in
  let end_ = Phantom.to_msgpack Int end_ in
  let strict_indexing = Phantom.to_msgpack Bool strict_indexing in
  { Api_result.name = "nvim_buf_get_lines"
  ; params = [ buffer; start; end_; strict_indexing ]
  ; witness = Array String
  }
;;

let nvim_buf_set_lines ~buffer ~start ~end_ ~strict_indexing ~replacement =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let start = Phantom.to_msgpack Int start in
  let end_ = Phantom.to_msgpack Int end_ in
  let strict_indexing = Phantom.to_msgpack Bool strict_indexing in
  let replacement = Phantom.to_msgpack (Array String) replacement in
  { Api_result.name = "nvim_buf_set_lines"
  ; params = [ buffer; start; end_; strict_indexing; replacement ]
  ; witness = Nil
  }
;;

let nvim_buf_set_text ~buffer ~start_row ~start_col ~end_row ~end_col ~replacement =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let start_row = Phantom.to_msgpack Int start_row in
  let start_col = Phantom.to_msgpack Int start_col in
  let end_row = Phantom.to_msgpack Int end_row in
  let end_col = Phantom.to_msgpack Int end_col in
  let replacement = Phantom.to_msgpack (Array String) replacement in
  { Api_result.name = "nvim_buf_set_text"
  ; params = [ buffer; start_row; start_col; end_row; end_col; replacement ]
  ; witness = Nil
  }
;;

let nvim_buf_get_text ~buffer ~start_row ~start_col ~end_row ~end_col ~opts =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let start_row = Phantom.to_msgpack Int start_row in
  let start_col = Phantom.to_msgpack Int start_col in
  let end_row = Phantom.to_msgpack Int end_row in
  let end_col = Phantom.to_msgpack Int end_col in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_buf_get_text"
  ; params = [ buffer; start_row; start_col; end_row; end_col; opts ]
  ; witness = Array String
  }
;;

let nvim_buf_get_offset ~buffer ~index =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let index = Phantom.to_msgpack Int index in
  { Api_result.name = "nvim_buf_get_offset"; params = [ buffer; index ]; witness = Int }
;;

let nvim_buf_get_var ~buffer ~name =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_buf_get_var"; params = [ buffer; name ]; witness = Object }
;;

let nvim_buf_get_changedtick ~buffer =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  { Api_result.name = "nvim_buf_get_changedtick"; params = [ buffer ]; witness = Int }
;;

let nvim_buf_get_keymap ~buffer ~mode =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let mode = Phantom.to_msgpack String mode in
  { Api_result.name = "nvim_buf_get_keymap"
  ; params = [ buffer; mode ]
  ; witness = Array Dict
  }
;;

let nvim_buf_set_keymap ~buffer ~mode ~lhs ~rhs ~opts =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let mode = Phantom.to_msgpack String mode in
  let lhs = Phantom.to_msgpack String lhs in
  let rhs = Phantom.to_msgpack String rhs in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_buf_set_keymap"
  ; params = [ buffer; mode; lhs; rhs; opts ]
  ; witness = Nil
  }
;;

let nvim_buf_del_keymap ~buffer ~mode ~lhs =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let mode = Phantom.to_msgpack String mode in
  let lhs = Phantom.to_msgpack String lhs in
  { Api_result.name = "nvim_buf_del_keymap"
  ; params = [ buffer; mode; lhs ]
  ; witness = Nil
  }
;;

let nvim_buf_set_var ~buffer ~name ~value =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let name = Phantom.to_msgpack String name in
  let value = Phantom.to_msgpack Object value in
  { Api_result.name = "nvim_buf_set_var"
  ; params = [ buffer; name; value ]
  ; witness = Nil
  }
;;

let nvim_buf_del_var ~buffer ~name =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_buf_del_var"; params = [ buffer; name ]; witness = Nil }
;;

let nvim_buf_get_name ~buffer =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  { Api_result.name = "nvim_buf_get_name"; params = [ buffer ]; witness = String }
;;

let nvim_buf_set_name ~buffer ~name =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_buf_set_name"; params = [ buffer; name ]; witness = Nil }
;;

let nvim_buf_is_loaded ~buffer =
  let buffer = Phantom.to_msgpack Buffer buffer in
  { Api_result.name = "nvim_buf_is_loaded"; params = [ buffer ]; witness = Bool }
;;

let nvim_buf_delete ~buffer ~opts =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_buf_delete"; params = [ buffer; opts ]; witness = Nil }
;;

let nvim_buf_is_valid ~buffer =
  let buffer = Phantom.to_msgpack Buffer buffer in
  { Api_result.name = "nvim_buf_is_valid"; params = [ buffer ]; witness = Bool }
;;

let nvim_buf_del_mark ~buffer ~name =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_buf_del_mark"; params = [ buffer; name ]; witness = Bool }
;;

let nvim_buf_set_mark ~buffer ~name ~line ~col ~opts =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let name = Phantom.to_msgpack String name in
  let line = Phantom.to_msgpack Int line in
  let col = Phantom.to_msgpack Int col in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_buf_set_mark"
  ; params = [ buffer; name; line; col; opts ]
  ; witness = Bool
  }
;;

let nvim_buf_get_mark ~buffer ~name =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_buf_get_mark"
  ; params = [ buffer; name ]
  ; witness = Tuple2 (Int, Int)
  }
;;

let nvim_parse_cmd ~str ~opts =
  let str = Phantom.to_msgpack String str in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_parse_cmd"; params = [ str; opts ]; witness = Dict }
;;

let nvim_cmd ~cmd ~opts =
  let cmd = Phantom.to_msgpack Dict cmd in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_cmd"; params = [ cmd; opts ]; witness = String }
;;

let nvim_create_user_command ~name ~command ~opts =
  let name = Phantom.to_msgpack String name in
  let command = Phantom.to_msgpack Object command in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_create_user_command"
  ; params = [ name; command; opts ]
  ; witness = Nil
  }
;;

let nvim_del_user_command ~name =
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_del_user_command"; params = [ name ]; witness = Nil }
;;

let nvim_buf_create_user_command ~buffer ~name ~command ~opts =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let name = Phantom.to_msgpack String name in
  let command = Phantom.to_msgpack Object command in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_buf_create_user_command"
  ; params = [ buffer; name; command; opts ]
  ; witness = Nil
  }
;;

let nvim_buf_del_user_command ~buffer ~name =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_buf_del_user_command"
  ; params = [ buffer; name ]
  ; witness = Nil
  }
;;

let nvim_get_commands ~opts =
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_get_commands"; params = [ opts ]; witness = Dict }
;;

let nvim_buf_get_commands ~buffer ~opts =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_buf_get_commands"; params = [ buffer; opts ]; witness = Dict }
;;

let nvim_get_option_info ~name =
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_get_option_info"; params = [ name ]; witness = Dict }
;;

let nvim_create_namespace ~name =
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_create_namespace"; params = [ name ]; witness = Int }
;;

let nvim_get_namespaces =
  { Api_result.name = "nvim_get_namespaces"; params = []; witness = Dict }
;;

let nvim_buf_get_extmark_by_id ~buffer ~ns_id ~id ~opts =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let ns_id = Phantom.to_msgpack Int ns_id in
  let id = Phantom.to_msgpack Int id in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_buf_get_extmark_by_id"
  ; params = [ buffer; ns_id; id; opts ]
  ; witness = Array Int
  }
;;

let nvim_buf_get_extmarks ~buffer ~ns_id ~start ~end_ ~opts =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let ns_id = Phantom.to_msgpack Int ns_id in
  let start = Phantom.to_msgpack Object start in
  let end_ = Phantom.to_msgpack Object end_ in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_buf_get_extmarks"
  ; params = [ buffer; ns_id; start; end_; opts ]
  ; witness = Array Object
  }
;;

let nvim_buf_set_extmark ~buffer ~ns_id ~line ~col ~opts =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let ns_id = Phantom.to_msgpack Int ns_id in
  let line = Phantom.to_msgpack Int line in
  let col = Phantom.to_msgpack Int col in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_buf_set_extmark"
  ; params = [ buffer; ns_id; line; col; opts ]
  ; witness = Int
  }
;;

let nvim_buf_del_extmark ~buffer ~ns_id ~id =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let ns_id = Phantom.to_msgpack Int ns_id in
  let id = Phantom.to_msgpack Int id in
  { Api_result.name = "nvim_buf_del_extmark"
  ; params = [ buffer; ns_id; id ]
  ; witness = Bool
  }
;;

let nvim_buf_add_highlight ~buffer ~ns_id ~hl_group ~line ~col_start ~col_end =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let ns_id = Phantom.to_msgpack Int ns_id in
  let hl_group = Phantom.to_msgpack String hl_group in
  let line = Phantom.to_msgpack Int line in
  let col_start = Phantom.to_msgpack Int col_start in
  let col_end = Phantom.to_msgpack Int col_end in
  { Api_result.name = "nvim_buf_add_highlight"
  ; params = [ buffer; ns_id; hl_group; line; col_start; col_end ]
  ; witness = Int
  }
;;

let nvim_buf_clear_namespace ~buffer ~ns_id ~line_start ~line_end =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let ns_id = Phantom.to_msgpack Int ns_id in
  let line_start = Phantom.to_msgpack Int line_start in
  let line_end = Phantom.to_msgpack Int line_end in
  { Api_result.name = "nvim_buf_clear_namespace"
  ; params = [ buffer; ns_id; line_start; line_end ]
  ; witness = Nil
  }
;;

let nvim_get_option_value ~name ~opts =
  let name = Phantom.to_msgpack String name in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_get_option_value"; params = [ name; opts ]; witness = Object }
;;

let nvim_set_option_value ~name ~value ~opts =
  let name = Phantom.to_msgpack String name in
  let value = Phantom.to_msgpack Object value in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_set_option_value"
  ; params = [ name; value; opts ]
  ; witness = Nil
  }
;;

let nvim_get_all_options_info =
  { Api_result.name = "nvim_get_all_options_info"; params = []; witness = Dict }
;;

let nvim_get_option_info2 ~name ~opts =
  let name = Phantom.to_msgpack String name in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_get_option_info2"; params = [ name; opts ]; witness = Dict }
;;

let nvim_set_option ~name ~value =
  let name = Phantom.to_msgpack String name in
  let value = Phantom.to_msgpack Object value in
  { Api_result.name = "nvim_set_option"; params = [ name; value ]; witness = Nil }
;;

let nvim_get_option ~name =
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_get_option"; params = [ name ]; witness = Object }
;;

let nvim_buf_get_option ~buffer ~name =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_buf_get_option"; params = [ buffer; name ]; witness = Object }
;;

let nvim_buf_set_option ~buffer ~name ~value =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let name = Phantom.to_msgpack String name in
  let value = Phantom.to_msgpack Object value in
  { Api_result.name = "nvim_buf_set_option"
  ; params = [ buffer; name; value ]
  ; witness = Nil
  }
;;

let nvim_win_get_option ~window ~name =
  let window = Window.Or_current.to_msgpack window in
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_win_get_option"; params = [ window; name ]; witness = Object }
;;

let nvim_win_set_option ~window ~name ~value =
  let window = Window.Or_current.to_msgpack window in
  let name = Phantom.to_msgpack String name in
  let value = Phantom.to_msgpack Object value in
  { Api_result.name = "nvim_win_set_option"
  ; params = [ window; name; value ]
  ; witness = Nil
  }
;;

let nvim_tabpage_list_wins ~tabpage =
  let tabpage = Tabpage.Or_current.to_msgpack tabpage in
  { Api_result.name = "nvim_tabpage_list_wins"
  ; params = [ tabpage ]
  ; witness = Array Window
  }
;;

let nvim_tabpage_get_var ~tabpage ~name =
  let tabpage = Tabpage.Or_current.to_msgpack tabpage in
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_tabpage_get_var"
  ; params = [ tabpage; name ]
  ; witness = Object
  }
;;

let nvim_tabpage_set_var ~tabpage ~name ~value =
  let tabpage = Tabpage.Or_current.to_msgpack tabpage in
  let name = Phantom.to_msgpack String name in
  let value = Phantom.to_msgpack Object value in
  { Api_result.name = "nvim_tabpage_set_var"
  ; params = [ tabpage; name; value ]
  ; witness = Nil
  }
;;

let nvim_tabpage_del_var ~tabpage ~name =
  let tabpage = Tabpage.Or_current.to_msgpack tabpage in
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_tabpage_del_var"; params = [ tabpage; name ]; witness = Nil }
;;

let nvim_tabpage_get_win ~tabpage =
  let tabpage = Tabpage.Or_current.to_msgpack tabpage in
  { Api_result.name = "nvim_tabpage_get_win"; params = [ tabpage ]; witness = Window }
;;

let nvim_tabpage_get_number ~tabpage =
  let tabpage = Tabpage.Or_current.to_msgpack tabpage in
  { Api_result.name = "nvim_tabpage_get_number"; params = [ tabpage ]; witness = Int }
;;

let nvim_tabpage_is_valid ~tabpage =
  let tabpage = Phantom.to_msgpack Tabpage tabpage in
  { Api_result.name = "nvim_tabpage_is_valid"; params = [ tabpage ]; witness = Bool }
;;

let nvim_ui_attach ~width ~height ~options =
  let width = Phantom.to_msgpack Int width in
  let height = Phantom.to_msgpack Int height in
  let options = Phantom.to_msgpack Dict options in
  { Api_result.name = "nvim_ui_attach"
  ; params = [ width; height; options ]
  ; witness = Nil
  }
;;

let nvim_ui_set_focus ~gained =
  let gained = Phantom.to_msgpack Bool gained in
  { Api_result.name = "nvim_ui_set_focus"; params = [ gained ]; witness = Nil }
;;

let nvim_ui_detach = { Api_result.name = "nvim_ui_detach"; params = []; witness = Nil }

let nvim_ui_try_resize ~width ~height =
  let width = Phantom.to_msgpack Int width in
  let height = Phantom.to_msgpack Int height in
  { Api_result.name = "nvim_ui_try_resize"; params = [ width; height ]; witness = Nil }
;;

let nvim_ui_set_option ~name ~value =
  let name = Phantom.to_msgpack String name in
  let value = Phantom.to_msgpack Object value in
  { Api_result.name = "nvim_ui_set_option"; params = [ name; value ]; witness = Nil }
;;

let nvim_ui_try_resize_grid ~grid ~width ~height =
  let grid = Phantom.to_msgpack Int grid in
  let width = Phantom.to_msgpack Int width in
  let height = Phantom.to_msgpack Int height in
  { Api_result.name = "nvim_ui_try_resize_grid"
  ; params = [ grid; width; height ]
  ; witness = Nil
  }
;;

let nvim_ui_pum_set_height ~height =
  let height = Phantom.to_msgpack Int height in
  { Api_result.name = "nvim_ui_pum_set_height"; params = [ height ]; witness = Nil }
;;

let nvim_ui_pum_set_bounds ~width ~height ~row ~col =
  let width = Phantom.to_msgpack Float width in
  let height = Phantom.to_msgpack Float height in
  let row = Phantom.to_msgpack Float row in
  let col = Phantom.to_msgpack Float col in
  { Api_result.name = "nvim_ui_pum_set_bounds"
  ; params = [ width; height; row; col ]
  ; witness = Nil
  }
;;

let nvim_get_hl_id_by_name ~name =
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_get_hl_id_by_name"; params = [ name ]; witness = Int }
;;

let nvim_get_hl ~ns_id ~opts =
  let ns_id = Phantom.to_msgpack Int ns_id in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_get_hl"; params = [ ns_id; opts ]; witness = Dict }
;;

let nvim_set_hl ~ns_id ~name ~val_ =
  let ns_id = Phantom.to_msgpack Int ns_id in
  let name = Phantom.to_msgpack String name in
  let val_ = Phantom.to_msgpack Dict val_ in
  { Api_result.name = "nvim_set_hl"; params = [ ns_id; name; val_ ]; witness = Nil }
;;

let nvim_set_hl_ns ~ns_id =
  let ns_id = Phantom.to_msgpack Int ns_id in
  { Api_result.name = "nvim_set_hl_ns"; params = [ ns_id ]; witness = Nil }
;;

let nvim_feedkeys ~keys ~mode ~escape_ks =
  let keys = Phantom.to_msgpack String keys in
  let mode = Phantom.to_msgpack String mode in
  let escape_ks = Phantom.to_msgpack Bool escape_ks in
  { Api_result.name = "nvim_feedkeys"; params = [ keys; mode; escape_ks ]; witness = Nil }
;;

let nvim_input ~keys =
  let keys = Phantom.to_msgpack String keys in
  { Api_result.name = "nvim_input"; params = [ keys ]; witness = Int }
;;

let nvim_input_mouse ~button ~action ~modifier ~grid ~row ~col =
  let button = Phantom.to_msgpack String button in
  let action = Phantom.to_msgpack String action in
  let modifier = Phantom.to_msgpack String modifier in
  let grid = Phantom.to_msgpack Int grid in
  let row = Phantom.to_msgpack Int row in
  let col = Phantom.to_msgpack Int col in
  { Api_result.name = "nvim_input_mouse"
  ; params = [ button; action; modifier; grid; row; col ]
  ; witness = Nil
  }
;;

let nvim_replace_termcodes ~str ~from_part ~do_lt ~special =
  let str = Phantom.to_msgpack String str in
  let from_part = Phantom.to_msgpack Bool from_part in
  let do_lt = Phantom.to_msgpack Bool do_lt in
  let special = Phantom.to_msgpack Bool special in
  { Api_result.name = "nvim_replace_termcodes"
  ; params = [ str; from_part; do_lt; special ]
  ; witness = String
  }
;;

let nvim_exec_lua ~code ~args =
  let code = Phantom.to_msgpack String code in
  let args = Phantom.to_msgpack (Array Object) args in
  { Api_result.name = "nvim_exec_lua"; params = [ code; args ]; witness = Object }
;;

let nvim_notify ~msg ~log_level ~opts =
  let msg = Phantom.to_msgpack String msg in
  let log_level = Phantom.to_msgpack Int log_level in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_notify"; params = [ msg; log_level; opts ]; witness = Object }
;;

let nvim_strwidth ~text =
  let text = Phantom.to_msgpack String text in
  { Api_result.name = "nvim_strwidth"; params = [ text ]; witness = Int }
;;

let nvim_list_runtime_paths =
  { Api_result.name = "nvim_list_runtime_paths"; params = []; witness = Array String }
;;

let nvim_get_runtime_file ~name ~all =
  let name = Phantom.to_msgpack String name in
  let all = Phantom.to_msgpack Bool all in
  { Api_result.name = "nvim_get_runtime_file"
  ; params = [ name; all ]
  ; witness = Array String
  }
;;

let nvim_set_current_dir ~dir =
  let dir = Phantom.to_msgpack String dir in
  { Api_result.name = "nvim_set_current_dir"; params = [ dir ]; witness = Nil }
;;

let nvim_get_current_line =
  { Api_result.name = "nvim_get_current_line"; params = []; witness = String }
;;

let nvim_set_current_line ~line =
  let line = Phantom.to_msgpack String line in
  { Api_result.name = "nvim_set_current_line"; params = [ line ]; witness = Nil }
;;

let nvim_del_current_line =
  { Api_result.name = "nvim_del_current_line"; params = []; witness = Nil }
;;

let nvim_get_var ~name =
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_get_var"; params = [ name ]; witness = Object }
;;

let nvim_set_var ~name ~value =
  let name = Phantom.to_msgpack String name in
  let value = Phantom.to_msgpack Object value in
  { Api_result.name = "nvim_set_var"; params = [ name; value ]; witness = Nil }
;;

let nvim_del_var ~name =
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_del_var"; params = [ name ]; witness = Nil }
;;

let nvim_get_vvar ~name =
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_get_vvar"; params = [ name ]; witness = Object }
;;

let nvim_set_vvar ~name ~value =
  let name = Phantom.to_msgpack String name in
  let value = Phantom.to_msgpack Object value in
  { Api_result.name = "nvim_set_vvar"; params = [ name; value ]; witness = Nil }
;;

let nvim_echo ~chunks ~history ~opts =
  let chunks = Phantom.to_msgpack (Array Object) chunks in
  let history = Phantom.to_msgpack Bool history in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_echo"; params = [ chunks; history; opts ]; witness = Nil }
;;

let nvim_out_write ~str =
  let str = Phantom.to_msgpack String str in
  { Api_result.name = "nvim_out_write"; params = [ str ]; witness = Nil }
;;

let nvim_err_write ~str =
  let str = Phantom.to_msgpack String str in
  { Api_result.name = "nvim_err_write"; params = [ str ]; witness = Nil }
;;

let nvim_err_writeln ~str =
  let str = Phantom.to_msgpack String str in
  { Api_result.name = "nvim_err_writeln"; params = [ str ]; witness = Nil }
;;

let nvim_list_bufs =
  { Api_result.name = "nvim_list_bufs"; params = []; witness = Array Buffer }
;;

let nvim_get_current_buf =
  { Api_result.name = "nvim_get_current_buf"; params = []; witness = Buffer }
;;

let nvim_set_current_buf ~buffer =
  let buffer = Phantom.to_msgpack Buffer buffer in
  { Api_result.name = "nvim_set_current_buf"; params = [ buffer ]; witness = Nil }
;;

let nvim_list_wins =
  { Api_result.name = "nvim_list_wins"; params = []; witness = Array Window }
;;

let nvim_get_current_win =
  { Api_result.name = "nvim_get_current_win"; params = []; witness = Window }
;;

let nvim_set_current_win ~window =
  let window = Phantom.to_msgpack Window window in
  { Api_result.name = "nvim_set_current_win"; params = [ window ]; witness = Nil }
;;

let nvim_create_buf ~listed ~scratch =
  let listed = Phantom.to_msgpack Bool listed in
  let scratch = Phantom.to_msgpack Bool scratch in
  { Api_result.name = "nvim_create_buf"; params = [ listed; scratch ]; witness = Buffer }
;;

let nvim_open_term ~buffer ~opts =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_open_term"; params = [ buffer; opts ]; witness = Int }
;;

let nvim_chan_send ~chan ~data =
  let chan = Phantom.to_msgpack Int chan in
  let data = Phantom.to_msgpack String data in
  { Api_result.name = "nvim_chan_send"; params = [ chan; data ]; witness = Nil }
;;

let nvim_list_tabpages =
  { Api_result.name = "nvim_list_tabpages"; params = []; witness = Array Tabpage }
;;

let nvim_get_current_tabpage =
  { Api_result.name = "nvim_get_current_tabpage"; params = []; witness = Tabpage }
;;

let nvim_set_current_tabpage ~tabpage =
  let tabpage = Phantom.to_msgpack Tabpage tabpage in
  { Api_result.name = "nvim_set_current_tabpage"; params = [ tabpage ]; witness = Nil }
;;

let nvim_paste ~data ~crlf ~phase =
  let data = Phantom.to_msgpack String data in
  let crlf = Phantom.to_msgpack Bool crlf in
  let phase = Phantom.to_msgpack Int phase in
  { Api_result.name = "nvim_paste"; params = [ data; crlf; phase ]; witness = Bool }
;;

let nvim_put ~lines ~type_ ~after ~follow =
  let lines = Phantom.to_msgpack (Array String) lines in
  let type_ = Phantom.to_msgpack String type_ in
  let after = Phantom.to_msgpack Bool after in
  let follow = Phantom.to_msgpack Bool follow in
  { Api_result.name = "nvim_put"
  ; params = [ lines; type_; after; follow ]
  ; witness = Nil
  }
;;

let nvim_subscribe ~event =
  let event = Phantom.to_msgpack String event in
  { Api_result.name = "nvim_subscribe"; params = [ event ]; witness = Nil }
;;

let nvim_unsubscribe ~event =
  let event = Phantom.to_msgpack String event in
  { Api_result.name = "nvim_unsubscribe"; params = [ event ]; witness = Nil }
;;

let nvim_get_color_by_name ~name =
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_get_color_by_name"; params = [ name ]; witness = Int }
;;

let nvim_get_color_map =
  { Api_result.name = "nvim_get_color_map"; params = []; witness = Dict }
;;

let nvim_get_context ~opts =
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_get_context"; params = [ opts ]; witness = Dict }
;;

let nvim_load_context ~dict =
  let dict = Phantom.to_msgpack Dict dict in
  { Api_result.name = "nvim_load_context"; params = [ dict ]; witness = Object }
;;

let nvim_get_mode = { Api_result.name = "nvim_get_mode"; params = []; witness = Dict }

let nvim_get_keymap ~mode =
  let mode = Phantom.to_msgpack String mode in
  { Api_result.name = "nvim_get_keymap"; params = [ mode ]; witness = Array Dict }
;;

let nvim_set_keymap ~mode ~lhs ~rhs ~opts =
  let mode = Phantom.to_msgpack String mode in
  let lhs = Phantom.to_msgpack String lhs in
  let rhs = Phantom.to_msgpack String rhs in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_set_keymap"
  ; params = [ mode; lhs; rhs; opts ]
  ; witness = Nil
  }
;;

let nvim_del_keymap ~mode ~lhs =
  let mode = Phantom.to_msgpack String mode in
  let lhs = Phantom.to_msgpack String lhs in
  { Api_result.name = "nvim_del_keymap"; params = [ mode; lhs ]; witness = Nil }
;;

let nvim_get_api_info =
  { Api_result.name = "nvim_get_api_info"; params = []; witness = Array Object }
;;

let nvim_set_client_info ~name ~version ~type_ ~methods ~attributes =
  let name = Phantom.to_msgpack String name in
  let version = Phantom.to_msgpack Dict version in
  let type_ = Phantom.to_msgpack String type_ in
  let methods = Phantom.to_msgpack Dict methods in
  let attributes = Phantom.to_msgpack Dict attributes in
  { Api_result.name = "nvim_set_client_info"
  ; params = [ name; version; type_; methods; attributes ]
  ; witness = Nil
  }
;;

let nvim_get_chan_info ~chan =
  let chan = Phantom.to_msgpack Int chan in
  { Api_result.name = "nvim_get_chan_info"; params = [ chan ]; witness = Dict }
;;

let nvim_list_chans =
  { Api_result.name = "nvim_list_chans"; params = []; witness = Array Object }
;;

let nvim_call_atomic ~calls =
  let calls = Phantom.to_msgpack (Array Object) calls in
  { Api_result.name = "nvim_call_atomic"; params = [ calls ]; witness = Array Object }
;;

let nvim_list_uis =
  { Api_result.name = "nvim_list_uis"; params = []; witness = Array Object }
;;

let nvim_get_proc_children ~pid =
  let pid = Phantom.to_msgpack Int pid in
  { Api_result.name = "nvim_get_proc_children"; params = [ pid ]; witness = Array Object }
;;

let nvim_get_proc ~pid =
  let pid = Phantom.to_msgpack Int pid in
  { Api_result.name = "nvim_get_proc"; params = [ pid ]; witness = Object }
;;

let nvim_select_popupmenu_item ~item ~insert ~finish ~opts =
  let item = Phantom.to_msgpack Int item in
  let insert = Phantom.to_msgpack Bool insert in
  let finish = Phantom.to_msgpack Bool finish in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_select_popupmenu_item"
  ; params = [ item; insert; finish; opts ]
  ; witness = Nil
  }
;;

let nvim_del_mark ~name =
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_del_mark"; params = [ name ]; witness = Bool }
;;

let nvim_get_mark ~name ~opts =
  let name = Phantom.to_msgpack String name in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_get_mark"; params = [ name; opts ]; witness = Array Object }
;;

let nvim_eval_statusline ~str ~opts =
  let str = Phantom.to_msgpack String str in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_eval_statusline"; params = [ str; opts ]; witness = Dict }
;;

let nvim_exec2 ~src ~opts =
  let src = Phantom.to_msgpack String src in
  let opts = Phantom.to_msgpack Dict opts in
  { Api_result.name = "nvim_exec2"; params = [ src; opts ]; witness = Dict }
;;

let nvim_command ~command =
  let command = Phantom.to_msgpack String command in
  { Api_result.name = "nvim_command"; params = [ command ]; witness = Nil }
;;

let nvim_eval ~expr =
  let expr = Phantom.to_msgpack String expr in
  { Api_result.name = "nvim_eval"; params = [ expr ]; witness = Object }
;;

let nvim_call_function ~fn ~args =
  let fn = Phantom.to_msgpack String fn in
  let args = Phantom.to_msgpack (Array Object) args in
  { Api_result.name = "nvim_call_function"; params = [ fn; args ]; witness = Object }
;;

let nvim_call_dict_function ~dict ~fn ~args =
  let dict = Phantom.to_msgpack Object dict in
  let fn = Phantom.to_msgpack String fn in
  let args = Phantom.to_msgpack (Array Object) args in
  { Api_result.name = "nvim_call_dict_function"
  ; params = [ dict; fn; args ]
  ; witness = Object
  }
;;

let nvim_parse_expression ~expr ~flags ~highlight =
  let expr = Phantom.to_msgpack String expr in
  let flags = Phantom.to_msgpack String flags in
  let highlight = Phantom.to_msgpack Bool highlight in
  { Api_result.name = "nvim_parse_expression"
  ; params = [ expr; flags; highlight ]
  ; witness = Dict
  }
;;

let nvim_open_win ~buffer ~enter ~config =
  let buffer = Buffer.Or_current.to_msgpack buffer in
  let enter = Phantom.to_msgpack Bool enter in
  let config = Phantom.to_msgpack Dict config in
  { Api_result.name = "nvim_open_win"
  ; params = [ buffer; enter; config ]
  ; witness = Window
  }
;;

let nvim_win_set_config ~window ~config =
  let window = Window.Or_current.to_msgpack window in
  let config = Phantom.to_msgpack Dict config in
  { Api_result.name = "nvim_win_set_config"; params = [ window; config ]; witness = Nil }
;;

let nvim_win_get_config ~window =
  let window = Window.Or_current.to_msgpack window in
  { Api_result.name = "nvim_win_get_config"; params = [ window ]; witness = Dict }
;;

let nvim_win_get_buf ~window =
  let window = Window.Or_current.to_msgpack window in
  { Api_result.name = "nvim_win_get_buf"; params = [ window ]; witness = Buffer }
;;

let nvim_win_set_buf ~window ~buffer =
  let window = Window.Or_current.to_msgpack window in
  let buffer = Phantom.to_msgpack Buffer buffer in
  { Api_result.name = "nvim_win_set_buf"; params = [ window; buffer ]; witness = Nil }
;;

let nvim_win_get_cursor ~window =
  let window = Window.Or_current.to_msgpack window in
  { Api_result.name = "nvim_win_get_cursor"
  ; params = [ window ]
  ; witness = Tuple2 (Int, Int)
  }
;;

let nvim_win_set_cursor ~window ~pos =
  let window = Window.Or_current.to_msgpack window in
  let pos = Phantom.to_msgpack (Tuple2 (Int, Int)) pos in
  { Api_result.name = "nvim_win_set_cursor"; params = [ window; pos ]; witness = Nil }
;;

let nvim_win_get_height ~window =
  let window = Window.Or_current.to_msgpack window in
  { Api_result.name = "nvim_win_get_height"; params = [ window ]; witness = Int }
;;

let nvim_win_set_height ~window ~height =
  let window = Window.Or_current.to_msgpack window in
  let height = Phantom.to_msgpack Int height in
  { Api_result.name = "nvim_win_set_height"; params = [ window; height ]; witness = Nil }
;;

let nvim_win_get_width ~window =
  let window = Window.Or_current.to_msgpack window in
  { Api_result.name = "nvim_win_get_width"; params = [ window ]; witness = Int }
;;

let nvim_win_set_width ~window ~width =
  let window = Window.Or_current.to_msgpack window in
  let width = Phantom.to_msgpack Int width in
  { Api_result.name = "nvim_win_set_width"; params = [ window; width ]; witness = Nil }
;;

let nvim_win_get_var ~window ~name =
  let window = Window.Or_current.to_msgpack window in
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_win_get_var"; params = [ window; name ]; witness = Object }
;;

let nvim_win_set_var ~window ~name ~value =
  let window = Window.Or_current.to_msgpack window in
  let name = Phantom.to_msgpack String name in
  let value = Phantom.to_msgpack Object value in
  { Api_result.name = "nvim_win_set_var"
  ; params = [ window; name; value ]
  ; witness = Nil
  }
;;

let nvim_win_del_var ~window ~name =
  let window = Window.Or_current.to_msgpack window in
  let name = Phantom.to_msgpack String name in
  { Api_result.name = "nvim_win_del_var"; params = [ window; name ]; witness = Nil }
;;

let nvim_win_get_position ~window =
  let window = Window.Or_current.to_msgpack window in
  { Api_result.name = "nvim_win_get_position"
  ; params = [ window ]
  ; witness = Tuple2 (Int, Int)
  }
;;

let nvim_win_get_tabpage ~window =
  let window = Window.Or_current.to_msgpack window in
  { Api_result.name = "nvim_win_get_tabpage"; params = [ window ]; witness = Tabpage }
;;

let nvim_win_get_number ~window =
  let window = Window.Or_current.to_msgpack window in
  { Api_result.name = "nvim_win_get_number"; params = [ window ]; witness = Int }
;;

let nvim_win_is_valid ~window =
  let window = Phantom.to_msgpack Window window in
  { Api_result.name = "nvim_win_is_valid"; params = [ window ]; witness = Bool }
;;

let nvim_win_hide ~window =
  let window = Window.Or_current.to_msgpack window in
  { Api_result.name = "nvim_win_hide"; params = [ window ]; witness = Nil }
;;

let nvim_win_close ~window ~force =
  let window = Window.Or_current.to_msgpack window in
  let force = Phantom.to_msgpack Bool force in
  { Api_result.name = "nvim_win_close"; params = [ window; force ]; witness = Nil }
;;

let nvim_win_set_hl_ns ~window ~ns_id =
  let window = Window.Or_current.to_msgpack window in
  let ns_id = Phantom.to_msgpack Int ns_id in
  { Api_result.name = "nvim_win_set_hl_ns"; params = [ window; ns_id ]; witness = Nil }
;;

module Options = struct
  module Data = struct
    module Type = struct
      type t =
        | String
        | Int
        | Bool
        | Char_list of { commalist : bool }
        | String_list
    end

    type t =
      { name : string
      ; global_local : bool
      ; type_ : Type.t
      }
  end

  module Global = struct
    let options : Data.t list =
      [ { name = "allowrevins"; global_local = false; type_ = Bool }
      ; { name = "ambiwidth"; global_local = false; type_ = String }
      ; { name = "arabicshape"; global_local = false; type_ = Bool }
      ; { name = "autochdir"; global_local = false; type_ = Bool }
      ; { name = "autowrite"; global_local = false; type_ = Bool }
      ; { name = "autowriteall"; global_local = false; type_ = Bool }
      ; { name = "background"; global_local = false; type_ = String }
      ; { name = "backspace"; global_local = false; type_ = String_list }
      ; { name = "backup"; global_local = false; type_ = Bool }
      ; { name = "backupdir"; global_local = false; type_ = String_list }
      ; { name = "backupext"; global_local = false; type_ = String }
      ; { name = "backupskip"; global_local = false; type_ = String_list }
      ; { name = "belloff"; global_local = false; type_ = String_list }
      ; { name = "breakat"
        ; global_local = false
        ; type_ = Char_list { commalist = false }
        }
      ; { name = "browsedir"; global_local = false; type_ = String }
      ; { name = "casemap"; global_local = false; type_ = String_list }
      ; { name = "cdhome"; global_local = false; type_ = Bool }
      ; { name = "cdpath"; global_local = false; type_ = String_list }
      ; { name = "cedit"; global_local = false; type_ = String }
      ; { name = "charconvert"; global_local = false; type_ = String }
      ; { name = "clipboard"; global_local = false; type_ = String_list }
      ; { name = "cmdheight"; global_local = false; type_ = Int }
      ; { name = "cmdwinheight"; global_local = false; type_ = Int }
      ; { name = "columns"; global_local = false; type_ = Int }
      ; { name = "compatible"; global_local = false; type_ = Bool }
      ; { name = "completeopt"; global_local = false; type_ = String_list }
      ; { name = "completeslash"; global_local = false; type_ = String }
      ; { name = "confirm"; global_local = false; type_ = Bool }
      ; { name = "cpoptions"
        ; global_local = false
        ; type_ = Char_list { commalist = false }
        }
      ; { name = "debug"; global_local = false; type_ = String }
      ; { name = "delcombine"; global_local = false; type_ = Bool }
      ; { name = "diffexpr"; global_local = false; type_ = String }
      ; { name = "diffopt"; global_local = false; type_ = String_list }
      ; { name = "digraph"; global_local = false; type_ = Bool }
      ; { name = "directory"; global_local = false; type_ = String_list }
      ; { name = "display"; global_local = false; type_ = String_list }
      ; { name = "eadirection"; global_local = false; type_ = String }
      ; { name = "emoji"; global_local = false; type_ = Bool }
      ; { name = "equalalways"; global_local = false; type_ = Bool }
      ; { name = "errorbells"; global_local = false; type_ = Bool }
      ; { name = "errorfile"; global_local = false; type_ = String }
      ; { name = "eventignore"; global_local = false; type_ = String_list }
      ; { name = "exrc"; global_local = false; type_ = Bool }
      ; { name = "fileencodings"; global_local = false; type_ = String_list }
      ; { name = "fileformats"; global_local = false; type_ = String_list }
      ; { name = "fileignorecase"; global_local = false; type_ = Bool }
      ; { name = "foldclose"; global_local = false; type_ = String_list }
      ; { name = "foldlevelstart"; global_local = false; type_ = Int }
      ; { name = "foldopen"; global_local = false; type_ = String_list }
      ; { name = "fsync"; global_local = false; type_ = Bool }
      ; { name = "grepformat"; global_local = false; type_ = String_list }
      ; { name = "guicursor"; global_local = false; type_ = String_list }
      ; { name = "guifont"; global_local = false; type_ = String_list }
      ; { name = "guifontwide"; global_local = false; type_ = String_list }
      ; { name = "guioptions"
        ; global_local = false
        ; type_ = Char_list { commalist = false }
        }
      ; { name = "guitablabel"; global_local = false; type_ = String }
      ; { name = "guitabtooltip"; global_local = false; type_ = String }
      ; { name = "helpfile"; global_local = false; type_ = String }
      ; { name = "helpheight"; global_local = false; type_ = Int }
      ; { name = "helplang"; global_local = false; type_ = String_list }
      ; { name = "hidden"; global_local = false; type_ = Bool }
      ; { name = "history"; global_local = false; type_ = Int }
      ; { name = "hlsearch"; global_local = false; type_ = Bool }
      ; { name = "icon"; global_local = false; type_ = Bool }
      ; { name = "iconstring"; global_local = false; type_ = String }
      ; { name = "ignorecase"; global_local = false; type_ = Bool }
      ; { name = "imcmdline"; global_local = false; type_ = Bool }
      ; { name = "imdisable"; global_local = false; type_ = Bool }
      ; { name = "inccommand"; global_local = false; type_ = String }
      ; { name = "incsearch"; global_local = false; type_ = Bool }
      ; { name = "isfname"; global_local = false; type_ = String_list }
      ; { name = "isident"; global_local = false; type_ = String_list }
      ; { name = "isprint"; global_local = false; type_ = String_list }
      ; { name = "joinspaces"; global_local = false; type_ = Bool }
      ; { name = "jumpoptions"; global_local = false; type_ = String_list }
      ; { name = "keymodel"; global_local = false; type_ = String_list }
      ; { name = "langmap"; global_local = false; type_ = String_list }
      ; { name = "langmenu"; global_local = false; type_ = String }
      ; { name = "langremap"; global_local = false; type_ = Bool }
      ; { name = "laststatus"; global_local = false; type_ = Int }
      ; { name = "lazyredraw"; global_local = false; type_ = Bool }
      ; { name = "lines"; global_local = false; type_ = Int }
      ; { name = "linespace"; global_local = false; type_ = Int }
      ; { name = "loadplugins"; global_local = false; type_ = Bool }
      ; { name = "magic"; global_local = false; type_ = Bool }
      ; { name = "makeef"; global_local = false; type_ = String }
      ; { name = "matchtime"; global_local = false; type_ = Int }
      ; { name = "maxfuncdepth"; global_local = false; type_ = Int }
      ; { name = "maxmapdepth"; global_local = false; type_ = Int }
      ; { name = "maxmempattern"; global_local = false; type_ = Int }
      ; { name = "menuitems"; global_local = false; type_ = Int }
      ; { name = "mkspellmem"; global_local = false; type_ = String }
      ; { name = "modelineexpr"; global_local = false; type_ = Bool }
      ; { name = "modelines"; global_local = false; type_ = Int }
      ; { name = "more"; global_local = false; type_ = Bool }
      ; { name = "mouse"; global_local = false; type_ = Char_list { commalist = false } }
      ; { name = "mousefocus"; global_local = false; type_ = Bool }
      ; { name = "mousehide"; global_local = false; type_ = Bool }
      ; { name = "mousemodel"; global_local = false; type_ = String }
      ; { name = "mousemoveevent"; global_local = false; type_ = Bool }
      ; { name = "mousescroll"; global_local = false; type_ = String_list }
      ; { name = "mouseshape"; global_local = false; type_ = String_list }
      ; { name = "mousetime"; global_local = false; type_ = Int }
      ; { name = "opendevice"; global_local = false; type_ = Bool }
      ; { name = "operatorfunc"; global_local = false; type_ = String }
      ; { name = "packpath"; global_local = false; type_ = String_list }
      ; { name = "paragraphs"; global_local = false; type_ = String }
      ; { name = "patchexpr"; global_local = false; type_ = String }
      ; { name = "patchmode"; global_local = false; type_ = String }
      ; { name = "previewheight"; global_local = false; type_ = Int }
      ; { name = "pumblend"; global_local = false; type_ = Int }
      ; { name = "pumheight"; global_local = false; type_ = Int }
      ; { name = "pumwidth"; global_local = false; type_ = Int }
      ; { name = "pyxversion"; global_local = false; type_ = Int }
      ; { name = "quickfixtextfunc"; global_local = false; type_ = String }
      ; { name = "redrawdebug"; global_local = false; type_ = String_list }
      ; { name = "redrawtime"; global_local = false; type_ = Int }
      ; { name = "regexpengine"; global_local = false; type_ = Int }
      ; { name = "report"; global_local = false; type_ = Int }
      ; { name = "revins"; global_local = false; type_ = Bool }
      ; { name = "ruler"; global_local = false; type_ = Bool }
      ; { name = "rulerformat"; global_local = false; type_ = String }
      ; { name = "runtimepath"; global_local = false; type_ = String_list }
      ; { name = "scrolljump"; global_local = false; type_ = Int }
      ; { name = "scrollopt"; global_local = false; type_ = String_list }
      ; { name = "sections"; global_local = false; type_ = String }
      ; { name = "selection"; global_local = false; type_ = String }
      ; { name = "selectmode"; global_local = false; type_ = String_list }
      ; { name = "shada"; global_local = false; type_ = String_list }
      ; { name = "shadafile"; global_local = false; type_ = String_list }
      ; { name = "shell"; global_local = false; type_ = String }
      ; { name = "shellcmdflag"; global_local = false; type_ = String }
      ; { name = "shellpipe"; global_local = false; type_ = String }
      ; { name = "shellquote"; global_local = false; type_ = String }
      ; { name = "shellredir"; global_local = false; type_ = String }
      ; { name = "shellslash"; global_local = false; type_ = Bool }
      ; { name = "shelltemp"; global_local = false; type_ = Bool }
      ; { name = "shellxescape"; global_local = false; type_ = String }
      ; { name = "shellxquote"; global_local = false; type_ = String }
      ; { name = "shiftround"; global_local = false; type_ = Bool }
      ; { name = "shortmess"
        ; global_local = false
        ; type_ = Char_list { commalist = false }
        }
      ; { name = "showcmd"; global_local = false; type_ = Bool }
      ; { name = "showcmdloc"; global_local = false; type_ = String }
      ; { name = "showfulltag"; global_local = false; type_ = Bool }
      ; { name = "showmatch"; global_local = false; type_ = Bool }
      ; { name = "showmode"; global_local = false; type_ = Bool }
      ; { name = "showtabline"; global_local = false; type_ = Int }
      ; { name = "sidescroll"; global_local = false; type_ = Int }
      ; { name = "smartcase"; global_local = false; type_ = Bool }
      ; { name = "smarttab"; global_local = false; type_ = Bool }
      ; { name = "spellsuggest"; global_local = false; type_ = String_list }
      ; { name = "splitbelow"; global_local = false; type_ = Bool }
      ; { name = "splitkeep"; global_local = false; type_ = String }
      ; { name = "splitright"; global_local = false; type_ = Bool }
      ; { name = "startofline"; global_local = false; type_ = Bool }
      ; { name = "suffixes"; global_local = false; type_ = String_list }
      ; { name = "switchbuf"; global_local = false; type_ = String_list }
      ; { name = "tabline"; global_local = false; type_ = String }
      ; { name = "tabpagemax"; global_local = false; type_ = Int }
      ; { name = "tagbsearch"; global_local = false; type_ = Bool }
      ; { name = "taglength"; global_local = false; type_ = Int }
      ; { name = "tagrelative"; global_local = false; type_ = Bool }
      ; { name = "tagstack"; global_local = false; type_ = Bool }
      ; { name = "termbidi"; global_local = false; type_ = Bool }
      ; { name = "termguicolors"; global_local = false; type_ = Bool }
      ; { name = "termpastefilter"; global_local = false; type_ = String_list }
      ; { name = "tildeop"; global_local = false; type_ = Bool }
      ; { name = "timeout"; global_local = false; type_ = Bool }
      ; { name = "timeoutlen"; global_local = false; type_ = Int }
      ; { name = "title"; global_local = false; type_ = Bool }
      ; { name = "titlelen"; global_local = false; type_ = Int }
      ; { name = "titleold"; global_local = false; type_ = String }
      ; { name = "titlestring"; global_local = false; type_ = String }
      ; { name = "ttimeout"; global_local = false; type_ = Bool }
      ; { name = "ttimeoutlen"; global_local = false; type_ = Int }
      ; { name = "undodir"; global_local = false; type_ = String_list }
      ; { name = "undoreload"; global_local = false; type_ = Int }
      ; { name = "updatecount"; global_local = false; type_ = Int }
      ; { name = "updatetime"; global_local = false; type_ = Int }
      ; { name = "verbose"; global_local = false; type_ = Int }
      ; { name = "verbosefile"; global_local = false; type_ = String }
      ; { name = "viewdir"; global_local = false; type_ = String }
      ; { name = "viminfo"; global_local = false; type_ = String }
      ; { name = "viminfofile"; global_local = false; type_ = String }
      ; { name = "visualbell"; global_local = false; type_ = Bool }
      ; { name = "warn"; global_local = false; type_ = Bool }
      ; { name = "whichwrap"
        ; global_local = false
        ; type_ = Char_list { commalist = true }
        }
      ; { name = "wildchar"; global_local = false; type_ = Int }
      ; { name = "wildcharm"; global_local = false; type_ = Int }
      ; { name = "wildignore"; global_local = false; type_ = String_list }
      ; { name = "wildignorecase"; global_local = false; type_ = Bool }
      ; { name = "wildmenu"; global_local = false; type_ = Bool }
      ; { name = "wildmode"; global_local = false; type_ = String_list }
      ; { name = "wildoptions"; global_local = false; type_ = String_list }
      ; { name = "winaltkeys"; global_local = false; type_ = String }
      ; { name = "window"; global_local = false; type_ = Int }
      ; { name = "winheight"; global_local = false; type_ = Int }
      ; { name = "winminheight"; global_local = false; type_ = Int }
      ; { name = "winminwidth"; global_local = false; type_ = Int }
      ; { name = "winwidth"; global_local = false; type_ = Int }
      ; { name = "wrapscan"; global_local = false; type_ = Bool }
      ; { name = "write"; global_local = false; type_ = Bool }
      ; { name = "writeany"; global_local = false; type_ = Bool }
      ; { name = "writebackup"; global_local = false; type_ = Bool }
      ; { name = "writedelay"; global_local = false; type_ = Int }
      ]
    ;;
  end

  module Buffer = struct
    let options : Data.t list =
      [ { name = "autoindent"; global_local = false; type_ = Bool }
      ; { name = "autoread"; global_local = true; type_ = Bool }
      ; { name = "backupcopy"; global_local = true; type_ = String_list }
      ; { name = "binary"; global_local = false; type_ = Bool }
      ; { name = "bomb"; global_local = false; type_ = Bool }
      ; { name = "bufhidden"; global_local = false; type_ = String }
      ; { name = "buflisted"; global_local = false; type_ = Bool }
      ; { name = "buftype"; global_local = false; type_ = String }
      ; { name = "channel"; global_local = false; type_ = Int }
      ; { name = "cindent"; global_local = false; type_ = Bool }
      ; { name = "cinkeys"; global_local = false; type_ = String_list }
      ; { name = "cinoptions"; global_local = false; type_ = String_list }
      ; { name = "cinscopedecls"; global_local = false; type_ = String_list }
      ; { name = "cinwords"; global_local = false; type_ = String_list }
      ; { name = "comments"; global_local = false; type_ = String_list }
      ; { name = "commentstring"; global_local = false; type_ = String }
      ; { name = "complete"; global_local = false; type_ = String_list }
      ; { name = "completefunc"; global_local = false; type_ = String }
      ; { name = "copyindent"; global_local = false; type_ = Bool }
      ; { name = "define"; global_local = true; type_ = String }
      ; { name = "dictionary"; global_local = true; type_ = String_list }
      ; { name = "endoffile"; global_local = false; type_ = Bool }
      ; { name = "endofline"; global_local = false; type_ = Bool }
      ; { name = "equalprg"; global_local = true; type_ = String }
      ; { name = "errorformat"; global_local = true; type_ = String_list }
      ; { name = "expandtab"; global_local = false; type_ = Bool }
      ; { name = "fileencoding"; global_local = false; type_ = String }
      ; { name = "fileformat"; global_local = false; type_ = String }
      ; { name = "filetype"; global_local = false; type_ = String }
      ; { name = "fixendofline"; global_local = false; type_ = Bool }
      ; { name = "formatexpr"; global_local = false; type_ = String }
      ; { name = "formatlistpat"; global_local = false; type_ = String }
      ; { name = "formatoptions"
        ; global_local = false
        ; type_ = Char_list { commalist = false }
        }
      ; { name = "formatprg"; global_local = true; type_ = String }
      ; { name = "grepprg"; global_local = true; type_ = String }
      ; { name = "iminsert"; global_local = false; type_ = Int }
      ; { name = "imsearch"; global_local = false; type_ = Int }
      ; { name = "include"; global_local = true; type_ = String }
      ; { name = "includeexpr"; global_local = false; type_ = String }
      ; { name = "indentexpr"; global_local = false; type_ = String }
      ; { name = "indentkeys"; global_local = false; type_ = String_list }
      ; { name = "infercase"; global_local = false; type_ = Bool }
      ; { name = "iskeyword"; global_local = false; type_ = String_list }
      ; { name = "keymap"; global_local = false; type_ = String }
      ; { name = "keywordprg"; global_local = true; type_ = String }
      ; { name = "lisp"; global_local = false; type_ = Bool }
      ; { name = "lispoptions"; global_local = false; type_ = String_list }
      ; { name = "lispwords"; global_local = true; type_ = String_list }
      ; { name = "makeencoding"; global_local = true; type_ = String }
      ; { name = "makeprg"; global_local = true; type_ = String }
      ; { name = "matchpairs"; global_local = false; type_ = String_list }
      ; { name = "modeline"; global_local = false; type_ = Bool }
      ; { name = "modifiable"; global_local = false; type_ = Bool }
      ; { name = "modified"; global_local = false; type_ = Bool }
      ; { name = "nrformats"; global_local = false; type_ = String_list }
      ; { name = "omnifunc"; global_local = false; type_ = String }
      ; { name = "path"; global_local = true; type_ = String_list }
      ; { name = "preserveindent"; global_local = false; type_ = Bool }
      ; { name = "quoteescape"; global_local = false; type_ = String }
      ; { name = "readonly"; global_local = false; type_ = Bool }
      ; { name = "scrollback"; global_local = false; type_ = Int }
      ; { name = "shiftwidth"; global_local = false; type_ = Int }
      ; { name = "smartindent"; global_local = false; type_ = Bool }
      ; { name = "softtabstop"; global_local = false; type_ = Int }
      ; { name = "spellcapcheck"; global_local = false; type_ = String }
      ; { name = "spellfile"; global_local = false; type_ = String_list }
      ; { name = "spelllang"; global_local = false; type_ = String_list }
      ; { name = "spelloptions"; global_local = false; type_ = String_list }
      ; { name = "suffixesadd"; global_local = false; type_ = String_list }
      ; { name = "swapfile"; global_local = false; type_ = Bool }
      ; { name = "synmaxcol"; global_local = false; type_ = Int }
      ; { name = "syntax"; global_local = false; type_ = String }
      ; { name = "tabstop"; global_local = false; type_ = Int }
      ; { name = "tagcase"; global_local = true; type_ = String }
      ; { name = "tagfunc"; global_local = false; type_ = String }
      ; { name = "tags"; global_local = true; type_ = String_list }
      ; { name = "textwidth"; global_local = false; type_ = Int }
      ; { name = "thesaurus"; global_local = true; type_ = String_list }
      ; { name = "thesaurusfunc"; global_local = true; type_ = String }
      ; { name = "undofile"; global_local = false; type_ = Bool }
      ; { name = "undolevels"; global_local = true; type_ = Int }
      ; { name = "varsofttabstop"; global_local = false; type_ = String_list }
      ; { name = "vartabstop"; global_local = false; type_ = String_list }
      ; { name = "wrapmargin"; global_local = false; type_ = Int }
      ]
    ;;
  end

  module Window = struct
    let options : Data.t list =
      [ { name = "arabic"; global_local = false; type_ = Bool }
      ; { name = "breakindent"; global_local = false; type_ = Bool }
      ; { name = "breakindentopt"; global_local = false; type_ = String_list }
      ; { name = "colorcolumn"; global_local = false; type_ = String_list }
      ; { name = "concealcursor"; global_local = false; type_ = String }
      ; { name = "conceallevel"; global_local = false; type_ = Int }
      ; { name = "cursorbind"; global_local = false; type_ = Bool }
      ; { name = "cursorcolumn"; global_local = false; type_ = Bool }
      ; { name = "cursorline"; global_local = false; type_ = Bool }
      ; { name = "cursorlineopt"; global_local = false; type_ = String_list }
      ; { name = "diff"; global_local = false; type_ = Bool }
      ; { name = "fillchars"; global_local = true; type_ = String_list }
      ; { name = "foldcolumn"; global_local = false; type_ = String }
      ; { name = "foldenable"; global_local = false; type_ = Bool }
      ; { name = "foldexpr"; global_local = false; type_ = String }
      ; { name = "foldignore"; global_local = false; type_ = String }
      ; { name = "foldlevel"; global_local = false; type_ = Int }
      ; { name = "foldmarker"; global_local = false; type_ = String_list }
      ; { name = "foldmethod"; global_local = false; type_ = String }
      ; { name = "foldminlines"; global_local = false; type_ = Int }
      ; { name = "foldnestmax"; global_local = false; type_ = Int }
      ; { name = "foldtext"; global_local = false; type_ = String }
      ; { name = "linebreak"; global_local = false; type_ = Bool }
      ; { name = "list"; global_local = false; type_ = Bool }
      ; { name = "listchars"; global_local = true; type_ = String_list }
      ; { name = "number"; global_local = false; type_ = Bool }
      ; { name = "numberwidth"; global_local = false; type_ = Int }
      ; { name = "previewwindow"; global_local = false; type_ = Bool }
      ; { name = "relativenumber"; global_local = false; type_ = Bool }
      ; { name = "rightleft"; global_local = false; type_ = Bool }
      ; { name = "rightleftcmd"; global_local = false; type_ = String }
      ; { name = "scroll"; global_local = false; type_ = Int }
      ; { name = "scrollbind"; global_local = false; type_ = Bool }
      ; { name = "scrolloff"; global_local = true; type_ = Int }
      ; { name = "showbreak"; global_local = true; type_ = String }
      ; { name = "sidescrolloff"; global_local = true; type_ = Int }
      ; { name = "signcolumn"; global_local = false; type_ = String }
      ; { name = "spell"; global_local = false; type_ = Bool }
      ; { name = "statuscolumn"; global_local = false; type_ = String }
      ; { name = "statusline"; global_local = true; type_ = String }
      ; { name = "virtualedit"; global_local = true; type_ = String_list }
      ; { name = "winbar"; global_local = true; type_ = String }
      ; { name = "winblend"; global_local = false; type_ = Int }
      ; { name = "winfixheight"; global_local = false; type_ = Bool }
      ; { name = "winfixwidth"; global_local = false; type_ = Bool }
      ; { name = "winhighlight"; global_local = false; type_ = String_list }
      ; { name = "wrap"; global_local = false; type_ = Bool }
      ]
    ;;
  end
end
