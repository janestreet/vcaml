open Core
open Async
open Import
include Nvim_internal.Window

let get_buf ~(here : [%call_pos]) client t =
  Nvim_internal.nvim_win_get_buf ~window:t |> run ~here client
;;

let set_buf ~(here : [%call_pos]) client t ~buffer =
  Nvim_internal.nvim_win_set_buf ~window:t ~buffer |> run ~here client
;;

let exists ~(here : [%call_pos]) client t =
  Nvim_internal.nvim_win_is_valid ~window:t |> run ~here client
;;

module When_this_is_the_buffer's_last_window = struct
  type t =
    | Hide
    | Unload of { if_modified : [ `Hide | `Abort_if_hiding_is_disabled ] }
end

let close ~(here : [%call_pos]) client t ~when_this_is_the_buffer's_last_window =
  (match when_this_is_the_buffer's_last_window with
   | When_this_is_the_buffer's_last_window.Hide -> Nvim_internal.nvim_win_hide ~window:t
   | Unload { if_modified } ->
     let force =
       match if_modified with
       | `Hide -> true
       | `Abort_if_hiding_is_disabled -> false
     in
     Nvim_internal.nvim_win_close ~window:t ~force)
  |> run ~here client
;;

let get_height ~(here : [%call_pos]) client t =
  Nvim_internal.nvim_win_get_height ~window:t |> run ~here client
;;

let set_height ~(here : [%call_pos]) client t ~height =
  Nvim_internal.nvim_win_set_height ~window:t ~height |> run ~here client
;;

let get_width ~(here : [%call_pos]) client t =
  Nvim_internal.nvim_win_get_width ~window:t |> run ~here client
;;

let set_width ~(here : [%call_pos]) client t ~width =
  Nvim_internal.nvim_win_set_width ~window:t ~width |> run ~here client
;;

let get_cursor ~(here : [%call_pos]) client t =
  Nvim_internal.nvim_win_get_cursor ~window:t
  |> map_witness ~f:(fun (row, col) -> Ok { Position.One_indexed_row.row; col })
  |> run ~here client
;;

let set_cursor ~(here : [%call_pos]) client t { Position.One_indexed_row.row; col } =
  Nvim_internal.nvim_win_set_cursor ~window:t ~pos:(row, col) |> run ~here client
;;

let get_var ~(here : [%call_pos]) client t name ~type_ =
  Nvim_internal.nvim_win_get_var ~window:t ~name
  |> map_witness ~f:(Type.of_msgpack type_)
  |> run ~here client
;;

let set_var ~(here : [%call_pos]) client t name ~type_ ~value =
  let value = Type.to_msgpack type_ value in
  Nvim_internal.nvim_win_set_var ~window:t ~name ~value |> run ~here client
;;

let delete_var ~(here : [%call_pos]) client t name =
  Nvim_internal.nvim_win_del_var ~window:t ~name |> run ~here client
;;

let get_tab ~(here : [%call_pos]) client t =
  Nvim_internal.nvim_win_get_tabpage ~window:t |> run ~here client
;;

let get_number ~(here : [%call_pos]) client t =
  Nvim_internal.nvim_win_get_number ~window:t |> run ~here client
;;

let get_position ~(here : [%call_pos]) client t =
  Nvim_internal.nvim_win_get_position ~window:t
  |> map_witness ~f:(fun (row, col) -> Ok { Position.row; col })
  |> run ~here client
;;

module Config = struct
  module Border = struct
    type t =
      | Single_line
      | Double_line
      | Single_line_rounded_corners
      | Whitespace
      | Shadow
      | Custom of Highlighted_text.t
    [@@deriving sexp_of]

    let to_msgpack : t option -> Msgpack.t = function
      | None -> String "none"
      | Some Single_line -> String "single"
      | Some Double_line -> String "double"
      | Some Single_line_rounded_corners -> String "rounded"
      | Some Whitespace -> String "solid"
      | Some Shadow -> String "shadow"
      | Some (Custom text) ->
        (* Even though the example in the help uses plain strings instead of singleton
           string arrays for chunks without highlighting, singleton string arrays are
           accepted, so we can just use [Highlighted_text.to_msgpack] here. *)
        Array (Highlighted_text.to_msgpack text)
    ;;

    let of_msgpack : Msgpack.t -> t option Or_error.t = function
      | String "none" -> Ok None
      | String "single" -> Ok (Some Single_line)
      | String "double" -> Ok (Some Double_line)
      | String "rounded" -> Ok (Some Single_line_rounded_corners)
      | String "solid" -> Ok (Some Whitespace)
      | String "shadow" -> Ok (Some Shadow)
      | Array custom ->
        let%map.Or_error border =
          custom
          |> List.mapi ~f:(fun index piece ->
            match piece with
            | String text | Array [ String text ] ->
              Ok { Highlighted_text.Chunk.text; hl_group = None }
            | Array [ String text; String hl_group ] ->
              Ok { text; hl_group = Some hl_group }
            | _ ->
              Or_error.error_s
                [%message "Malformed [border] piece" (index : int) (piece : Msgpack.t)])
          |> Or_error.combine_errors
        in
        Some (Custom border)
      | border -> Or_error.error_s [%message "Malformed [border]" (border : Msgpack.t)]
    ;;
  end

  module Title = struct
    type pos =
      | Left
      | Center
      | Right
    [@@deriving sexp_of]

    type t =
      { pos : pos
      ; text : Highlighted_text.t
      }
    [@@deriving sexp_of]

    let to_msgpack_map title =
      match title with
      | None -> String.Map.empty
      | Some { text; pos } ->
        let pos =
          match pos with
          | Left -> "left"
          | Center -> "center"
          | Right -> "right"
        in
        [ "title_pos", Msgpack.String pos
        ; "title", Array (Highlighted_text.to_msgpack text)
        ]
        |> String.Map.of_alist_exn
    ;;

    let of_msgpack_map map =
      let open Or_error.Let_syntax in
      match Core.Map.find map "title" with
      | None -> return None
      | Some title ->
        let%bind pos =
          match%bind find_and_convert map "title_pos" (Type.of_msgpack String) with
          | None | Some "left" -> Ok Left
          | Some "center" -> Ok Center
          | Some "right" -> Ok Right
          | Some title_pos ->
            Or_error.error_s [%message "Unrecognized title position" title_pos]
        in
        let%bind text =
          match title with
          | String text ->
            Ok [ { Highlighted_text.Chunk.text; hl_group = Some "FloatTitle" } ]
          | Array chunks ->
            List.map chunks ~f:(function
              | Array [ String text ] ->
                Ok { Highlighted_text.Chunk.text; hl_group = None }
              | Array [ String text; String hl_group ] ->
                Ok { Highlighted_text.Chunk.text; hl_group = Some hl_group }
              | _ -> Or_error.error_s [%message "Malformed title" ~_:(title : Msgpack.t)])
            |> Result.all
          | _ -> Or_error.error_s [%message "Malformed title" ~_:(title : Msgpack.t)]
        in
        Ok (Some { pos; text })
    ;;
  end

  module Floating = struct
    module Corner = struct
      type t =
        | Top_left
        | Top_right
        | Bottom_left
        | Bottom_right
      [@@deriving sexp_of]

      let to_msgpack : t -> Msgpack.t = function
        | Top_left -> String "NW"
        | Top_right -> String "NE"
        | Bottom_left -> String "SW"
        | Bottom_right -> String "SE"
      ;;

      let of_msgpack msgpack =
        let open Or_error.Let_syntax in
        match%bind Type.of_msgpack String msgpack with
        | "NW" -> Ok Top_left
        | "NE" -> Ok Top_right
        | "SW" -> Ok Bottom_left
        | "SE" -> Ok Bottom_right
        | anchor ->
          Or_error.error_s [%message "Failed to parse anchor in window config" anchor]
      ;;

      module Position = struct
        type nonrec t =
          | Relative_to_editor of { pos : Position.t }
          | Relative_to_window of
              { window : Or_current.t
              ; pos : Position.t
              }
          | Relative_to_text_in_window of
              { window : Or_current.t
              ; text_pos : Position.t
              ; pos : Position.t option
              }
          | Relative_to_cursor_in_current_window of { pos : Position.t }
          | Relative_to_mouse of { pos : Position.t }
        [@@deriving sexp_of]

        let to_msgpack_map t =
          let map =
            match t with
            | Relative_to_editor { pos } ->
              [ "relative", Msgpack.String "editor"
              ; "row", Int pos.row
              ; "col", Int pos.col
              ]
            | Relative_to_window { window; pos } ->
              let msgpack =
                [ "relative", Msgpack.String "win"
                ; "row", Int pos.row
                ; "col", Int pos.col
                ]
              in
              (match window with
               | Current -> msgpack
               | Id t -> ("win", Type.to_msgpack Window t) :: msgpack)
            | Relative_to_text_in_window { window; text_pos; pos } ->
              let msgpack =
                [ "relative", Msgpack.String "win"
                ; "bufpos", Array [ Int text_pos.row; Int text_pos.col ]
                ]
              in
              let msgpack =
                match pos with
                | None -> msgpack
                | Some pos -> ("row", Int pos.row) :: ("col", Int pos.col) :: msgpack
              in
              (match window with
               | Current -> msgpack
               | Id t -> ("win", Type.to_msgpack Window t) :: msgpack)
            | Relative_to_cursor_in_current_window { pos } ->
              [ "relative", String "cursor"; "row", Int pos.row; "col", Int pos.col ]
            | Relative_to_mouse { pos } ->
              [ "relative", Msgpack.String "mouse"
              ; "row", Int pos.row
              ; "col", Int pos.col
              ]
          in
          String.Map.of_alist_exn map
        ;;

        let of_msgpack_map map ~corner =
          let open Or_error.Let_syntax in
          let int_of_msgpack_float msgpack =
            let%bind float = Type.of_msgpack Float msgpack in
            match Float.iround_nearest float with
            | Some int -> Ok int
            | None ->
              Or_error.error_s
                [%message "Failed to round float to nearest integer" ~_:(float : float)]
          in
          match%bind
            find_or_error_and_convert map "relative" (Type.of_msgpack String)
          with
          | "editor" ->
            let%bind row = find_or_error_and_convert map "row" int_of_msgpack_float in
            let%bind col = find_or_error_and_convert map "col" int_of_msgpack_float in
            Ok (Relative_to_editor { pos = { row; col } })
          | "cursor" ->
            let%bind row = find_or_error_and_convert map "row" (Type.of_msgpack Int) in
            let%bind col = find_or_error_and_convert map "col" (Type.of_msgpack Int) in
            Ok (Relative_to_cursor_in_current_window { pos = { row; col } })
          | "win" ->
            let%bind window =
              match%bind find_and_convert map "win" (Type.of_msgpack Window) with
              | None -> Ok Or_current.Current
              | Some t -> Ok (Id t)
            in
            (match%bind
               find_and_convert map "bufpos" (Type.of_msgpack (Tuple2 (Int, Int)))
             with
             | None ->
               let%bind row = find_or_error_and_convert map "row" int_of_msgpack_float in
               let%bind col = find_or_error_and_convert map "col" int_of_msgpack_float in
               Ok (Relative_to_window { window; pos = { row; col } })
             | Some (row, col) ->
               let text_pos = { Position.row; col } in
               let%bind row = find_and_convert map "row" int_of_msgpack_float in
               let%bind col = find_and_convert map "col" int_of_msgpack_float in
               let pos =
                 match row, col with
                 | None, None -> None
                 | Some row, None -> Some { Position.row; col = 0 }
                 | None, Some col ->
                   let row =
                     match corner with
                     | Top_left | Top_right -> 1
                     | Bottom_left | Bottom_right -> 0
                   in
                   Some { row; col }
                 | Some row, Some col -> Some { row; col }
               in
               Ok (Relative_to_text_in_window { window; text_pos; pos }))
          | "mouse" ->
            let%bind row = find_or_error_and_convert map "row" int_of_msgpack_float in
            let%bind col = find_or_error_and_convert map "col" int_of_msgpack_float in
            Ok (Relative_to_mouse { pos = { row; col } })
          | relative ->
            Or_error.error_s
              [%message
                "Unrecognized relative position spec for floating window" relative]
        ;;
      end
    end

    type t =
      { width : int
      ; height : int
      ; corner : Corner.t
      ; corner_pos : Corner.Position.t
      ; zindex : int option
      ; focusable : bool
      ; border : Border.t option
      ; title : Title.t option
      }
    [@@deriving sexp_of]

    let to_msgpack_map t =
      let map =
        [ "border", Border.to_msgpack t.border
        ; "width", Int t.width
        ; "height", Int t.height
        ; "anchor", Corner.to_msgpack t.corner
        ; "focusable", Bool t.focusable
        ]
      in
      let map =
        match t.zindex with
        | None -> map
        | Some zindex -> ("zindex", Int zindex) :: map
      in
      List.reduce_exn
        [ String.Map.of_alist_exn map
        ; Corner.Position.to_msgpack_map t.corner_pos
        ; Title.to_msgpack_map t.title
        ]
        ~f:
          (Core.Map.merge_skewed ~combine:(fun ~key _ _ ->
             raise_s [%message "BUG: Key appears twice in window config" key]))
    ;;

    let of_msgpack_map map =
      let open Or_error.Let_syntax in
      let%bind width = find_or_error_and_convert map "width" (Type.of_msgpack Int) in
      let%bind height = find_or_error_and_convert map "height" (Type.of_msgpack Int) in
      let%bind corner =
        find_and_convert map "anchor" Corner.of_msgpack
        >>| Option.value ~default:Corner.Top_left
      in
      let%bind corner_pos = Corner.Position.of_msgpack_map map ~corner in
      let%bind zindex = find_and_convert map "zindex" (Type.of_msgpack Int) in
      let%bind focusable =
        find_and_convert map "focusable" (Type.of_msgpack Bool)
        >>| Option.value ~default:true
      in
      let%bind border = find_and_convert map "border" Border.of_msgpack >>| Option.join in
      let%bind title = Title.of_msgpack_map map in
      return { width; height; corner; corner_pos; zindex; focusable; border; title }
    ;;
  end

  module External = struct
    type t =
      { width : int
      ; height : int
      ; focusable : bool
      ; border : Border.t option
      ; title : Title.t option
      }
    [@@deriving sexp_of]

    let to_msgpack_map t =
      let map =
        [ "border", Border.to_msgpack t.border
        ; "width", Int t.width
        ; "height", Int t.height
        ; "focusable", Bool t.focusable
        ; "external", Bool true
        ]
      in
      Core.Map.merge_skewed
        (String.Map.of_alist_exn map)
        (Title.to_msgpack_map t.title)
        ~combine:(fun ~key _ _ ->
          raise_s [%message "BUG: Key appears twice in window config" key])
    ;;

    let of_msgpack_map map =
      let open Or_error.Let_syntax in
      let%bind width = find_or_error_and_convert map "width" (Type.of_msgpack Int) in
      let%bind height = find_or_error_and_convert map "height" (Type.of_msgpack Int) in
      let%bind focusable =
        find_and_convert map "focusable" (Type.of_msgpack Bool)
        >>| Option.value ~default:true
      in
      let%bind border = find_and_convert map "border" Border.of_msgpack >>| Option.join in
      let%bind title = Title.of_msgpack_map map in
      return { width; height; focusable; border; title }
    ;;
  end

  type t =
    | Floating of Floating.t
    | External of External.t
  [@@deriving sexp_of]

  let of_msgpack_map map =
    let open Or_error.Let_syntax in
    match Core.Map.find map "external" with
    | Some (Msgpack.Bool true) ->
      let%map config = External.of_msgpack_map map in
      Some (External config)
    | _ ->
      (match Core.Map.find map "relative" with
       | Some (String "") -> Ok None
       | _ ->
         let%map config = Floating.of_msgpack_map map in
         Some (Floating config))
  ;;

  let to_msgpack_map = function
    | Floating config -> Floating.to_msgpack_map config
    | External config -> External.to_msgpack_map config
  ;;
end

let get_config ~(here : [%call_pos]) client t =
  Nvim_internal.nvim_win_get_config ~window:t
  |> map_witness ~f:Config.of_msgpack_map
  |> run ~here client
;;

let set_config ~(here : [%call_pos]) client t config =
  Nvim_internal.nvim_win_set_config ~window:t ~config:(Config.to_msgpack_map config)
  |> run ~here client
;;

let open_ ~here client ?(noautocmd = false) () ~buffer ~enter ~config ~minimal_style =
  let config =
    Config.to_msgpack_map config
    |> Core.Map.add_exn ~key:"noautocmd" ~data:(Bool noautocmd)
  in
  let config =
    match minimal_style with
    | false -> config
    | true -> Core.Map.add_exn config ~key:"style" ~data:(String "minimal")
  in
  Nvim_internal.nvim_open_win ~buffer ~enter ~config
  |> run ~here client
  >>| function
  | Error _ as error -> error
  | Ok t ->
    (match (t :> int) with
     | 0 ->
       Or_error.error_s
         [%message
           "nvim_open_win failed (returned 0)"
             (buffer : Nvim_internal.Buffer.Or_current.t)]
     | _ -> Ok t)
;;

let open_floating
  ~(here : [%call_pos])
  client
  ?noautocmd
  ()
  ~buffer
  ~enter
  ~config
  ~minimal_style
  =
  open_ ~here client ?noautocmd () ~buffer ~enter ~config:(Floating config) ~minimal_style
;;

let open_external
  ~(here : [%call_pos])
  client
  ?noautocmd
  ()
  ~buffer
  ~enter
  ~config
  ~minimal_style
  =
  open_ ~here client ?noautocmd () ~buffer ~enter ~config:(External config) ~minimal_style
;;

module Statusline = Statusline [@@alert "-vcaml_do_not_export"]
module Winbar = Statusline [@@alert "-vcaml_do_not_export"]
module Statuscolumn = Statusline [@@alert "-vcaml_do_not_export"]

module Fast = struct
  let eval_statusline
    ~(here : [%call_pos])
    client
    t
    ?max_width
    ?fill_char
    ~include_highlights
    statusline
    =
    Statusline.eval
      ~here
      client
      ?window:(Or_current.to_id t)
      ?max_width
      ?fill_char
      ~include_highlights
      Statusline
      statusline
  ;;

  let eval_statuscolumn
    ~(here : [%call_pos])
    client
    t
    ?max_width
    ?fill_char
    ~include_highlights
    ~one_indexed_row
    statuscolumn
    =
    Statusline.eval
      ~here
      client
      ?window:(Or_current.to_id t)
      ?max_width
      ?fill_char
      ~include_highlights
      (Statuscol { one_indexed_row })
      statuscolumn
  ;;

  let eval_winbar
    ~(here : [%call_pos])
    client
    t
    ?max_width
    ?fill_char
    ~include_highlights
    statuscolumn
    =
    Statusline.eval
      ~here
      client
      ?window:(Or_current.to_id t)
      ?max_width
      ?fill_char
      ~include_highlights
      Winbar
      statuscolumn
  ;;
end

module Option = struct
  (*$ Vcaml_cinaps.generate_options_impl ~scope:Window *)
  type 'a t =
    | Previewwindow : bool t
    | Scroll : int t
    | Winfixheight : bool t
    | Winfixwidth : bool t
  [@@deriving sexp_of]

  let to_string (type a) : a t -> string = function
    | Previewwindow -> "previewwindow"
    | Scroll -> "scroll"
    | Winfixheight -> "winfixheight"
    | Winfixwidth -> "winfixwidth"
  ;;

  let[@warning "-33"] of_msgpack (type a) (t : a t) msgpack : a Or_error.t =
    let open Option_helpers in
    match t with
    | Previewwindow -> Type.of_msgpack Bool msgpack
    | Scroll -> Type.of_msgpack Int msgpack
    | Winfixheight -> Type.of_msgpack Bool msgpack
    | Winfixwidth -> Type.of_msgpack Bool msgpack
  ;;

  let[@warning "-33"] to_msgpack (type a) (t : a t) (value : a) =
    let open Option_helpers in
    match t with
    | Previewwindow -> Type.to_msgpack Bool value
    | Scroll -> Type.to_msgpack Int value
    | Winfixheight -> Type.to_msgpack Bool value
    | Winfixwidth -> Type.to_msgpack Bool value
  ;;

  (*$*)

  let get_dynamic_info (type a) ~(here : [%call_pos]) client (t : a t) =
    Nvim_internal.nvim_get_option_info ~name:(to_string t)
    |> map_witness
         ~f:(Dynamic_option_info.of_msgpack_map ~default_of_msgpack:(of_msgpack t))
    |> run ~here client
  ;;

  let get ~(here : [%call_pos]) client window t =
    Nvim_internal.nvim_get_option_value
      ~name:(to_string t)
      ~opts:(String.Map.singleton "win" (Or_current.to_msgpack window))
    |> map_witness ~f:(of_msgpack t)
    |> run ~here client
  ;;

  let set ~(here : [%call_pos]) client window t value =
    Nvim_internal.nvim_set_option_value
      ~name:(to_string t)
      ~value:(to_msgpack t value)
      ~opts:
        ([ "win", Or_current.to_msgpack window; "scope", String "local" ]
         |> String.Map.of_alist_exn)
    |> run ~here client
  ;;

  module Per_buffer = struct
    (*$ Vcaml_cinaps.generate_options_impl ~scope:Window_per_buffer *)
    type ('a, 'global) t =
      | Arabic : (bool, [ `copied ]) t
      | Breakindent : (bool, [ `copied ]) t
      | Breakindentopt : (string list, [ `copied ]) t
      | Colorcolumn : (string list, [ `copied ]) t
      | Concealcursor : (string, [ `copied ]) t
      | Conceallevel : (int, [ `copied ]) t
      | Cursorbind : (bool, [ `copied ]) t
      | Cursorcolumn : (bool, [ `copied ]) t
      | Cursorline : (bool, [ `copied ]) t
      | Cursorlineopt : (string list, [ `copied ]) t
      | Diff : (bool, [ `copied ]) t
      | Fillchars : (string list, [ `global ]) t
      | Foldcolumn : (string, [ `copied ]) t
      | Foldenable : (bool, [ `copied ]) t
      | Foldexpr : (string, [ `copied ]) t
      | Foldignore : (string, [ `copied ]) t
      | Foldlevel : (int, [ `copied ]) t
      | Foldmarker : (string list, [ `copied ]) t
      | Foldmethod : (string, [ `copied ]) t
      | Foldminlines : (int, [ `copied ]) t
      | Foldnestmax : (int, [ `copied ]) t
      | Foldtext : (string, [ `copied ]) t
      | Linebreak : (bool, [ `copied ]) t
      | List : (bool, [ `copied ]) t
      | Listchars : (string list, [ `global ]) t
      | Number : (bool, [ `copied ]) t
      | Numberwidth : (int, [ `copied ]) t
      | Relativenumber : (bool, [ `copied ]) t
      | Rightleft : (bool, [ `copied ]) t
      | Rightleftcmd : (string, [ `copied ]) t
      | Scrollbind : (bool, [ `copied ]) t
      | Scrolloff : (int, [ `global ]) t
      | Showbreak : (string, [ `global ]) t
      | Sidescrolloff : (int, [ `global ]) t
      | Signcolumn : (string, [ `copied ]) t
      | Spell : (bool, [ `copied ]) t
      | Statuscolumn : (string, [ `copied ]) t
      | Statusline : (string, [ `global ]) t
      | Virtualedit : (string list, [ `global ]) t
      | Winbar : (string, [ `global ]) t
      | Winblend : (int, [ `copied ]) t
      | Winhighlight : (string list, [ `copied ]) t
      | Wrap : (bool, [ `copied ]) t
    [@@deriving sexp_of]

    let to_string (type a g) : (a, g) t -> string = function
      | Arabic -> "arabic"
      | Breakindent -> "breakindent"
      | Breakindentopt -> "breakindentopt"
      | Colorcolumn -> "colorcolumn"
      | Concealcursor -> "concealcursor"
      | Conceallevel -> "conceallevel"
      | Cursorbind -> "cursorbind"
      | Cursorcolumn -> "cursorcolumn"
      | Cursorline -> "cursorline"
      | Cursorlineopt -> "cursorlineopt"
      | Diff -> "diff"
      | Fillchars -> "fillchars"
      | Foldcolumn -> "foldcolumn"
      | Foldenable -> "foldenable"
      | Foldexpr -> "foldexpr"
      | Foldignore -> "foldignore"
      | Foldlevel -> "foldlevel"
      | Foldmarker -> "foldmarker"
      | Foldmethod -> "foldmethod"
      | Foldminlines -> "foldminlines"
      | Foldnestmax -> "foldnestmax"
      | Foldtext -> "foldtext"
      | Linebreak -> "linebreak"
      | List -> "list"
      | Listchars -> "listchars"
      | Number -> "number"
      | Numberwidth -> "numberwidth"
      | Relativenumber -> "relativenumber"
      | Rightleft -> "rightleft"
      | Rightleftcmd -> "rightleftcmd"
      | Scrollbind -> "scrollbind"
      | Scrolloff -> "scrolloff"
      | Showbreak -> "showbreak"
      | Sidescrolloff -> "sidescrolloff"
      | Signcolumn -> "signcolumn"
      | Spell -> "spell"
      | Statuscolumn -> "statuscolumn"
      | Statusline -> "statusline"
      | Virtualedit -> "virtualedit"
      | Winbar -> "winbar"
      | Winblend -> "winblend"
      | Winhighlight -> "winhighlight"
      | Wrap -> "wrap"
    ;;

    let of_msgpack (type a g) (t : (a, g) t) msgpack : a Or_error.t =
      let open Option_helpers in
      match t with
      | Arabic -> Type.of_msgpack Bool msgpack
      | Breakindent -> Type.of_msgpack Bool msgpack
      | Breakindentopt -> Type.of_msgpack (Custom (module String_list)) msgpack
      | Colorcolumn -> Type.of_msgpack (Custom (module String_list)) msgpack
      | Concealcursor -> Type.of_msgpack String msgpack
      | Conceallevel -> Type.of_msgpack Int msgpack
      | Cursorbind -> Type.of_msgpack Bool msgpack
      | Cursorcolumn -> Type.of_msgpack Bool msgpack
      | Cursorline -> Type.of_msgpack Bool msgpack
      | Cursorlineopt -> Type.of_msgpack (Custom (module String_list)) msgpack
      | Diff -> Type.of_msgpack Bool msgpack
      | Fillchars -> Type.of_msgpack (Custom (module String_list)) msgpack
      | Foldcolumn -> Type.of_msgpack String msgpack
      | Foldenable -> Type.of_msgpack Bool msgpack
      | Foldexpr -> Type.of_msgpack String msgpack
      | Foldignore -> Type.of_msgpack String msgpack
      | Foldlevel -> Type.of_msgpack Int msgpack
      | Foldmarker -> Type.of_msgpack (Custom (module String_list)) msgpack
      | Foldmethod -> Type.of_msgpack String msgpack
      | Foldminlines -> Type.of_msgpack Int msgpack
      | Foldnestmax -> Type.of_msgpack Int msgpack
      | Foldtext -> Type.of_msgpack String msgpack
      | Linebreak -> Type.of_msgpack Bool msgpack
      | List -> Type.of_msgpack Bool msgpack
      | Listchars -> Type.of_msgpack (Custom (module String_list)) msgpack
      | Number -> Type.of_msgpack Bool msgpack
      | Numberwidth -> Type.of_msgpack Int msgpack
      | Relativenumber -> Type.of_msgpack Bool msgpack
      | Rightleft -> Type.of_msgpack Bool msgpack
      | Rightleftcmd -> Type.of_msgpack String msgpack
      | Scrollbind -> Type.of_msgpack Bool msgpack
      | Scrolloff -> Type.of_msgpack Int msgpack
      | Showbreak -> Type.of_msgpack String msgpack
      | Sidescrolloff -> Type.of_msgpack Int msgpack
      | Signcolumn -> Type.of_msgpack String msgpack
      | Spell -> Type.of_msgpack Bool msgpack
      | Statuscolumn -> Type.of_msgpack String msgpack
      | Statusline -> Type.of_msgpack String msgpack
      | Virtualedit -> Type.of_msgpack (Custom (module String_list)) msgpack
      | Winbar -> Type.of_msgpack String msgpack
      | Winblend -> Type.of_msgpack Int msgpack
      | Winhighlight -> Type.of_msgpack (Custom (module String_list)) msgpack
      | Wrap -> Type.of_msgpack Bool msgpack
    ;;

    let to_msgpack (type a g) (t : (a, g) t) (value : a) =
      let open Option_helpers in
      match t with
      | Arabic -> Type.to_msgpack Bool value
      | Breakindent -> Type.to_msgpack Bool value
      | Breakindentopt -> Type.to_msgpack (Custom (module String_list)) value
      | Colorcolumn -> Type.to_msgpack (Custom (module String_list)) value
      | Concealcursor -> Type.to_msgpack String value
      | Conceallevel -> Type.to_msgpack Int value
      | Cursorbind -> Type.to_msgpack Bool value
      | Cursorcolumn -> Type.to_msgpack Bool value
      | Cursorline -> Type.to_msgpack Bool value
      | Cursorlineopt -> Type.to_msgpack (Custom (module String_list)) value
      | Diff -> Type.to_msgpack Bool value
      | Fillchars -> Type.to_msgpack (Custom (module String_list)) value
      | Foldcolumn -> Type.to_msgpack String value
      | Foldenable -> Type.to_msgpack Bool value
      | Foldexpr -> Type.to_msgpack String value
      | Foldignore -> Type.to_msgpack String value
      | Foldlevel -> Type.to_msgpack Int value
      | Foldmarker -> Type.to_msgpack (Custom (module String_list)) value
      | Foldmethod -> Type.to_msgpack String value
      | Foldminlines -> Type.to_msgpack Int value
      | Foldnestmax -> Type.to_msgpack Int value
      | Foldtext -> Type.to_msgpack String value
      | Linebreak -> Type.to_msgpack Bool value
      | List -> Type.to_msgpack Bool value
      | Listchars -> Type.to_msgpack (Custom (module String_list)) value
      | Number -> Type.to_msgpack Bool value
      | Numberwidth -> Type.to_msgpack Int value
      | Relativenumber -> Type.to_msgpack Bool value
      | Rightleft -> Type.to_msgpack Bool value
      | Rightleftcmd -> Type.to_msgpack String value
      | Scrollbind -> Type.to_msgpack Bool value
      | Scrolloff -> Type.to_msgpack Int value
      | Showbreak -> Type.to_msgpack String value
      | Sidescrolloff -> Type.to_msgpack Int value
      | Signcolumn -> Type.to_msgpack String value
      | Spell -> Type.to_msgpack Bool value
      | Statuscolumn -> Type.to_msgpack String value
      | Statusline -> Type.to_msgpack String value
      | Virtualedit -> Type.to_msgpack (Custom (module String_list)) value
      | Winbar -> Type.to_msgpack String value
      | Winblend -> Type.to_msgpack Int value
      | Winhighlight -> Type.to_msgpack (Custom (module String_list)) value
      | Wrap -> Type.to_msgpack Bool value
    ;;

    (*$*)

    let get_dynamic_info (type a g) ~(here : [%call_pos]) client (t : (a, g) t) =
      Nvim_internal.nvim_get_option_info ~name:(to_string t)
      |> map_witness
           ~f:(Dynamic_option_info.of_msgpack_map ~default_of_msgpack:(of_msgpack t))
      |> run ~here client
    ;;
  end

  let get_for_current_buffer_in_window ~(here : [%call_pos]) client window t =
    Nvim_internal.nvim_get_option_value
      ~name:(Per_buffer.to_string t)
      ~opts:(String.Map.singleton "win" (Or_current.to_msgpack window))
    |> map_witness ~f:(Per_buffer.of_msgpack t)
    |> run ~here client
  ;;

  let set_for_current_buffer_in_window ~(here : [%call_pos]) client window t value =
    Nvim_internal.nvim_set_option_value
      ~name:(Per_buffer.to_string t)
      ~value:(Per_buffer.to_msgpack t value)
      ~opts:
        ([ "win", Or_current.to_msgpack window; "scope", String "local" ]
         |> String.Map.of_alist_exn)
    |> run ~here client
  ;;

  let get_global ~(here : [%call_pos]) client window t =
    Nvim_internal.nvim_get_option_value
      ~name:(Per_buffer.to_string t)
      ~opts:
        ([ "win", Or_current.to_msgpack window; "scope", String "global" ]
         |> String.Map.of_alist_exn)
    |> map_witness ~f:(Per_buffer.of_msgpack t)
    |> run ~here client
  ;;

  let set_global ~(here : [%call_pos]) client window t value =
    Nvim_internal.nvim_set_option_value
      ~name:(Per_buffer.to_string t)
      ~value:(Per_buffer.to_msgpack t value)
      ~opts:
        ([ "win", Or_current.to_msgpack window; "scope", String "global" ]
         |> String.Map.of_alist_exn)
    |> run ~here client
  ;;

  let set_default ~(here : [%call_pos]) client t value =
    set_global ~here client Current t value
  ;;

  let get_default ~(here : [%call_pos]) client t = get_global ~here client Current t
  let set_for_new_buffers_opened_in_window = set_global
  let get_for_new_buffers_opened_in_window = get_global
end
