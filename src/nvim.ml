module Unshadow = struct
  module Command = Command
end

open Core
module Command = Unshadow.Command

let list_chans =
  let open Api_call.Let_syntax in
  let%map result = Nvim_internal.nvim_list_chans |> Api_call.of_api_result in
  Or_error.bind
    ~f:(fun r -> List.map ~f:Channel_info.of_msgpack r |> Or_error.combine_errors)
    result
;;

let command_output ~command =
  Nvim_internal.nvim_command_output ~command |> Api_call.of_api_result
;;

let command ~command = Nvim_internal.nvim_command ~command |> Api_call.of_api_result

let list_bufs =
  let open Api_call.Let_syntax in
  let%map result = Nvim_internal.nvim_list_bufs |> Api_call.of_api_result in
  let open Or_error.Let_syntax in
  let%bind result = result in
  List.map result ~f:Nvim_internal.Buffer.of_msgpack |> Or_error.combine_errors
;;

let get_chan_info ~chan =
  let open Api_call.Let_syntax in
  let%map result = Nvim_internal.nvim_get_chan_info ~chan |> Api_call.of_api_result in
  Or_error.bind ~f:(fun r -> Channel_info.of_msgpack (Map r)) result
;;

let eval ~expr ~result_type =
  Nvim_internal.nvim_eval ~expr
  |> Api_call.of_api_result
  |> Api_call.Or_error.map ~f:(Extract.value result_type)
  |> Api_call.map ~f:Or_error.join
;;

let get_current_buf = Nvim_internal.nvim_get_current_buf |> Api_call.of_api_result

let set_current_buf ~buffer =
  Nvim_internal.nvim_set_current_buf ~buffer |> Api_call.of_api_result
;;

let feedkeys ~keys ~mode ~escape_csi =
  Nvim_internal.nvim_feedkeys ~keys ~mode ~escape_csi |> Api_call.of_api_result
;;

let set_client_info
      ?(version =
        { Client_info.Version.major = None
        ; minor = None
        ; patch = None
        ; prerelease = None
        ; commit = None
        })
      ?(methods = String.Map.empty)
      ?(attributes = String.Map.empty)
      ~name
      ~(type_ : Client_info.Client_type.t)
      ()
  =
  let module M = Msgpack in
  let version =
    match version with
    | { major; minor; patch; prerelease; commit } ->
      List.filter_opt
        [ Option.map major ~f:(fun i -> M.String "major", M.Integer i)
        ; Option.map minor ~f:(fun i -> M.String "minor", M.Integer i)
        ; Option.map patch ~f:(fun i -> M.String "patch", M.Integer i)
        ; Option.map prerelease ~f:(fun s -> M.String "prerelease", M.String s)
        ; Option.map commit ~f:(fun s -> M.String "commit", M.String s)
        ]
  in
  let type_ =
    match type_ with
    | `Remote -> "remote"
    | `Ui -> "ui"
    | `Embedder -> "embedder"
    | `Host -> "host"
    | `Plugin -> "plugin"
  in
  let convert_method { Client_info.Client_method.async; nargs } =
    M.Map
      ((M.String "async", M.Boolean async)
       ::
       List.map (Option.to_list nargs) ~f:(function
         | `Fixed i -> M.String "nargs", M.Integer i
         | `Range (lo, hi) -> M.String "nargs", M.Array [ Integer lo; Integer hi ]))
  in
  let methods =
    Map.map methods ~f:convert_method
    |> Map.to_alist
    |> List.map ~f:(fun (a, b) -> M.String a, b)
  in
  let attributes =
    Map.to_alist attributes |> List.map ~f:(fun (a, b) -> M.String a, M.String b)
  in
  Nvim_internal.nvim_set_client_info ~name ~version ~type_ ~methods ~attributes
  |> Api_call.of_api_result
;;

let get_current_win = Nvim_internal.nvim_get_current_win |> Api_call.of_api_result

let set_current_win ~window =
  Nvim_internal.nvim_set_current_win ~window |> Api_call.of_api_result
;;

let list_wins =
  let open Api_call.Let_syntax in
  let%map result = Nvim_internal.nvim_list_wins |> Api_call.of_api_result in
  Or_error.bind
    ~f:(fun r -> List.map r ~f:Window.of_msgpack |> Or_error.combine_errors)
    result
;;

let replace_termcodes ~str ~from_part ~do_lt ~special =
  Nvim_internal.nvim_replace_termcodes ~str ~from_part ~do_lt ~special
  |> Api_call.of_api_result
;;

let get_color_by_name ~name =
  Nvim_internal.nvim_get_color_by_name ~name
  |> Api_call.of_api_result
  |> Api_call.map_bind ~f:(function
    | -1 -> Or_error.error_s [%message "Unrecognized color" (name : string)]
    | color -> Color.True_color.of_24bit_int color)
;;

let get_color_map =
  Nvim_internal.nvim_get_color_map
  |> Api_call.of_api_result
  |> Api_call.map_bind ~f:(fun color_map ->
    color_map
    |> List.map ~f:(fun (name, bits) ->
      let open Or_error.Let_syntax in
      let%bind name = Extract.string name in
      let%bind bits = Extract.int bits in
      let%map color = Color.True_color.of_24bit_int bits in
      name, color)
    |> Or_error.combine_errors
    |> Or_error.bind ~f:String.Map.of_alist_or_error)
;;

let parse_highlight
      (type a)
      (msg : (Msgpack.t * Msgpack.t) list)
      ~(color : a Color.Kind.t)
  : a Color.Highlight.t Or_error.t
  =
  match msg with
  | [ (String "foreground", Integer fg); (String "background", Integer bg) ] ->
    let open Or_error.Let_syntax in
    let open Color in
    (match color with
     | True_color ->
       let%bind fg = True_color.of_24bit_int fg in
       let%bind bg = True_color.of_24bit_int bg in
       return { Highlight.fg; bg }
     | Color256 ->
       let%bind fg = Color256.of_8bit_int fg in
       let%bind bg = Color256.of_8bit_int bg in
       return { Highlight.fg; bg })
  | msg ->
    Or_error.error_s
      [%message "Unable to parse highlight response" (msg : (Msgpack.t * Msgpack.t) list)]
;;

let get_hl_by_name (type a) ~name ~(color : a Color.Kind.t) =
  let rgb =
    match color with
    | True_color -> true
    | Color256 -> false
  in
  Nvim_internal.nvim_get_hl_by_name ~name ~rgb
  |> Api_call.of_api_result
  |> Api_call.map_bind ~f:(parse_highlight ~color)
;;

let get_hl_by_id (type a) ~hl_id ~(color : a Color.Kind.t) =
  let rgb =
    match color with
    | True_color -> true
    | Color256 -> false
  in
  Nvim_internal.nvim_get_hl_by_id ~hl_id ~rgb
  |> Api_call.of_api_result
  |> Api_call.map_bind ~f:(parse_highlight ~color)
;;

let get_var ~name ~type_ =
  Nvim_internal.nvim_get_var ~name
  |> Api_call.of_api_result
  |> Api_call.map_bind ~f:(Extract.value type_)
;;

let set_var ~name ~type_ ~value =
  let value = Extract.inject type_ value in
  Nvim_internal.nvim_set_var ~name ~value |> Api_call.of_api_result
;;

module Fast = struct
  let get_mode =
    Nvim_internal.nvim_get_mode
    |> Api_call.of_api_result
    |> Api_call.map_bind ~f:(function
      | [ (String "mode", String mode); (String "blocking", Boolean blocking) ] ->
        (match Mode.of_mode_symbol mode with
         | Ok mode -> Ok { Mode.With_blocking_info.mode; blocking }
         | Error _ as error -> error)
      | msg ->
        Or_error.error_s
          [%message
            "Unable to parse [get_mode] response"
              (msg : (Msgpack.t * Msgpack.t) list)])
  ;;

  let input ~keys = Nvim_internal.nvim_input ~keys |> Api_call.of_api_result
end

module Untested = struct
  let strwidth ~text = Nvim_internal.nvim_strwidth ~text |> Api_call.of_api_result

  let list_runtime_paths =
    let open Api_call.Let_syntax in
    let%map result = Nvim_internal.nvim_list_runtime_paths |> Api_call.of_api_result in
    let open Or_error.Let_syntax in
    let%bind result = result in
    List.map result ~f:Extract.string |> Or_error.combine_errors
  ;;

  let set_current_dir ~dir =
    Nvim_internal.nvim_set_current_dir ~dir |> Api_call.of_api_result
  ;;

  let get_current_line = Nvim_internal.nvim_get_current_line |> Api_call.of_api_result

  let set_current_line ~line =
    Nvim_internal.nvim_set_current_line ~line |> Api_call.of_api_result
  ;;

  let del_current_line = Nvim_internal.nvim_del_current_line |> Api_call.of_api_result
  let del_var ~name = Nvim_internal.nvim_del_var ~name |> Api_call.of_api_result

  let get_vvar ~name ~type_ =
    Nvim_internal.nvim_get_vvar ~name
    |> Api_call.of_api_result
    |> Api_call.map_bind ~f:(Extract.value type_)
  ;;

  let get_option ~name ~type_ =
    Nvim_internal.nvim_get_option ~name
    |> Api_call.of_api_result
    |> Api_call.map_bind ~f:(Extract.value type_)
  ;;

  let set_option ~name ~type_ ~value =
    let value = Extract.inject type_ value in
    Nvim_internal.nvim_set_option ~name ~value |> Api_call.of_api_result
  ;;

  let out_write ~str = Nvim_internal.nvim_out_write ~str |> Api_call.of_api_result
  let err_write ~str = Nvim_internal.nvim_err_write ~str |> Api_call.of_api_result
  let err_writeln ~str = Nvim_internal.nvim_err_writeln ~str |> Api_call.of_api_result

  let list_tabpages =
    let open Api_call.Let_syntax in
    let%map result = Nvim_internal.nvim_list_tabpages |> Api_call.of_api_result in
    Or_error.bind
      ~f:(fun r -> List.map r ~f:Tabpage.of_msgpack |> Or_error.combine_errors)
      result
  ;;

  let get_current_tabpage =
    Nvim_internal.nvim_get_current_tabpage |> Api_call.of_api_result
  ;;

  let set_current_tabpage ~tabpage =
    Nvim_internal.nvim_set_current_tabpage ~tabpage |> Api_call.of_api_result
  ;;

  let subscribe ~event = Nvim_internal.nvim_subscribe ~event |> Api_call.of_api_result
  let unsubscribe ~event = Nvim_internal.nvim_unsubscribe ~event |> Api_call.of_api_result

  let get_user_defined_commands =
    let open Api_call.Let_syntax in
    let%map result =
      (* [opts] is not used by this version of Neovim, but may be used in the future. If
         we expose it, we should do so in a typeful way rather than asking the user to
         build [Msgpack.t] values. *)
      Nvim_internal.nvim_get_commands ~opts:[] |> Api_call.of_api_result
    in
    let open Or_error.Let_syntax in
    let%bind result = result in
    let%bind commands_with_names =
      List.map result ~f:(fun (name, command) ->
        let open Or_error.Let_syntax in
        let%bind n = Extract.string name in
        let%bind c = Command.of_msgpack command in
        return (n, c))
      |> Or_error.combine_errors
    in
    String.Map.of_alist_or_error commands_with_names
  ;;

  module Expert = struct
    let execute_lua ~code ~args =
      Nvim_internal.nvim_execute_lua ~code ~args |> Api_call.of_api_result
    ;;
  end
end

(* These functions are part of the Neovim API but are not exposed in VCaml. *)
module Unused = struct
  (* If we are going to expose this function we should only do it in a typesafe way
     similar to the way we expose [nvim_call_function] via [wrap_viml_function]. I'm not
     sure there's actually much value in exposing this function since OCaml plugins likely
     will not be using VimL OOP. *)
  let (_ : _) = Nvim_internal.nvim_call_dict_function

  (* We get the API info at codegen time, and we currently don't support any compatibility
     checking. VCaml models parts of Neovim that aren't strictly exposed in the Msgpack
     API, so this compatibility check wouldn't be especially helpful. *)
  let (_ : _) = Nvim_internal.nvim_get_api_info

  (* Since the goal of VCaml is to move away from writing any VimL, I would be surprised
     if we actually wanted to do analysis on a VimL AST (aside from Tree-sitter). *)
  let (_ : _) = Nvim_internal.nvim_parse_expression

  (* These functions are not related to Neovim and shouldn't be part of the API. *)
  let (_ : _) = Nvim_internal.nvim_get_proc
  let (_ : _) = Nvim_internal.nvim_get_proc_children

  let (_ : _) = Nvim_internal.nvim_ui_set_option
end
