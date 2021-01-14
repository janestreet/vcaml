open! Core

let list_chans =
  let open Api_call.Let_syntax in
  let%map result = Nvim_internal.Wrappers.nvim_list_chans |> Api_call.of_api_result in
  Or_error.bind
    ~f:(fun r -> List.map ~f:Channel_info.of_msgpack r |> Or_error.combine_errors)
    result
;;

let command_output ~command =
  Nvim_internal.Wrappers.nvim_command_output ~command |> Api_call.of_api_result
;;

let command ~command =
  Nvim_internal.Wrappers.nvim_command ~command |> Api_call.of_api_result
;;

let list_bufs =
  let open Api_call.Let_syntax in
  let%map result = Nvim_internal.Wrappers.nvim_list_bufs |> Api_call.of_api_result in
  let open Or_error.Let_syntax in
  let%bind result = result in
  List.map result ~f:Buf.of_msgpack |> Or_error.combine_errors
;;

let get_chan_info ~chan =
  let open Api_call.Let_syntax in
  let%map result =
    Nvim_internal.Wrappers.nvim_get_chan_info ~chan |> Api_call.of_api_result
  in
  Or_error.bind ~f:(fun r -> Channel_info.of_msgpack (Map r)) result
;;

let call_atomic ~calls =
  Nvim_internal.Wrappers.nvim_call_atomic ~calls |> Api_call.of_api_result
;;

let eval ~expr = Nvim_internal.Wrappers.nvim_eval ~expr |> Api_call.of_api_result

let call_function ~fn ~args =
  Nvim_internal.Wrappers.nvim_call_function ~fn ~args |> Api_call.of_api_result
;;

let get_current_buf =
  Nvim_internal.Wrappers.nvim_get_current_buf |> Api_call.of_api_result
;;

let set_current_buf ~buffer =
  Nvim_internal.Wrappers.nvim_set_current_buf ~buffer |> Api_call.of_api_result
;;

let feedkeys ~keys ~mode ~escape_csi =
  Nvim_internal.Wrappers.nvim_feedkeys ~keys ~mode ~escape_csi |> Api_call.of_api_result
;;

let set_client_info
      ?(version =
        { Types.Client_info.major = None
        ; minor = None
        ; patch = None
        ; prerelease = None
        ; commit = None
        })
      ?(methods = String.Map.empty)
      ?(attributes = String.Map.empty)
      ~name
      ~(type_ : Types.Client_info.client_type)
      ()
  =
  let open Msgpack in
  let version =
    match version with
    | { major; minor; patch; prerelease; commit } ->
      List.filter_opt
        [ Option.map major ~f:(fun i -> String "major", Integer i)
        ; Option.map minor ~f:(fun i -> String "minor", Integer i)
        ; Option.map patch ~f:(fun i -> String "patch", Integer i)
        ; Option.map prerelease ~f:(fun s -> String "prerelease", String s)
        ; Option.map commit ~f:(fun s -> String "commit", String s)
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
  let convert_method { Types.Client_info.async; nargs; opts } =
    Map
      (((String "async", Boolean async)
        :: List.map (Option.to_list nargs) ~f:(function
          | `Fixed i -> String "nargs", Integer i
          | `Range (lo, hi) -> String "nargs", Array [ Integer lo; Integer hi ]))
       @ (Map.to_alist opts |> List.map ~f:(fun (a, b) -> String a, b)))
  in
  let methods =
    Map.map methods ~f:convert_method
    |> Map.to_alist
    |> List.map ~f:(fun (a, b) -> String a, b)
  in
  let attributes =
    Map.to_alist attributes |> List.map ~f:(fun (a, b) -> String a, String b)
  in
  Nvim_internal.Wrappers.nvim_set_client_info ~name ~version ~type_ ~methods ~attributes
  |> Api_call.of_api_result
;;

let get_current_win =
  Nvim_internal.Wrappers.nvim_get_current_win |> Api_call.of_api_result
;;

let set_current_win ~window =
  Nvim_internal.Wrappers.nvim_set_current_win ~window |> Api_call.of_api_result
;;

let list_wins =
  let open Api_call.Let_syntax in
  let%map result = Nvim_internal.Wrappers.nvim_list_wins |> Api_call.of_api_result in
  Or_error.bind
    ~f:(fun r -> List.map r ~f:Window.of_msgpack |> Or_error.combine_errors)
    result
;;

let replace_termcodes ~str ~from_part ~do_lt ~special =
  Nvim_internal.Wrappers.nvim_replace_termcodes ~str ~from_part ~do_lt ~special
  |> Api_call.of_api_result
;;

module Untested = struct
  let ui_attach ~width ~height ~options =
    Nvim_internal.Wrappers.nvim_ui_attach ~width ~height ~options
    |> Api_call.of_api_result
  ;;

  let ui_detach = Nvim_internal.Wrappers.nvim_ui_detach |> Api_call.of_api_result

  let ui_try_resize ~width ~height =
    Nvim_internal.Wrappers.nvim_ui_try_resize ~width ~height |> Api_call.of_api_result
  ;;

  let ui_set_option ~name ~value =
    Nvim_internal.Wrappers.nvim_ui_set_option ~name ~value |> Api_call.of_api_result
  ;;

  let get_hl_by_name ~name ~rgb =
    Nvim_internal.Wrappers.nvim_get_hl_by_name ~name ~rgb |> Api_call.of_api_result
  ;;

  let get_hl_by_id ~hl_id ~rgb =
    Nvim_internal.Wrappers.nvim_get_hl_by_id ~hl_id ~rgb |> Api_call.of_api_result
  ;;

  let input ~keys = Nvim_internal.Wrappers.nvim_input ~keys |> Api_call.of_api_result

  let execute_lua ~code ~args =
    Nvim_internal.Wrappers.nvim_execute_lua ~code ~args |> Api_call.of_api_result
  ;;

  let call_dict_function ~dict ~fn ~args =
    Nvim_internal.Wrappers.nvim_call_dict_function ~dict ~fn ~args
    |> Api_call.of_api_result
  ;;

  let strwidth ~text =
    Nvim_internal.Wrappers.nvim_strwidth ~text |> Api_call.of_api_result
  ;;

  let list_runtime_paths =
    let open Api_call.Let_syntax in
    let%map result =
      Nvim_internal.Wrappers.nvim_list_runtime_paths |> Api_call.of_api_result
    in
    let open Or_error.Let_syntax in
    let%bind result = result in
    List.map result ~f:Extract.string |> Or_error.combine_errors
  ;;

  let set_current_dir ~dir =
    Nvim_internal.Wrappers.nvim_set_current_dir ~dir |> Api_call.of_api_result
  ;;

  let get_current_line =
    Nvim_internal.Wrappers.nvim_get_current_line |> Api_call.of_api_result
  ;;

  let set_current_line ~line =
    Nvim_internal.Wrappers.nvim_set_current_line ~line |> Api_call.of_api_result
  ;;

  let del_current_line =
    Nvim_internal.Wrappers.nvim_del_current_line |> Api_call.of_api_result
  ;;

  let get_var ~name = Nvim_internal.Wrappers.nvim_get_var ~name |> Api_call.of_api_result

  let set_var ~name ~value =
    Nvim_internal.Wrappers.nvim_set_var ~name ~value |> Api_call.of_api_result
  ;;

  let del_var ~name = Nvim_internal.Wrappers.nvim_del_var ~name |> Api_call.of_api_result

  let get_vvar ~name =
    Nvim_internal.Wrappers.nvim_get_vvar ~name |> Api_call.of_api_result
  ;;

  let get_option ~name =
    Nvim_internal.Wrappers.nvim_get_option ~name |> Api_call.of_api_result
  ;;

  let set_option ~name ~value =
    Nvim_internal.Wrappers.nvim_set_option ~name ~value |> Api_call.of_api_result
  ;;

  let out_write ~str =
    Nvim_internal.Wrappers.nvim_out_write ~str |> Api_call.of_api_result
  ;;

  let err_write ~str =
    Nvim_internal.Wrappers.nvim_err_write ~str |> Api_call.of_api_result
  ;;

  let err_writeln ~str =
    Nvim_internal.Wrappers.nvim_err_writeln ~str |> Api_call.of_api_result
  ;;

  let list_tabpages =
    let open Api_call.Let_syntax in
    let%map result =
      Nvim_internal.Wrappers.nvim_list_tabpages |> Api_call.of_api_result
    in
    Or_error.bind
      ~f:(fun r -> List.map r ~f:Tabpage.of_msgpack |> Or_error.combine_errors)
      result
  ;;

  let get_current_tabpage =
    Nvim_internal.Wrappers.nvim_get_current_tabpage |> Api_call.of_api_result
  ;;

  let set_current_tabpage ~tabpage =
    Nvim_internal.Wrappers.nvim_set_current_tabpage ~tabpage |> Api_call.of_api_result
  ;;

  let subscribe ~event =
    Nvim_internal.Wrappers.nvim_subscribe ~event |> Api_call.of_api_result
  ;;

  let unsubscribe ~event =
    Nvim_internal.Wrappers.nvim_unsubscribe ~event |> Api_call.of_api_result
  ;;

  let get_color_by_name ~name =
    Nvim_internal.Wrappers.nvim_get_color_by_name ~name |> Api_call.of_api_result
  ;;

  let get_color_map = Nvim_internal.Wrappers.nvim_get_color_map |> Api_call.of_api_result
  let get_mode = Nvim_internal.Wrappers.nvim_get_mode |> Api_call.of_api_result

  let get_commands ~opts =
    let open Api_call.Let_syntax in
    let%map result =
      Nvim_internal.Wrappers.nvim_get_commands ~opts |> Api_call.of_api_result
    in
    let open Or_error.Let_syntax in
    let%bind result = result in
    let%bind commands_with_names =
      List.map result ~f:(fun (name, command) ->
        let open Or_error.Let_syntax in
        let%bind n = Extract.string name in
        let%bind c = Nvim_command.of_msgpack command in
        return (n, c))
      |> Or_error.combine_errors
    in
    String.Map.of_alist_or_error commands_with_names
  ;;

  let get_api_info = Nvim_internal.Wrappers.nvim_get_api_info |> Api_call.of_api_result

  let parse_expression ~expr ~flags ~highlight =
    Nvim_internal.Wrappers.nvim_parse_expression ~expr ~flags ~highlight
    |> Api_call.of_api_result
  ;;

  let list_uis = Nvim_internal.Wrappers.nvim_list_uis |> Api_call.of_api_result

  let get_proc_children ~pid =
    Nvim_internal.Wrappers.nvim_get_proc_children ~pid |> Api_call.of_api_result
  ;;

  let get_proc ~pid = Nvim_internal.Wrappers.nvim_get_proc ~pid |> Api_call.of_api_result
end
