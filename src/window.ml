open Core
include Nvim_internal.Window

let get_height ~window =
  Nvim_internal.nvim_win_get_height ~window |> Api_call.of_api_result
;;

let set_height ~window ~height =
  Nvim_internal.nvim_win_set_height ~window ~height |> Api_call.of_api_result
;;

let get_cursor ~window =
  let open Api_call.Let_syntax in
  let%map cursor = Nvim_internal.nvim_win_get_cursor ~window |> Api_call.of_api_result in
  let open Or_error.Let_syntax in
  match%bind cursor with
  | [ Msgpack.Integer row; Integer col ] -> Ok { Position.One_indexed_row.row; col }
  | _ -> Or_error.error_string "malformed result from [nvim_win_get_cursor]"
;;

let set_cursor ~window { Position.One_indexed_row.row; col } =
  let pos = [ Msgpack.Integer row; Integer col ] in
  Nvim_internal.nvim_win_set_cursor ~window ~pos |> Api_call.of_api_result
;;

module Untested = struct
  let get_buf ~window = Nvim_internal.nvim_win_get_buf ~window |> Api_call.of_api_result

  let get_width ~window =
    Nvim_internal.nvim_win_get_width ~window |> Api_call.of_api_result
  ;;

  let set_width ~window ~width =
    Nvim_internal.nvim_win_set_width ~window ~width |> Api_call.of_api_result
  ;;

  let get_var ~window ~name ~type_ =
    Nvim_internal.nvim_win_get_var ~window ~name
    |> Api_call.of_api_result
    |> Api_call.map_bind ~f:(Extract.value type_)
  ;;

  let set_var ~window ~name ~type_ ~value =
    let value = Extract.inject type_ value in
    Nvim_internal.nvim_win_set_var ~window ~name ~value |> Api_call.of_api_result
  ;;

  let del_var ~window ~name =
    Nvim_internal.nvim_win_del_var ~window ~name |> Api_call.of_api_result
  ;;

  let get_option ~window ~name ~type_ =
    Nvim_internal.nvim_win_get_option ~window ~name
    |> Api_call.of_api_result
    |> Api_call.map_bind ~f:(Extract.value type_)
  ;;

  let set_option ~window ~scope ~name ~type_ ~value =
    let vcaml_tmp = "__vcaml_tmp" in
    let value = Extract.inject type_ value in
    let command command = Nvim_internal.nvim_command ~command in
    let set_option = Nvim_internal.nvim_win_set_option ~window ~name ~value in
    let api_results =
      match scope with
      | `Global -> [ set_option ]
      | `Local ->
        [ command [%string "let %{vcaml_tmp} = &g:%{name}"]
        ; set_option
        ; command [%string "let &g:%{name} = %{vcaml_tmp}"]
        ; command [%string "unlet %{vcaml_tmp}"]
        ]
    in
    api_results |> List.map ~f:Api_call.of_api_result |> Api_call.Or_error.all_unit
  ;;

  let get_position ~window =
    let open Api_call.Let_syntax in
    let%map position =
      Nvim_internal.nvim_win_get_position ~window |> Api_call.of_api_result
    in
    let open Or_error.Let_syntax in
    match%bind position with
    | [ Msgpack.Integer row; Integer col ] -> Ok { Position.row; col }
    | _ -> Or_error.error_string "malformed result from [nvim_win_get_position]"
  ;;

  let get_tabpage ~window =
    Nvim_internal.nvim_win_get_tabpage ~window |> Api_call.of_api_result
  ;;

  let get_number ~window =
    Nvim_internal.nvim_win_get_number ~window |> Api_call.of_api_result
  ;;

  let is_valid ~window = Nvim_internal.nvim_win_is_valid ~window |> Api_call.of_api_result

  let get_config ~window =
    Nvim_internal.nvim_win_get_config ~window
    |> Api_call.of_api_result
    |> Api_call.map_bind ~f:Extract.map_of_msgpack_alist
  ;;

  let set_config ~window ~config =
    let config = Extract.map_to_msgpack_alist config in
    Nvim_internal.nvim_win_set_config ~window ~config |> Api_call.of_api_result
  ;;

  let open_ ~buffer ~enter ~config =
    let config = Extract.map_to_msgpack_alist config in
    Nvim_internal.nvim_open_win ~buffer ~enter ~config |> Api_call.of_api_result
  ;;

  module When_this_is_the_buffer's_last_window = struct
    type t =
      | Hide
      | Unload of { if_modified : [ `Hide | `Abort ] }
  end

  let close ~window ~when_this_is_the_buffer's_last_window =
    (match when_this_is_the_buffer's_last_window with
     | When_this_is_the_buffer's_last_window.Hide -> Nvim_internal.nvim_win_hide ~window
     | Unload { if_modified } ->
       let force =
         match if_modified with
         | `Hide -> true
         | `Abort -> false
       in
       Nvim_internal.nvim_win_close ~window ~force)
    |> Api_call.of_api_result
  ;;

  module Expert = struct
    let set_buf ~window ~buffer =
      Nvim_internal.nvim_win_set_buf ~window ~buffer |> Api_call.of_api_result
    ;;

    let win_call ~window ~lua_callback =
      Nvim_internal.nvim_win_call ~window ~fun_:lua_callback |> Api_call.of_api_result
    ;;
  end
end
