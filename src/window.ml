open Core

type t = Types.Window.t [@@deriving sexp]

type position =
  { row : int
  ; col : int
  }

let to_msgpack = Types.Window.to_msgpack
let of_msgpack = Types.Window.of_msgpack

let get_height ~window =
  Nvim_internal.Wrappers.nvim_win_get_height ~window |> Api_call.of_api_result
;;

let set_height ~window ~height =
  Nvim_internal.Wrappers.nvim_win_set_height ~window ~height |> Api_call.of_api_result
;;

module Untested = struct
  let get_buf ~window =
    Nvim_internal.Wrappers.nvim_win_get_buf ~window |> Api_call.of_api_result
  ;;

  let get_cursor ~window =
    let open Api_call.Let_syntax in
    let%map cursor =
      Nvim_internal.Wrappers.nvim_win_get_cursor ~window |> Api_call.of_api_result
    in
    let open Or_error.Let_syntax in
    match%bind cursor with
    | [ Msgpack.Integer row; Integer col ] -> Ok { row; col }
    | _ -> Or_error.error_string "malformed result from [nvim_win_get_cursor]"
  ;;

  let set_cursor ~window ~row ~col =
    let pos = [ Msgpack.Integer row; Integer col ] in
    Nvim_internal.Wrappers.nvim_win_set_cursor ~window ~pos |> Api_call.of_api_result
  ;;

  let get_width ~window =
    Nvim_internal.Wrappers.nvim_win_get_width ~window |> Api_call.of_api_result
  ;;

  let set_width ~window ~width =
    Nvim_internal.Wrappers.nvim_win_set_width ~window ~width |> Api_call.of_api_result
  ;;

  let get_var ~window ~name =
    Nvim_internal.Wrappers.nvim_win_get_var ~window ~name |> Api_call.of_api_result
  ;;

  let set_var ~window ~name ~value =
    Nvim_internal.Wrappers.nvim_win_set_var ~window ~name ~value
    |> Api_call.of_api_result
  ;;

  let del_var ~window ~name =
    Nvim_internal.Wrappers.nvim_win_del_var ~window ~name |> Api_call.of_api_result
  ;;

  let get_option ~window ~name =
    Nvim_internal.Wrappers.nvim_win_get_option ~window ~name |> Api_call.of_api_result
  ;;

  let set_option ~window ~name ~value =
    Nvim_internal.Wrappers.nvim_win_set_option ~window ~name ~value
    |> Api_call.of_api_result
  ;;

  let get_position ~window =
    let open Api_call.Let_syntax in
    let%map position =
      Nvim_internal.Wrappers.nvim_win_get_position ~window |> Api_call.of_api_result
    in
    let open Or_error.Let_syntax in
    match%bind position with
    | [ Msgpack.Integer row; Integer col ] -> Ok { row; col }
    | _ -> Or_error.error_string "malformed result from [nvim_win_get_position]"
  ;;

  let get_tabpage ~window =
    Nvim_internal.Wrappers.nvim_win_get_tabpage ~window |> Api_call.of_api_result
  ;;

  let get_number ~window =
    Nvim_internal.Wrappers.nvim_win_get_number ~window |> Api_call.of_api_result
  ;;

  let is_valid ~window =
    Nvim_internal.Wrappers.nvim_win_is_valid ~window |> Api_call.of_api_result
  ;;
end
