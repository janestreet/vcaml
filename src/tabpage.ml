open Core
include Nvim_internal.Tabpage

module Untested = struct
  let list_wins ~tabpage =
    let open Api_call.Let_syntax in
    let%map result =
      Nvim_internal.nvim_tabpage_list_wins ~tabpage |> Api_call.of_api_result
    in
    Or_error.bind
      ~f:(fun r -> List.map r ~f:Window.of_msgpack |> Or_error.combine_errors)
      result
  ;;

  let get_var ~tabpage ~name ~type_ =
    Nvim_internal.nvim_tabpage_get_var ~tabpage ~name
    |> Api_call.of_api_result
    |> Api_call.map_bind ~f:(Extract.value type_)
  ;;

  let set_var ~tabpage ~name ~type_ ~value =
    let value = Extract.inject type_ value in
    Nvim_internal.nvim_tabpage_set_var ~tabpage ~name ~value |> Api_call.of_api_result
  ;;

  let del_var ~tabpage ~name =
    Nvim_internal.nvim_tabpage_del_var ~tabpage ~name |> Api_call.of_api_result
  ;;

  let get_win ~tabpage =
    Nvim_internal.nvim_tabpage_get_win ~tabpage |> Api_call.of_api_result
  ;;

  let get_number ~tabpage =
    Nvim_internal.nvim_tabpage_get_number ~tabpage |> Api_call.of_api_result
  ;;

  let is_valid ~tabpage =
    Nvim_internal.nvim_tabpage_is_valid ~tabpage |> Api_call.of_api_result
  ;;
end
