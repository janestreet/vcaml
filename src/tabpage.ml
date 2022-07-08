open Core
include Nvim_internal.Tabpage

module Untested = struct
  let list_wins t =
    let open Api_call.Let_syntax in
    let%map result =
      Nvim_internal.nvim_tabpage_list_wins ~tabpage:t |> Api_call.of_api_result
    in
    Or_error.bind
      ~f:(fun r -> List.map r ~f:Window.of_msgpack |> Or_error.combine_errors)
      result
  ;;

  let get_var t ~name ~type_ =
    Nvim_internal.nvim_tabpage_get_var ~tabpage:t ~name
    |> Api_call.of_api_result
    |> Api_call.map_bind ~f:(Extract.value type_)
  ;;

  let set_var t ~name ~type_ ~value =
    let value = Extract.inject type_ value in
    Nvim_internal.nvim_tabpage_set_var ~tabpage:t ~name ~value |> Api_call.of_api_result
  ;;

  let delete_var t ~name =
    Nvim_internal.nvim_tabpage_del_var ~tabpage:t ~name |> Api_call.of_api_result
  ;;

  let get_win t = Nvim_internal.nvim_tabpage_get_win ~tabpage:t |> Api_call.of_api_result

  let get_number t =
    Nvim_internal.nvim_tabpage_get_number ~tabpage:t |> Api_call.of_api_result
  ;;

  let is_valid t =
    Nvim_internal.nvim_tabpage_is_valid ~tabpage:t |> Api_call.of_api_result
  ;;
end
