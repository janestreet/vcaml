open Core
open Async
open Import
module Event = Nvim_internal.Ui_event

module Options = struct
  include Nvim_internal.Ui_options

  let default = { empty with ext_linegrid = true }
end

module Description = struct
  type t =
    { channel : [ `Tui | `Id of int ]
    ; height : int
    ; width : int
    ; options : Options.t
    }
  [@@deriving sexp_of]
end

let attach
  ~(here : [%call_pos])
  client
  ~width
  ~height
  ~options
  ~only_enable_options_supported_by_other_attached_uis
  =
  let open Deferred.Or_error.Let_syntax in
  let subscription_manager =
    (Type_equal.conv Client.Private.eq client).subscription_manager
  in
  let%bind reader = Subscription_manager.subscribe_to_ui_events subscription_manager in
  let options =
    let use field =
      let name = Field.name field in
      let value = Field.get field options in
      name, Msgpack.Bool value
    in
    Nvim_internal.Ui_options.Fields.to_list
      ~ext_cmdline:use
      ~ext_hlstate:use
      ~ext_linegrid:use
      ~ext_messages:use
      ~ext_multigrid:use
      ~ext_popupmenu:use
      ~ext_tabline:use
      ~ext_termcolors:use
      ~ext_wildmenu:use
      ~rgb:use
    |> String.Map.of_alist_exn
    |> Map.add_exn
         ~key:"override"
         ~data:(Bool (not only_enable_options_supported_by_other_attached_uis))
  in
  let%bind () =
    Nvim_internal.nvim_ui_attach ~width ~height ~options |> run ~here client
  in
  upon (Pipe.closed reader) (fun () ->
    don't_wait_for
      (Nvim_internal.nvim_ui_detach |> run ~here:[%here] client |> Deferred.ignore_m));
  return reader
;;

let describe_attached_uis ~(here : [%call_pos]) client =
  Nvim_internal.nvim_list_uis
  |> map_witness ~f:(fun uis ->
    uis
    |> List.map ~f:(fun msgpack ->
      let open Or_error.Let_syntax in
      let%bind map = Type.of_msgpack Dict msgpack in
      let%bind channel =
        match%map find_and_convert map "chan" (Type.of_msgpack Int) with
        | None | Some 0 -> `Tui
        | Some i -> `Id i
      in
      let%bind width = find_or_error_and_convert map "width" (Type.of_msgpack Int) in
      let%bind height = find_or_error_and_convert map "height" (Type.of_msgpack Int) in
      let find_bool_or_error map key =
        find_or_error_and_convert map key (Type.of_msgpack Bool)
      in
      let%bind ext_cmdline = find_bool_or_error map "ext_cmdline" in
      let%bind ext_hlstate = find_bool_or_error map "ext_hlstate" in
      let%bind ext_linegrid = find_bool_or_error map "ext_linegrid" in
      let%bind ext_messages = find_bool_or_error map "ext_messages" in
      let%bind ext_multigrid = find_bool_or_error map "ext_multigrid" in
      let%bind ext_popupmenu = find_bool_or_error map "ext_popupmenu" in
      let%bind ext_tabline = find_bool_or_error map "ext_tabline" in
      let%bind ext_termcolors = find_bool_or_error map "ext_termcolors" in
      let%bind ext_wildmenu = find_bool_or_error map "ext_wildmenu" in
      let%bind rgb = find_bool_or_error map "rgb" in
      return
        { Description.channel
        ; height
        ; width
        ; options =
            { ext_cmdline
            ; ext_hlstate
            ; ext_linegrid
            ; ext_messages
            ; ext_multigrid
            ; ext_popupmenu
            ; ext_tabline
            ; ext_termcolors
            ; ext_wildmenu
            ; rgb
            }
        })
    |> Or_error.combine_errors)
  |> run ~here client
;;

module Untested = struct
  let set_focus ~(here : [%call_pos]) client focus =
    let gained =
      match focus with
      | `Gained -> true
      | `Lost -> false
    in
    Nvim_internal.nvim_ui_set_focus ~gained |> run ~here client
  ;;

  let try_resizing_grid ~(here : [%call_pos]) client ~grid ~width ~height =
    Nvim_internal.nvim_ui_try_resize_grid ~grid ~width ~height |> run ~here client
  ;;

  let popup_menu_set_height ~(here : [%call_pos]) client ~height =
    Nvim_internal.nvim_ui_pum_set_height ~height |> run ~here client
  ;;

  let popup_menu_set_bounds ~(here : [%call_pos]) client ~width ~height ~row ~col =
    Nvim_internal.nvim_ui_pum_set_bounds ~width ~height ~row ~col |> run ~here client
  ;;
end

(* These functions are part of the Neovim API but are not exposed in VCaml. *)
module _ = struct
  let (_ : _) = Nvim_internal.nvim_ui_set_option
  let (_ : _) = Nvim_internal.nvim_ui_try_resize
end
