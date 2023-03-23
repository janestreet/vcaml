open Core
open Import
module Event = Nvim_internal.Ui_event

module Options = struct
  include Nvim_internal.Ui_options

  let default = { empty with ext_linegrid = true }
end

type t = [ `connected ] Client.t

module Description = struct
  type t =
    { channel : [ `Tui | `Id of int ]
    ; height : int
    ; width : int
    ; options : Options.t
    }
  [@@deriving sexp_of]
end

let attach here client ~width ~height ~options ~on_event ~on_parse_error =
  let events =
    let client = Type_equal.conv Client.Private.eq client in
    client.events
  in
  let on_parse_error =
    match on_parse_error with
    | `Call f -> f
    | `Ignore -> fun _ _ -> ()
    | `Raise ->
      fun error event ->
        raise_s
          [%message
            "Failed to parse buffer event"
              (error : Error.t)
              ~_:(event : Msgpack_rpc.Event.t)]
  in
  let cleared_screen = Set_once.create () in
  Bus.iter_exn events here ~f:(fun ({ method_name; params } as event) ->
    match method_name with
    | "redraw" ->
      List.iter params ~f:(fun param ->
        match Event.of_msgpack param with
        | Ok events ->
          List.iter events ~f:(fun event ->
            (match event with
             | Clear | Resize _ | Grid_resize { grid = 1; _ } | Grid_clear { grid = 1 } ->
               Set_once.set_if_none cleared_screen [%here] ()
             | _ -> ());
            if Set_once.is_some cleared_screen then on_event event)
        | Error error -> on_parse_error error event)
    | _ -> ());
  let options : (Msgpack.t * Msgpack.t) list =
    let use field =
      let name = Field.name field in
      let value = Field.get field options in
      Msgpack.String name, Msgpack.Boolean value
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
  in
  Nvim_internal.nvim_ui_attach ~width ~height ~options
  |> Api_call.of_api_result
  |> Api_call.run_join [%here] client
  |> Async.Deferred.map ~f:(tag_callsite here)
  |> Async.Deferred.Or_error.map ~f:(fun () -> client)
;;

let detach t here =
  Nvim_internal.nvim_ui_detach
  |> Api_call.of_api_result
  |> Api_call.run_join [%here] t
  |> Async.Deferred.map ~f:(tag_callsite here)
;;

let describe_attached_uis =
  let make_ui_options, () =
    let find_exn field () = (fun opts -> Map.find_exn opts (Field.name field)), () in
    Nvim_internal.Ui_options.Fields.make_creator
      ()
      ~ext_cmdline:find_exn
      ~ext_hlstate:find_exn
      ~ext_linegrid:find_exn
      ~ext_messages:find_exn
      ~ext_multigrid:find_exn
      ~ext_popupmenu:find_exn
      ~ext_tabline:find_exn
      ~ext_termcolors:find_exn
      ~ext_wildmenu:find_exn
      ~rgb:find_exn
  in
  Nvim_internal.nvim_list_uis
  |> Api_call.of_api_result
  |> Api_call.map_bind ~f:(fun uis ->
    try
      List.map uis ~f:(function
        | Map values ->
          let ints, opts =
            List.partition_map values ~f:(function
              | String key, Integer value -> First (key, value)
              | String key, Boolean value -> Second (key, value)
              | _ -> failwith "Parse failure")
          in
          let channel, width, height =
            let ints = String.Map.of_alist_exn ints in
            let channel =
              match Map.find ints "chan" with
              | None | Some 0 -> `Tui
              | Some i -> `Id i
            in
            let width = Map.find_exn ints "width" in
            let height = Map.find_exn ints "height" in
            channel, width, height
          in
          let options = make_ui_options (String.Map.of_alist_exn opts) in
          { Description.channel; height; width; options }
        | _ -> failwith "Parse failure")
      |> Or_error.return
    with
    | _ ->
      Or_error.error_s
        [%message "Unable to parse [list_uis] response" (uis : Msgpack.t list)])
;;

module Untested = struct
  let try_resizing_grid ~grid ~width ~height =
    Nvim_internal.nvim_ui_try_resize_grid ~grid ~width ~height |> Api_call.of_api_result
  ;;

  let popup_menu_set_height ~height =
    Nvim_internal.nvim_ui_pum_set_height ~height |> Api_call.of_api_result
  ;;

  let popup_menu_set_bounds ~width ~height ~row ~col =
    Nvim_internal.nvim_ui_pum_set_bounds ~width ~height ~row ~col
    |> Api_call.of_api_result
  ;;
end

(* These functions are part of the Neovim API but are not exposed in VCaml. *)
module _ = struct
  let (_ : _) = Nvim_internal.nvim_ui_set_option
  let (_ : _) = Nvim_internal.nvim_ui_try_resize
end
