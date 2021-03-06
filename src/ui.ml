open Core
module Event = Nvim_internal.Ui_event
module Options = Nvim_internal.Ui_options

type t = Client.t

module Description = struct
  type t =
    { channel_id : [ `Tui | `Id of int ]
    ; height : int
    ; width : int
    ; options : Options.t
    }
  [@@deriving sexp_of]
end

let attach ?on_error (client : Client.t) ~width ~height ~options ~on_event =
  let T = Client.Private.eq in
  let on_error = Option.value on_error ~default:client.on_error in
  let cleared_screen = Set_once.create () in
  Bus.iter_exn client.events [%here] ~f:(fun ({ method_name; params } as event) ->
    let T = Client.Private.eq in
    match method_name with
    | "redraw" ->
      List.iter params ~f:(fun param ->
        match Event.of_msgpack param with
        | Ok events ->
          List.iter events ~f:(fun event ->
            (match event with
             | Clear | Resize _ -> Set_once.set_if_none cleared_screen [%here] ()
             | _ -> ());
            if Set_once.is_some cleared_screen then on_event event)
        | Error error ->
          on_error (Error.tag_s error ~tag:[%sexp (event : Msgpack_rpc.event)]))
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
      ~ext_popupmenu:use
      ~ext_tabline:use
      ~ext_wildmenu:use
      ~rgb:use
  in
  Nvim_internal.nvim_ui_attach ~width ~height ~options
  |> Api_call.of_api_result
  |> Api_call.run_join client
  |> Async.Deferred.Or_error.map ~f:(fun () -> client)
;;

let detach t =
  Nvim_internal.nvim_ui_detach |> Api_call.of_api_result |> Api_call.run_join t
;;

let describe_attached_uis =
  let make_ui_options, () =
    let find_exn field () = (fun opts -> Map.find_exn opts (Field.name field)), () in
    Nvim_internal.Ui_options.Fields.make_creator
      ()
      ~ext_cmdline:find_exn
      ~ext_hlstate:find_exn
      ~ext_linegrid:find_exn
      ~ext_popupmenu:find_exn
      ~ext_tabline:find_exn
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
          let channel_id, width, height =
            let ints = String.Map.of_alist_exn ints in
            let channel_id =
              match Map.find_exn ints "chan" with
              | 0 -> `Tui
              | i -> `Id i
            in
            let width = Map.find_exn ints "width" in
            let height = Map.find_exn ints "height" in
            channel_id, width, height
          in
          let options = make_ui_options (String.Map.of_alist_exn opts) in
          { Description.channel_id; height; width; options }
        | _ -> failwith "Parse failure")
      |> Or_error.return
    with
    | _ ->
      Or_error.error_s
        [%message "Unable to parse [list_uis] response" (uis : Msgpack.t list)])
;;
