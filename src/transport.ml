open Core
open Async
open Msgpack

module Make (U : Msgpack_rpc.S) = struct
  let bootstrap_nvim_api_fn ~conn { Nvim_internal.Types.name; params; _ } =
    U.call conn ~method_name:name ~parameters:params
  ;;

  (* Makes sure that the function that is being called is present in the map
     returned to us by neovim, and that it isn't deprecated. *)
  let call_nvim_api_fn ~conn ~fns_map f =
    let { Nvim_internal.Types.name; witness; _ } = f in
    match Map.find fns_map name with
    | None -> Or_error.errorf "function does not exist: %s" name |> Deferred.return
    | Some data ->
      let open Deferred.Or_error.Let_syntax in
      let result = bootstrap_nvim_api_fn ~conn f in
      (match Map.find data "deprecated_since" with
       | None -> ()
       | Some _ ->
         let msg = sprintf "echom \"Use of deprecated function: %s\"" name in
         let witness = Nvim_internal.Types.Phantom.Nil in
         bootstrap_nvim_api_fn
           ~conn
           { name = "nvim_command"; params = Array [ String msg ]; witness }
         |> Deferred.ignore_m
         |> don't_wait_for);
      let%bind r = Deferred.map result ~f:Extract.convert_msgpack_error in
      Deferred.return (Extract.value witness r)
  ;;

  (* The event subscription is just a bus that distributes all events
     that go through the client. *)
  let create_event_subscription ~conn =
    let result =
      Bus.create
        [%here]
        Bus.Callback_arity.Arity1
        ~on_subscription_after_first_write:Allow_and_send_last_value
        ~on_callback_raise:Error.raise
    in
    U.subscribe conn [%here]
    |> Pipe.iter_without_pushback ~f:(Bus.write result)
    |> don't_wait_for;
    Bus.read_only result
  ;;

  let function_name_to_info info =
    let open Or_error.Let_syntax in
    let%bind name = Map.find_or_error info "name" in
    let%bind name = Extract.string name in
    return (name, info)
  ;;

  (* Dynamically pull the function information from neovim and
     so that call_nvim_api_fn can use it to validate calls. *)
  let extract_function_info api_data =
    let open Or_error.Let_syntax in
    let%bind _channel, api =
      match%bind Extract.convert_msgpack_error api_data with
      | Array [ _channel; api ] -> Ok (_channel, api)
      | other ->
        Or_error.error_s
          [%message "[get_api_info] returned unexpected input" (other : Msgpack.t)]
    in
    let%bind data = Extract.map_of_msgpack_map api in
    let%bind fn_objs =
      match%bind Map.find_or_error data "functions" with
      | Array objs -> Ok objs
      | other ->
        Or_error.error_s
          [%message "[get_api_info] returned unexpected_input" (other : Msgpack.t)]
    in
    let%bind fn_maps =
      List.map fn_objs ~f:Extract.map_of_msgpack_map |> Or_error.combine_errors
    in
    let%bind name_to_info =
      List.map fn_maps ~f:function_name_to_info |> Or_error.combine_errors
    in
    String.Map.of_alist_or_error name_to_info
  ;;

  let attach (conn : U.t) =
    let%bind.Deferred api_data =
      Nvim_internal.Wrappers.nvim_get_api_info |> bootstrap_nvim_api_fn ~conn
    in
    let result =
      let open Or_error.Let_syntax in
      let events = create_event_subscription ~conn in
      let register_request ~name ~f = U.register_method ~name ~f in
      let%map fns_map = extract_function_info api_data in
      { Types.events
      ; call_nvim_api_fn = (fun f -> call_nvim_api_fn ~conn ~fns_map f)
      ; register_request
      ; buffers_attached = Buf.Table.create ()
      ; attach_sequencer = Throttle.Sequencer.create ~continue_on_error:false ()
      }
    in
    return result
  ;;
end
