open! Core
open! Async
open Vcaml
module Rpc_handler = Vcaml_plugin_intf.Rpc_handler
module Raise_on_any_error = Vcaml_plugin_intf.Raise_on_any_error

let get_realtime_client ~on_error ~on_error_event =
  let pipe = Sys.getenv_exn "NVIM_LISTEN_ADDRESS" in
  Client.attach
    (Unix pipe)
    ~on_error
    ~on_error_event
    ~time_source:(Time_source.wall_clock ())
;;

module Oneshot = struct
  module type Arg = Vcaml_plugin_intf.Oneshot_arg
  module type S = Vcaml_plugin_intf.Oneshot_s

  module Make (O : Arg) = struct
    let run_for_testing = O.execute

    let run () =
      let open Deferred.Or_error.Let_syntax in
      let%bind client =
        get_realtime_client ~on_error:O.on_error ~on_error_event:O.on_error_event
      in
      O.execute client
    ;;

    let command ~summary () =
      Async.Command.async_or_error ~summary (Core.Command.Param.return (fun () -> run ()))
    ;;
  end
end

module Persistent = struct
  module type Arg = Vcaml_plugin_intf.Persistent_arg
  module type S = Vcaml_plugin_intf.Persistent_s

  module Make (P : Arg) = struct
    type state = P.state

    let register_handler ~client ~state ~handler ~shutdown =
      match handler with
      | Rpc_handler.Sync_handler { name; type_; f } ->
        register_request_blocking client ~name ~type_ ~f:(f client state ~shutdown)
      | Async_handler { name; here; type_; f; on_error } ->
        Or_error.return
          (register_request_async
             ?on_error
             here
             client
             ~name
             ~type_
             ~f:(f client state ~shutdown))
    ;;

    let perform_handler_registration ~client ~state ~handlers ~shutdown =
      Deferred.return
        (handlers
         |> List.map ~f:(fun handler -> register_handler ~client ~state ~handler ~shutdown)
         |> Or_error.all_unit)
    ;;

    let notify_vim_of_plugin_start ~client ~chan_id ~vimscript_notify_fn =
      let open Deferred.Or_error.Let_syntax in
      match vimscript_notify_fn with
      | None -> return ()
      | Some function_name ->
        (match%bind
           wrap_viml_function
             ~type_:(Defun.Vim.unary Integer Integer)
             ~function_name
             chan_id
           |> run_join [%here] client
         with
         | 0 -> return ()
         | error_code ->
           Deferred.Or_error.errorf "[%s] returned %d" function_name error_code)
    ;;

    let run_internal ~client ~during_plugin =
      let open Deferred.Or_error.Let_syntax in
      let terminate_var = Ivar.create () in
      let shutdown () = Ivar.fill_if_empty terminate_var () in
      let chan_id = Client.rpc_channel_id client in
      let%bind state = P.startup client ~shutdown in
      let%bind () =
        perform_handler_registration ~client ~state ~handlers:P.rpc_handlers ~shutdown
      in
      let%bind () =
        notify_vim_of_plugin_start
          ~client
          ~chan_id
          ~vimscript_notify_fn:P.vimscript_notify_fn
      in
      let%bind () = during_plugin ~client ~chan_id ~state in
      let%bind () = Deferred.ok (Ivar.read terminate_var) in
      let%bind () = P.on_shutdown client state in
      return state
    ;;

    let ignore_during_plugin ~chan_id:_ ~state:_ = Deferred.Or_error.return ()

    let run () =
      let open Deferred.Or_error.Let_syntax in
      let%bind client =
        get_realtime_client ~on_error:P.on_error ~on_error_event:P.on_error_event
      in
      let%bind _state =
        run_internal ~client ~during_plugin:(fun ~client:_ -> ignore_during_plugin)
      in
      return ()
    ;;

    let run_for_testing ?(during_plugin = ignore_during_plugin) client =
      run_internal ~client ~during_plugin:(fun ~client:_ -> during_plugin)
    ;;

    let command ~summary () =
      Async.Command.async_or_error ~summary (Core.Command.Param.return (fun () -> run ()))
    ;;
  end
end
