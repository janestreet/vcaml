open! Core
open! Async
open Vcaml
open Vcaml_plugin_intf

module Oneshot = struct
  include Oneshot

  module Make (O : Arg) = struct
    let command ~summary =
      Async.Command.async_or_error
        ~summary
        (Core.Command.Param.return (fun () ->
           let open Deferred.Or_error.Let_syntax in
           let client = Client.create ~on_error:O.on_error in
           let shutdown = Ivar.create () in
           let invoked_rpc = Set_once.create () in
           let oneshot ~name f =
             match Set_once.get invoked_rpc with
             | Some previously_invoked ->
               Deferred.Or_error.error_s
                 [%message
                   "Already invoked an RPC"
                     ~invoking:(name : string)
                     (previously_invoked : string)]
             | None ->
               Set_once.set_exn invoked_rpc [%here] name;
               let%map result = f () in
               Ivar.fill shutdown ();
               result
           in
           List.iter O.rpc_handlers ~f:(fun (Sync_rpc { name; type_; f }) ->
             Private.register_request_blocking
               client
               ~name
               ~type_
               ~f
               ~wrap_f:(oneshot ~name));
           let%bind (_ : [ `connected ] Client.t) =
             Client.attach client Stdio ~time_source:(Time_source.wall_clock ())
           in
           Ivar.read shutdown |> Deferred.ok))
    ;;
  end
end

module Persistent = struct
  include Persistent

  module Make (P : Arg) = struct
    let register_handlers ~client ~state ~shutdown =
      let shutdown = Ivar.fill_if_empty shutdown in
      List.iter P.rpc_handlers ~f:(function
        | Sync_rpc { name; type_; f } ->
          register_request_blocking client ~name ~type_ ~f:(f state ~shutdown)
        | Async_rpc { name; type_; f } ->
          Private.register_request_async
            client
            ~name
            ~type_
            ~f:(f state ~shutdown)
            ~wrap_f:(fun f -> f () |> Deferred.Or_error.tag ~tag:P.name))
    ;;

    let display_error_in_neovim ~client error =
      error
      |> Error.tag ~tag:P.name
      |> Error.to_string_hum
      |> (fun str -> Nvim.err_writeln ~str)
      |> run_join [%here] client
      (* We can't really do anything interesting with a failure to display an error. *)
      |> (Deferred.ignore_m : unit Deferred.Or_error.t -> unit Deferred.t)
    ;;

    let start ~client ~state ~shutdown =
      let%bind result =
        let open Deferred.Or_error.Let_syntax in
        let shutdown = Ivar.fill_if_empty shutdown in
        let chan_id = Client.rpc_channel_id client in
        let%bind () = P.on_startup client state ~shutdown in
        match P.vimscript_notify_fn with
        | None -> return ()
        | Some function_name ->
          (match%bind
             wrap_viml_function
               ~type_:Defun.Vim.(Integer @-> return Object)
               ~function_name
               chan_id
             |> run_join [%here] client
           with
           | Integer 0 -> return ()
           | value ->
             Deferred.Or_error.error_s
               [%message
                 (sprintf "%s returned a value" function_name) ~_:(value : Msgpack.t)])
      in
      let%bind () =
        match result with
        | Ok _ -> return ()
        | Error error -> display_error_in_neovim ~client error
      in
      return result
    ;;

    let on_shutdown client state =
      let%bind result = P.on_shutdown client state in
      let%bind () =
        match result with
        | Ok _ -> return ()
        | Error error -> display_error_in_neovim ~client error
      in
      return result
    ;;

    let command =
      Async.Command.async_or_error
        ~summary:P.description
        (Core.Command.Param.return (fun () ->
           let open Deferred.Or_error.Let_syntax in
           let state = P.init_state () in
           let shutdown = Ivar.create () in
           let client = Vcaml.Client.create ~on_error:P.on_error in
           register_handlers ~client ~state ~shutdown;
           let%bind client =
             Vcaml.Client.attach
               client
               (Unix `Child)
               ~time_source:(Time_source.wall_clock ())
           in
           let%bind () = start ~client ~state ~shutdown in
           let%bind () = Ivar.read shutdown |> Deferred.ok in
           on_shutdown client state))
    ;;

    module For_testing = struct
      type plugin_state = P.state [@@deriving sexp_of]

      module State = struct
        type t =
          { plugin_state : plugin_state
          ; shutdown : unit -> unit
          ; wait_for_shutdown : unit Or_error.t Deferred.t
          }
      end

      let start ~client =
        let open Deferred.Or_error.Let_syntax in
        let state = P.init_state () in
        let shutdown_started = Ivar.create () in
        let shutdown_finished = Ivar.create () in
        don't_wait_for
          (let%bind.Deferred () = Ivar.read shutdown_started in
           let%map.Deferred result = on_shutdown client state in
           Ivar.fill shutdown_finished result);
        register_handlers ~client ~state ~shutdown:shutdown_started;
        let%bind () = start ~client ~state ~shutdown:shutdown_started in
        return
          { State.plugin_state = state
          ; shutdown = Ivar.fill shutdown_started
          ; wait_for_shutdown = Ivar.read shutdown_finished
          }
      ;;
    end
  end
end
