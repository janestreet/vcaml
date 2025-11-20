open Core
open Async
open Vcaml
module Private = Vcaml_test_helpers.Private

(* This is analogous to the [behave_nicely_in_pipeline] argument passed to Async commands
   but does not shutdown with SIGPIPE when the writer is closed. The consumer-leaving
   scenario is [jobstart] defining a handler for the channel and then the channel being
   closed with [chanclose]. Although this is inadvisable, we don't need to treat it as
   exceptional, particularly given that Neovim will terminate the plugin during shutdown.
   Also note that if no handler is defined for the channel libuv will redirect it to
   /dev/null, which will not lead to a consumer-leaving scenario. *)
let behave_nicely_when_called_from_nvim ~stdout_is_used_for_msgpack =
  let behave_nicely writer =
    Writer.set_buffer_age_limit writer `Unlimited;
    Writer.set_raise_when_consumer_leaves writer false
  in
  behave_nicely (force Writer.stderr);
  if not stdout_is_used_for_msgpack then behave_nicely (force Writer.stdout)
;;

let run_with_crash_handler ~on_crash ~f =
  (* Ignore subsequent errors because we want to allow [on_crash] to finish its logic
     uninterrupted before raising the original error. *)
  match%bind Monitor.try_with_or_error ~rest:(`Call ignore) f with
  | Ok () -> return ()
  | Error error ->
    let%map () =
      (* Ignore errors that happen during [on_crash]. *)
      Monitor.try_with ~rest:(`Call ignore) (fun () -> on_crash error)
      |> Deferred.ignore_m
    in
    Error.raise error
;;

module Oneshot = struct
  module Rpc = struct
    type t =
      | Blocking_rpc :
          { here : Source_code_position.t
          ; name : string
          ; type_ : 'fn Ocaml_from_nvim.Blocking.t
          ; f : client:[ `blocking ] Client.t -> 'fn
          }
          -> t

    let create ~(here : [%call_pos]) name ~type_ ~f =
      Blocking_rpc { here; name; type_; f }
    ;;
  end

  let create ?on_crash ~name ~description rpcs =
    let f () =
      behave_nicely_when_called_from_nvim ~stdout_is_used_for_msgpack:true;
      let client = Client.create ~name ~on_error:`Raise in
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
          Set_once.set_exn invoked_rpc name;
          f ()
      in
      List.iter rpcs ~f:(fun (Rpc.Blocking_rpc { here; name; type_; f }) ->
        Ocaml_from_nvim.Private.register_request_blocking
          ~here
          (Not_connected client)
          ~name
          ~type_
          ~f:(fun () ~run_in_background ->
            (* If we simply invoked [f] here, filled [shutdown] on its completion, and
               then returned, the invocation of [shutdown] would race with returning the
               result of this callback to Neovim. We avoid this race by only filling
               [shutdown] inside [run_in_background] after sending a dummy request, since
               we know that that dummy request can only succeed once the client passed to
               [run_in_background] receives permission to run, which will only happen
               after we've sent the callback's response to Neovim and expired the blocking
               client's permission to run. While technically after that point Neovim could
               make another blocking request, that would be an improper use of a oneshot
               plugin, and guarding against that is difficult and not worthwhile. *)
            run_in_background ~here (fun client ->
              let%map.Deferred.Or_error (_ : Buffer.t) =
                (* This will only succeed once the result of [f] has successfully been
                   sent to Neovim. *)
                Nvim.get_current_buf client
              in
              Ivar.fill_exn shutdown ());
            f)
          ~wrap_f:(oneshot ~name));
      let%bind (_ : [ `asynchronous ] Client.t) =
        Private.attach_client client Stdio >>| ok_exn
      in
      Ivar.read shutdown
    in
    Async.Command.async
      ~behave_nicely_in_pipeline:false
      ~summary:description
      (Async.Command.Param.return (fun () ->
         match on_crash with
         | None -> f ()
         | Some on_crash -> run_with_crash_handler ~on_crash ~f))
  ;;
end

module Persistent = struct
  module Rpc = struct
    type 'state t =
      | Blocking_rpc :
          { here : Source_code_position.t
          ; name : string
          ; type_ : 'fn Ocaml_from_nvim.Blocking.t
          ; f :
              'state
              -> run_in_background:
                   (here:[%call_pos]
                    -> ([ `asynchronous ] Client.t -> unit Deferred.Or_error.t)
                    -> unit)
              -> client:[ `blocking ] Client.t
              -> 'fn
          ; on_keyboard_interrupt : (unit -> unit) option
          }
          -> 'state t
      | Async_rpc :
          { here : Source_code_position.t
          ; name : string
          ; type_ : 'fn Ocaml_from_nvim.Async.t
          ; f : 'state -> client:[ `asynchronous ] Client.t -> 'fn
          }
          -> 'state t

    let create_blocking ?on_keyboard_interrupt ~(here : [%call_pos]) name ~type_ ~f =
      Blocking_rpc { here; name; type_; f; on_keyboard_interrupt }
    ;;

    let create_async ~(here : [%call_pos]) name ~type_ ~f =
      Async_rpc { here; name; type_; f }
    ;;

    let contra_map t ~f =
      match t with
      | Blocking_rpc { here; name; type_; f = f'; on_keyboard_interrupt } ->
        Blocking_rpc { here; name; type_; f = Fn.compose f' f; on_keyboard_interrupt }
      | Async_rpc { here; name; type_; f = f' } ->
        Async_rpc { here; name; type_; f = Fn.compose f' f }
    ;;
  end

  let register_handlers ~client ~state rpcs =
    let get_state state ~name =
      match Set_once.get state with
      | Some state -> state
      | None ->
        (* This error indicates a bug in the plugin's initialization logic. Under normal
           circumstances, Neovim won't learn about the plugin's channel until [notify_fn]
           is invoked, which happens after [on_startup] returns, but it is possible that
           [on_startup] gave Neovim a way to invoke the plugin's RPCs, e.g., by
           registering autocommands. Any logic that does this must be moved into
           [after_startup] so that the state is guaranteed to be initialized before RPCs
           are invoked. *)
        raise_s [%message "Called RPC before plugin finished initializing." name]
    in
    let client = Client.Maybe_connected.Not_connected client in
    List.iter rpcs ~f:(function
      | Rpc.Blocking_rpc { here; name; type_; f; on_keyboard_interrupt } ->
        Ocaml_from_nvim.Private.register_request_blocking
          ?on_keyboard_interrupt
          ~here
          client
          ~name
          ~type_
          ~f
          ~wrap_f:(fun f -> f (get_state state ~name))
      | Async_rpc { here; name; type_; f } ->
        Ocaml_from_nvim.Private.register_request_async
          ~here
          client
          ~name
          ~type_
          ~f
          ~wrap_f:(fun f -> f (get_state state ~name)))
  ;;

  let create'
    ?on_crash
    ?after_startup
    ~name
    ~description
    ~param
    ~on_startup
    ~notify_fn
    rpcs
    =
    let f ~param =
      behave_nicely_when_called_from_nvim ~stdout_is_used_for_msgpack:false;
      let state = Set_once.create () in
      let display_error_in_neovim' =
        ref (fun ~(here : [%call_pos]) _ ->
          let _ : Source_code_position.t = here in
          ())
      in
      let on_error =
        (* Track whether we already sent notifications due to parse failures of UI and
           buffer events. These events are streamed from Neovim, and as it's likely that
           if we failed to parse one we will fail to parse many, we don't want to spam the
           user with failure messages. *)
        let failed_to_parse_ui_event = ref false in
        let failed_to_parse_buffer_event = ref false in
        fun (error : Vcaml_error.t) ->
          match error with
          | Msgpack_rpc_error error -> error |> Msgpack_rpc.Error.to_error |> Error.raise
          | Nvim_error_event error_event ->
            !display_error_in_neovim' (Vcaml_error.Nvim_error_event.to_error error_event)
          | Nvim_error_event_parse_failure notification ->
            !display_error_in_neovim'
              (Error.create_s
                 [%message
                   "Failed to parse error event"
                     ~_:(notification : Vcaml_error.Notification.t)])
          | Nvim_buffer_event_parse_failure (exn, notification) ->
            (match !failed_to_parse_buffer_event with
             | true -> ()
             | false ->
               failed_to_parse_buffer_event := true;
               !display_error_in_neovim'
                 (Error.create_s
                    [%message
                      "Failed to parse buffer event(s) - only reporting first failure."
                        ~error:(exn : exn)
                        (notification : Vcaml_error.Notification.t)]))
          | Nvim_ui_event_parse_failure (exn, notification) ->
            (match !failed_to_parse_ui_event with
             | true -> ()
             | false ->
               failed_to_parse_ui_event := true;
               !display_error_in_neovim'
                 (Error.create_s
                    [%message
                      "Failed to parse UI event(s) - only reporting first failure."
                        ~error:(exn : exn)
                        (notification : Vcaml_error.Notification.t)]))
      in
      let client = Client.create ~name ~on_error:(`Call on_error) in
      let rpc_registration_failure = ref None in
      (try register_handlers ~client ~state rpcs with
       | exn -> rpc_registration_failure := Some exn);
      let%bind client =
        Private.attach_client client (Socket `Infer_from_parent_nvim) >>| ok_exn
      in
      (display_error_in_neovim'
       := fun ~(here : [%call_pos]) error ->
            Private.notify_nvim_of_error client ~here error |> don't_wait_for);
      let%bind () =
        match !rpc_registration_failure with
        | None -> return ()
        | Some exn ->
          let%bind () = Private.notify_nvim_of_error client (Error.of_exn exn) in
          raise exn
      in
      match%bind Monitor.try_with_join_or_error (fun () -> on_startup param ~client) with
      | Error error ->
        let%bind () = Private.notify_nvim_of_error client error in
        Error.raise error
      | Ok state' ->
        (* It's important that we set [state] before notifying Neovim that the plugin is
           ready, since at that point Neovim has the green light to call RPCs. *)
        Set_once.set_exn state state';
        (match%bind
           match after_startup with
           | None -> Deferred.Or_error.return ()
           | Some f -> Monitor.try_with_join_or_error (fun () -> f state' ~client)
         with
         | Error error ->
           let%bind () = Private.notify_nvim_of_error client error in
           Error.raise error
         | Ok () ->
           let%bind () =
             match%bind
               Nvim.call_function
                 client
                 ~name:notify_fn
                 ~type_:Nvim.Func.(Int @-> return Object)
                 (Client.channel client)
             with
             | Error error ->
               let%bind () = Private.notify_nvim_of_error client error in
               Error.raise error
             | Ok value ->
               (match value, notify_fn with
                | Int 0, `Viml _ | Nil, `Lua _ -> return ()
                | _, (`Viml function_name | `Lua function_name) ->
                  let error =
                    Error.create_s
                      [%message
                        (sprintf "%s returned a value" function_name)
                          ~_:(value : Msgpack.t)]
                  in
                  let%bind () = Private.notify_nvim_of_error client error in
                  Error.raise error)
           in
           Deferred.never ())
    in
    Async.Command.async
      ~behave_nicely_in_pipeline:false
      ~summary:description
      (let%map_open.Async.Command param in
       fun () ->
         match on_crash with
         | None -> f ~param
         | Some on_crash -> run_with_crash_handler ~on_crash ~f:(fun () -> f ~param))
  ;;

  let create ?on_crash ?after_startup ~name ~description ~on_startup ~notify_fn rpcs =
    create'
      ?on_crash
      ?after_startup
      ~name
      ~description
      ~param:(Async.Command.Param.return ())
      ~on_startup:(fun () ~client -> on_startup client)
      ~notify_fn
      rpcs
  ;;
end
