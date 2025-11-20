module For_testing = struct
  module Do_not_move = struct
    let line3 = [%here]
    let line4 = [%here]
    let line5 = [%here]
    let line6 = [%here]
  end
end

let transparent_pos = [%here]

open Core
open Async
open Import0

let nvim_error_event = "nvim_error_event"
let unregister_blocking_rpc = "unregister_blocking_rpc"
let before_sending_response_hook_for_tests = ref None

module Helpers = struct
  module Error_pattern_helpers = struct
    let tag_callsite here error =
      match Source_code_position.equal here transparent_pos with
      | true -> error
      | false ->
        let here = [%sexp (here : Source_code_position.t)] in
        (match [%sexp (error : Error.t)] with
         | List [ message; List (List [ Atom "Called from"; Atom _ ] :: _ as trace) ] ->
           Sexp.List [ message; List (trace @ [ List [ Atom "Called from"; here ] ]) ]
         | sexp -> Sexp.List [ sexp; List [ List [ Atom "Called from"; here ] ] ])
        |> Error.create_s
    ;;

    let vim_error ?(backtrace = []) error_type message =
      let error =
        [%message "Vim returned error" ~_:(message : Sexp.t) (error_type : Error_type.t)]
      in
      Error.create_s
        (match backtrace with
         | [] -> error
         | _ :: _ -> List [ error; List backtrace ])
    ;;

    let destruct_vim_error error =
      match error with
      | Sexp.List
          [ List
              [ Atom "Vim returned error"
              ; message
              ; List [ Atom "error_type"; error_type ]
              ]
          ; List backtrace
          ] ->
        Option.try_with (fun () ->
          Error_type.t_of_sexp error_type, message, `Backtrace backtrace)
      | List
          [ Atom "Vim returned error"; message; List [ Atom "error_type"; error_type ] ]
        ->
        Option.try_with (fun () ->
          Error_type.t_of_sexp error_type, message, `Backtrace [])
      | _ -> None
    ;;
  end

  let is_keyboard_interrupt_error = function
    | Msgpack.Array [ Int error_type; String message ] ->
      let error_type = Error_type.of_int error_type in
      (match error_type, message with
       | Exception, "Keyboard interrupt" -> true
       | _ -> false)
    | _ -> false
  ;;

  let rec nvim_call_atomic_response_has_keyboard_interrupt ~name ~params ~response =
    match name, params, response with
    | "nvim_call_atomic", calls, Msgpack.Array [ Array partial_results; maybe_error ] ->
      let has_keyboard_interrupt =
        match maybe_error with
        | Array (Int _idx :: error) -> is_keyboard_interrupt_error (Array error)
        | _ -> false
      in
      let calls_and_results, _ = List.zip_with_remainder calls partial_results in
      List.fold
        calls_and_results
        ~init:has_keyboard_interrupt
        ~f:(fun acc (call, response) ->
          match acc with
          | true -> true
          | false ->
            (match call with
             | Msgpack.Array [ String name; Array params ] ->
               nvim_call_atomic_response_has_keyboard_interrupt ~name ~params ~response
             | _ -> false))
    | _ -> false
  ;;

  let asynchronous_rpcs_must_be_unique ~name = function
    | `Ok -> ()
    | `Duplicate -> failwithf "Already defined asynchronous RPC: %s" name ()
  ;;

  let synchronous_rpcs_must_be_unique ~name = function
    | `Ok -> ()
    | `Duplicate -> failwithf "Already defined synchronous RPC: %s" name ()
  ;;

  module Method_info = struct
    module Callable_via = struct
      type t =
        | Request
        | Notification
        | Request_or_notification
    end

    type t = { callable_via : Callable_via.t }
  end

  let registered_methods ~rpc =
    let subscribed_events =
      nvim_error_event :: unregister_blocking_rpc :: Subscription_manager.events
    in
    let async_methods =
      Msgpack_rpc.Expert.registered_notification_handlers rpc
      |> List.filter ~f:(Fn.non (List.mem subscribed_events ~equal:String.equal))
      |> String.Set.of_list
    in
    let blocking_methods =
      Msgpack_rpc.Expert.registered_request_handlers rpc
      |> List.filter ~f:(Fn.non (String.is_prefix ~prefix:"anon_rpc__"))
      |> String.Set.of_list
    in
    let dual_methods = Set.inter async_methods blocking_methods in
    let async_methods = Set.diff async_methods dual_methods in
    let blocking_methods = Set.diff blocking_methods dual_methods in
    let entries names ~callable_via =
      names |> Set.to_list |> List.map ~f:(fun name -> name, { Method_info.callable_via })
    in
    [ entries dual_methods ~callable_via:Request_or_notification
    ; entries async_methods ~callable_via:Notification
    ; entries blocking_methods ~callable_via:Request
    ]
    |> List.concat
    |> String.Map.of_alist_exn
  ;;
end

open Helpers
open Error_pattern_helpers

module Private = struct
  let unregister_blocking_rpc = unregister_blocking_rpc
  let before_sending_response_hook_for_tests = before_sending_response_hook_for_tests
  let heartbeat_interval = Time_ns.Span.of_int_ms 100

  module Message_type = struct
    type ('in_, 'out) t =
      | Request : ('a, 'a) t
      | Notification : ('a, unit) t
  end

  module Method_info = Method_info

  module rec Callbacks : sig
    type async = [ `asynchronous ] T.t -> Msgpack.t list -> unit Deferred.Or_error.t

    type blocking =
      run_in_background:
        (here:[%call_pos] -> ([ `asynchronous ] T.t -> unit Deferred.Or_error.t) -> unit)
      -> [ `blocking ] T.t
      -> Msgpack.t list
      -> Msgpack.t Deferred.Or_error.t
  end =
    Callbacks

  and T : sig
    (* It's important that the fields in this record not be mutable since whenever the
       client is copied we want the state to be shared. *)
    type 'kind t =
      { channel : int Set_once.t
      ; name : string
      ; register_request_async : here:[%call_pos] -> string -> f:Callbacks.async -> unit
      ; register_request_blocking :
          here:[%call_pos]
          -> string
          -> f:Callbacks.blocking
          -> on_keyboard_interrupt:(unit -> unit)
          -> unit
      ; unregister_request_blocking : name:string -> unit
      ; name_anonymous_blocking_request : unit -> string
      ; registered_methods : unit -> Method_info.t String.Map.t
      ; call_nvim_api_fn :
          'a 'b.
          here:[%call_pos]
          -> ('a, 'b) Message_type.t
          -> 'a Api_result.t
          -> 'b Deferred.Or_error.t
      ; keyboard_interrupts : (unit, read_write) Bvar.t
      ; on_error : Vcaml_error.t -> unit
      ; notify_nvim_of_error : here:[%call_pos] -> Error.t -> unit Deferred.t
      ; subscription_manager : Subscription_manager.t
      ; close : unit -> unit Deferred.t
      ; vcaml_internal_group : int Set_once.t
      }

    type 'kind public = 'kind t
  end =
    T

  include T

  let eq = Type_equal.T

  module Not_connected = struct
    type t =
      { name : string
      ; async_callbacks : (Source_code_position.t * Callbacks.async) String.Table.t
      ; blocking_callbacks :
          (Source_code_position.t
          * Callbacks.blocking
          * [ `on_keyboard_interrupt of unit -> unit ])
            String.Table.t
      ; on_error : Vcaml_error.t -> unit
      }

    let name t = t.name

    let register_request_async ~(here : [%call_pos]) t ~name ~f =
      Hashtbl.add t.async_callbacks ~key:name ~data:(here, f)
      |> asynchronous_rpcs_must_be_unique ~name
    ;;

    let register_request_blocking ~(here : [%call_pos]) t ~name ~f ~on_keyboard_interrupt =
      Hashtbl.add
        t.blocking_callbacks
        ~key:name
        ~data:(here, f, `on_keyboard_interrupt on_keyboard_interrupt)
      |> synchronous_rpcs_must_be_unique ~name
    ;;
  end

  let nvim_set_client_info
    ~(here : [%call_pos])
    t
    ?(version =
      { Client_info.Version.major = None
      ; minor = None
      ; patch = None
      ; prerelease = None
      ; commit = None
      })
    ?(attributes = String.Map.empty)
    ?(client_type = Client_info.Client_type.Remote)
    ()
    =
    let module M = Msgpack in
    let version = Client_info.Version.to_msgpack_map version in
    let client_type = Client_info.Client_type.to_string client_type in
    let methods =
      t.registered_methods ()
      |> Map.map ~f:(fun { callable_via } ->
        let async =
          (* There currently isn't a good way to communicate to Neovim that both Async and
             Sync calls are supported. We currently leave [async] unspecified when this
             happens, but the docs say that an unspecified [async] should be interpreted
             as [false]. Tracked in https://github.com/neovim/neovim/issues/23114. *)
          match callable_via with
          | Notification -> Some true
          | Request -> Some false
          | Request_or_notification -> None
        in
        Client_info.How_to_call_method.to_msgpack { async; nargs = None })
    in
    let attributes = Map.map attributes ~f:(fun attribute -> M.String attribute) in
    Nvim_internal.nvim_set_client_info
      ~name:t.name
      ~version
      ~type_:client_type
      ~methods
      ~attributes
    |> t.call_nvim_api_fn ~here Request
  ;;

  let nvim_list_chans ~(here : [%call_pos]) t =
    Nvim_internal.nvim_list_chans
    |> map_witness ~f:(fun channels ->
      channels
      |> List.map ~f:(fun channel ->
        let%bind.Or_error channel = Type.of_msgpack Dict channel in
        Channel_info.of_msgpack_map channel)
      |> Or_error.combine_errors)
    |> t.call_nvim_api_fn ~here Request
  ;;
end

include Private

let create ~name ~on_error =
  { Not_connected.name
  ; async_callbacks = String.Table.create ()
  ; blocking_callbacks = String.Table.create ()
  ; on_error
  }
;;

module Expiration_reason = struct
  type t =
    | Permission_expiration_is_a_bug
    | Permission_expiration_reason of [ `Rpc_returned | `Keyboard_interrupt ] ref
end

let connect
  ?(time_source = Time_source.wall_clock ())
  { Not_connected.name; async_callbacks; blocking_callbacks; on_error }
  reader
  writer
  =
  let keyboard_interrupts = Bvar.create () in
  let rpc =
    Msgpack_rpc.create ~on_error:(fun error ->
      on_error (Vcaml_error.Msgpack_rpc_error error))
  in
  let rec call_nvim_api_fn_unthrottled
    : type a b.
      Source_code_position.t
      -> (a, b) Message_type.t
      -> a Api_result.t
      -> permission_to_run:Nvim_lock.Permission_to_run.t
      -> expiration_reason:Expiration_reason.t
      -> b Deferred.Or_error.t
    =
    fun here message_type f ~permission_to_run ~expiration_reason ->
    let%tydi { name; params; witness } = f in
    let%bind () = Nvim_lock.Permission_to_run.value_available permission_to_run in
    match Nvim_lock.Permission_to_run.peek permission_to_run with
    | None ->
      call_nvim_api_fn_unthrottled
        here
        message_type
        f
        ~permission_to_run
        ~expiration_reason
    | Some `Expired ->
      let message =
        match expiration_reason with
        | Permission_expiration_is_a_bug ->
          failwith "BUG: A [Permission_to_run.t] expired unexpectedly." [@nontail]
        | Permission_expiration_reason expiration_reason ->
          (match !expiration_reason with
           | `Keyboard_interrupt -> "Keyboard interrupt"
           | `Rpc_returned ->
             "Called Neovim with an expired client. This probably happened because a \
              reference to the client persisted beyond the scope of the callback. The \
              client is not allowed to escape its context, but OCaml's type system \
              cannot prevent it.")
      in
      Error.of_string message |> tag_callsite here |> Deferred.Or_error.fail
    | Some `Ok ->
      (match message_type with
       | Notification ->
         Msgpack_rpc.notify rpc ~method_name:name ~parameters:params
         >>| Result.map_error ~f:(tag_callsite here)
       | Request ->
         let%map result = Msgpack_rpc.call rpc ~method_name:name ~parameters:params in
         (match%bind.Or_error result with
          | Ok response ->
            if nvim_call_atomic_response_has_keyboard_interrupt ~name ~params ~response
            then Bvar.broadcast keyboard_interrupts ();
            Type.of_msgpack witness response
          | Error (Array [ Int error_type; String message ] as error) ->
            if is_keyboard_interrupt_error error
            then Bvar.broadcast keyboard_interrupts ();
            let error_type = Error_type.of_int error_type in
            Error (vim_error error_type (Atom message))
          | Error error ->
            Or_error.error "Msgpack error response" error [%sexp_of: Msgpack.t])
         |> Result.map_error ~f:(tag_callsite here))
  in
  let call_nvim_api_fn
    ~(here : [%call_pos])
    message_type
    api_result
    ~permission_to_run
    ~request_sequencer
    ~expiration_reason
    =
    (* We cannot inline the sequencing because [call_nvim_api_fn_unthrottled] is
       recursive, and enqueueing recursively will deadlock. *)
    Throttle.enqueue request_sequencer (fun () ->
      call_nvim_api_fn_unthrottled
        here
        message_type
        api_result
        ~permission_to_run
        ~expiration_reason)
  in
  let nvim_lock = Nvim_lock.create () in
  let permission_to_run_in_background = Nvim_lock.take nvim_lock in
  let () =
    (* When Neovim is blocked on an [rpcrequest], send heartbeats every
       [heartbeat_interval] to see if the user pressed Ctrl-C. We need to do this until
       Neovim pushes notifications about this proactively
       (https://github.com/neovim/neovim/issues/7546). Without this heartbeating, the
       plugin wouldn't learn that the user pressed Ctrl-C until it called back into
       Neovim. *)
    let permission_to_heartbeat = Nvim_lock.(create () |> take) in
    Time_source.every'
      time_source
      ~stop:(Writer.close_started writer)
      heartbeat_interval
      (fun () ->
         let%bind () =
           match Nvim_lock.Permission_to_run.peek permission_to_run_in_background with
           | Some `Ok -> Nvim_lock.Permission_to_run.taken permission_to_run_in_background
           | Some `Expired ->
             failwith "Bug: Top-level [Permission_to_run.t] expired" [@nontail]
           | None -> return ()
         in
         call_nvim_api_fn_unthrottled
           [%here]
           (* bfredl confirmed that if a request is sent immediately before a response,
              Neovim might process that response as part of the batch of messages it
              received before sending a reply to the request, and that would be considered
              invalid and Neovim would close the connection. To ensure this does not occur
              with heartbeats, we send them as notifications rather than as requests. *)
           Notification
           (* The method we call here seems to matter - some don't go through the code
              path that would lead Neovim to notify us of a keyboard interrupt. *)
           (Nvim_internal.nvim_eval ~expr:"0")
           ~permission_to_run:permission_to_heartbeat
           ~expiration_reason:Permission_expiration_is_a_bug
         |> Deferred.ignore_m)
  in
  let close () =
    let%bind () =
      (* To avoid a Neovim bug in which closing the connection immediately after returning
         a response can fail the request (https://github.com/neovim/neovim/issues/24214),
         we send a request that will only get filled after Neovim has processed any
         responses we sent to its blocking requests. We use [Msgpack_rpc.call] directly
         because waiting for permission to run in the background could deadlock with
         blocking callbacks that call [close], and we use [nvim_input], an api-fast RPC,
         to ensure the request is processed even in the presence of the hit-enter prompt.
      *)
      let%tydi { name; params; witness = _ } = Nvim_internal.nvim_input ~keys:"" in
      Msgpack_rpc.call rpc ~method_name:name ~parameters:params |> Deferred.ignore_m
    in
    let%map () = Writer.close writer
    and () = Reader.close reader in
    ()
  in
  (* Also to avoid https://github.com/neovim/neovim/issues/24214, we ensure an orderly
     close happens on shutdown. *)
  Shutdown.at_shutdown close;
  let background_request_sequencer = Throttle.Sequencer.create () in
  let notify_nvim_of_error ~(here : [%call_pos]) error =
    let error = tag_callsite here (Error.tag error ~tag:name) in
    Nvim_internal.nvim_err_writeln ~str:(Error.to_string_hum error)
    |> call_nvim_api_fn
         Notification
         ~permission_to_run:permission_to_run_in_background
         ~request_sequencer:background_request_sequencer
         ~expiration_reason:Permission_expiration_is_a_bug
    |> Deferred.ignore_m
  in
  let try_with ~(here : [%call_pos]) f =
    Monitor.try_with_join_or_error
      ~rest:
        (`Call
          (fun exn -> don't_wait_for (notify_nvim_of_error ~here (Error.of_exn exn))))
      f
  in
  let client_is_ready = Ivar.create () in
  let called_blocking_rpc = Bvar.create () in
  let rec t =
    { channel = Set_once.create ()
    ; name
    ; register_request_async
    ; register_request_blocking
    ; unregister_request_blocking
    ; name_anonymous_blocking_request =
        (* [Unique_id.Int63] does not give uniqueness between clients, but this isn't
           necessary as RPCs need only be unique per-client (i.e. per-channel in Neovim). *)
        (let module Id = Unique_id.Int63 () in
        fun () ->
          let id = Id.create () in
          [%string "anon_rpc__%{id#Id}"])
    ; registered_methods = (fun () -> registered_methods ~rpc)
    ; call_nvim_api_fn =
        call_nvim_api_fn
          ~permission_to_run:permission_to_run_in_background
          ~request_sequencer:background_request_sequencer
          ~expiration_reason:Permission_expiration_is_a_bug
    ; keyboard_interrupts
    ; on_error
    ; notify_nvim_of_error
    ; subscription_manager = Subscription_manager.create rpc ~on_error
    ; close
    ; vcaml_internal_group = Set_once.create ()
    }
  and run_in_background ~(here : [%call_pos]) f =
    don't_wait_for
      (let%bind () = Ivar.read client_is_ready in
       let%bind () =
         (* Because sending messages to Neovim is gated by a permission check, checking
            that we have permission before invoking [f] may seem redundant. However, if
            [f] will exit the plugin, it's important that the response be returned before
            [exit] is invoked. *)
         Nvim_lock.Permission_to_run.value_available permission_to_run_in_background
       in
       match%bind try_with ~here (fun () -> f t) with
       | Ok () -> return ()
       | Error error -> notify_nvim_of_error ~here error)
  and register_request_async ~(here : [%call_pos]) name ~f =
    Msgpack_rpc.register_notification_handler rpc ~name ~f:(fun params ->
      run_in_background ~here (fun t -> f t params))
    |> asynchronous_rpcs_must_be_unique ~name
  and register_request_blocking ~(here : [%call_pos]) name ~f ~on_keyboard_interrupt =
    Msgpack_rpc.register_request_handler
      rpc
      ~name
      ~f:(fun args ->
        let%bind () = Ivar.read client_is_ready in
        let request_sequencer = Throttle.Sequencer.create () in
        Bvar.broadcast called_blocking_rpc ();
        let flush_neovim_event_queue here ~permission_to_run =
          (* We continue attempting to flush Neovim's event queue until we confirm that
             there is no more hidden activity and Neovim is waiting for the plugin to send
             a message. If we detect a nested RPC we need to reflush both because that RPC
             could have sent notifications and because that RPC invocation may be part of
             further logic that Neovim is waiting to run upon receiving a response (which
             would be sent after the flush). When this returns, [permission_to_run] is
             guaranteed to be filled, because permission would only be lost to a nested
             RPC. *)
          Deferred.repeat_until_finished () (fun () ->
            let%bind () = Nvim_lock.Permission_to_run.value_available permission_to_run in
            match Nvim_lock.Permission_to_run.peek permission_to_run with
            | None -> return (`Repeat ())
            | Some `Expired ->
              failwith "BUG: Permission for callback expired before we expired it."
            | Some `Ok ->
              let called_blocking_rpc_after_flush = Bvar.wait called_blocking_rpc in
              let%map () =
                (* We use [nvim_get_current_buf] instead of [nvim_eval ~expr:"0"] because
                   it's basically just an indirection, rather than invoking an
                   interpreter, so it should be faster. We can't do this for heartbeats
                   because it's in fact so simple that it doesn't invoke the logic we need
                   to prompt Neovim to send us a keyboard interrupt notification. It *is*
                   sufficient to ensure flushing though, since Neovim guarantees it does
                   not process messages out of order
                   (https://github.com/neovim/neovim/issues/19932#issuecomment-1226467468).
                *)
                call_nvim_api_fn
                  ~here
                  ~request_sequencer
                  ~permission_to_run
                  ~expiration_reason:Permission_expiration_is_a_bug
                  Request
                  Nvim_internal.nvim_get_current_buf
                >>| (ignore : Nvim_internal.Buffer.t Or_error.t -> unit)
              in
              (match Deferred.is_determined called_blocking_rpc_after_flush with
               | true -> `Repeat ()
               | false -> `Finished ()))
        in
        let expiration_reason = ref `Rpc_returned in
        let permission_to_run = Nvim_lock.take nvim_lock in
        let result =
          let call_nvim_api_fn =
            call_nvim_api_fn
              ~permission_to_run
              ~request_sequencer
              ~expiration_reason:(Permission_expiration_reason expiration_reason)
          in
          let t = { t with call_nvim_api_fn } in
          let%bind () = flush_neovim_event_queue [%here] ~permission_to_run in
          try_with ~here (fun () -> f ~run_in_background t args)
        in
        (* In the case of a keyboard interrupt we want to return an [Ok] result instead of
           an [Error] because we don't want to display an error message to the user. We
           also send a ":<BS>" command because Neovim doesn't recognize that it's in a
           blocked context so it displays a message about how to exit, and we want to hide
           this message. We use [nvim_feedkeys] instead of [nvim_input] because the latter
           doesn't reliably clear the command line. [nvim_out_write "\n"] also does not
           work here to clear the command line even though it normally does. *)
        let%bind response =
          choose
            [ choice result (fun result ->
                return (Result.map_error result ~f:(tag_callsite here)))
            ; choice (Bvar.wait keyboard_interrupts) (fun () ->
                on_keyboard_interrupt ();
                expiration_reason := `Keyboard_interrupt;
                upon result (fun _ -> expiration_reason := `Rpc_returned);
                let%map () =
                  call_nvim_api_fn
                    ~here
                    Notification
                    ~permission_to_run
                    ~request_sequencer
                    (Nvim_internal.nvim_feedkeys ~keys:":\x08" ~mode:"n" ~escape_ks:false)
                    ~expiration_reason:(Permission_expiration_reason expiration_reason)
                  |> Deferred.ignore_m
                in
                Ok Msgpack.Nil)
            ]
          |> Deferred.join
        in
        let permission_to_run =
          Nvim_lock.expire_other_users nvim_lock permission_to_run
        in
        (* At this point, we need to send the result to Neovim. However, we need to ensure
           that there are no other incoming blocking RPC requests from Neovim. So, we
           flush Neovim's message queue and ensure that there are no such requests prior
           to returning the response. *)
        let%bind () = flush_neovim_event_queue [%here] ~permission_to_run in
        match !before_sending_response_hook_for_tests with
        | None -> return (response, permission_to_run)
        | Some f ->
          let%map () = f () in
          response, permission_to_run)
      ~on_response_sent:(fun p -> Nvim_lock.expire nvim_lock p)
    |> synchronous_rpcs_must_be_unique ~name
  and unregister_request_blocking ~name =
    Msgpack_rpc.Expert.unregister_request_handler rpc ~name
  in
  let () =
    let method_name = nvim_error_event in
    Msgpack_rpc.register_notification_handler rpc ~name:method_name ~f:(fun params ->
      match params with
      | [ Int error_type; String message ] ->
        (match is_keyboard_interrupt_error (Array params) with
         | true -> Bvar.broadcast keyboard_interrupts ()
         | false ->
           let error_type = Error_type.of_int error_type in
           on_error (Nvim_error_event { error_type; message }))
      | _ -> on_error (Nvim_error_event_parse_failure { method_name; params }))
    |> function
    | `Ok -> ()
    | `Duplicate -> failwithf "BUG: Already registered %s handler" method_name ()
  in
  let () =
    let method_name = unregister_blocking_rpc in
    Msgpack_rpc.register_notification_handler rpc ~name:method_name ~f:(fun params ->
      match params with
      | [ String name ] -> unregister_request_blocking ~name
      | _ -> ())
    |> function
    | `Ok -> ()
    | `Duplicate -> failwithf "BUG: Already registered %s handler" method_name ()
  in
  Msgpack_rpc.set_default_notification_handler rpc ~f:(fun ~name _ ->
    don't_wait_for
      (notify_nvim_of_error (Error.of_string [%string "Unknown method %{name}"])));
  Hashtbl.iteri async_callbacks ~f:(fun ~key:name ~data:(here, f) ->
    register_request_async ~here name ~f);
  Hashtbl.iteri
    blocking_callbacks
    ~f:(fun ~key:name ~data:(here, f, `on_keyboard_interrupt on_keyboard_interrupt) ->
      register_request_blocking ~here name ~f ~on_keyboard_interrupt);
  (* It's important that we not establish the connection until after we've registered the
     handlers. *)
  Msgpack_rpc.connect rpc reader writer;
  let uuid = Uuid.to_string (Uuid_unix.create ()) in
  let%bind.Deferred.Or_error () =
    Nvim_internal.nvim_set_client_info
      ~name:uuid
      ~version:String.Map.empty
      ~type_:"remote"
      ~methods:String.Map.empty
      ~attributes:String.Map.empty
    |> t.call_nvim_api_fn Request
  in
  let%bind.Deferred.Or_error channels = nvim_list_chans t in
  List.find_map channels ~f:(fun channel ->
    let open Option.Let_syntax in
    let%bind client = channel.client in
    let%bind client_name = client.name in
    match String.equal uuid client_name with
    | true -> Some channel.id
    | false -> None)
  |> function
  | None -> Deferred.Or_error.error_string "Failed to find current client in channel list"
  | Some channel_id ->
    Set_once.set_exn t.channel channel_id;
    let%bind.Deferred.Or_error () = nvim_set_client_info t () in
    let%bind.Deferred.Or_error vcaml_internal_group =
      Nvim_internal.nvim_create_augroup
        ~name:[%string "vcaml_internal__%{name}__%{uuid}"]
        ~opts:String.Map.empty
      |> t.call_nvim_api_fn Request
    in
    Set_once.set_exn t.vcaml_internal_group vcaml_internal_group;
    Ivar.fill_exn client_is_ready ();
    Deferred.Or_error.return t
;;

(* If we have nested errors from VCaml, we extract the inner error and apply the outer
   backtrace. This will happen in the common case of the [f] callback in [block_nvim]
   returning the error from a VCaml function. *)
let flatten_vcaml_errors error ~anticipated_header =
  match destruct_vim_error [%sexp (error : Error.t)] with
  | None -> error
  | Some (error_type, message, `Backtrace backtrace) ->
    let inner_error =
      match message with
      | List _ -> message
      | Atom message ->
        let message = String.chop_prefix_if_exists message ~prefix:anticipated_header in
        (match Sexp.of_string message with
         | sexp -> sexp
         | exception _ -> Atom message)
    in
    (match destruct_vim_error inner_error with
     | None -> vim_error error_type inner_error ~backtrace
     | Some (error_type, message, `Backtrace inner_backtrace) ->
       vim_error error_type message ~backtrace:(inner_backtrace @ backtrace))
;;

let block_nvim' ~(here : [%call_pos]) t ~f =
  let channel = Set_once.get_exn t.channel in
  let name = t.name_anonymous_blocking_request () in
  let result = Set_once.create () in
  t.register_request_blocking
    ~here:transparent_pos
    name
    ~on_keyboard_interrupt:(fun () -> Set_once.set_exn result `Keyboard_interrupted)
    ~f:(fun ~run_in_background:_ t _args ->
      t.unregister_request_blocking ~name;
      let%map.Deferred.Or_error result' =
        Deferred.Or_error.try_with_join (fun () -> f t)
      in
      Set_once.set_if_none result (`Ok result');
      Msgpack.Nil);
  let%map response =
    Nvim_internal.nvim_call_function ~fn:"rpcrequest" ~args:[ Int channel; String name ]
    |> map_witness ~f:(Type.of_msgpack Nil)
    |> t.call_nvim_api_fn ~here Request
  in
  match response with
  | Error error ->
    let anticipated_header =
      let channel = Set_once.get_exn t.channel in
      [%string "Vim:Error invoking '%{name}' on channel %{channel#Int} (%{t.name}):\n"]
    in
    let error = flatten_vcaml_errors error ~anticipated_header in
    `Error error
  | Ok () -> Set_once.get_exn result
;;

let block_nvim ~(here : [%call_pos]) t ~f =
  match%map block_nvim' ~here t ~f with
  | `Ok result -> Ok result
  | `Error error -> Error error
  | `Keyboard_interrupted ->
    Error (vim_error Exception (Atom "Keyboard interrupt") |> tag_callsite here)
;;

let channel t = Set_once.get_exn t.channel

module Maybe_connected = struct
  type nonrec 'kind t =
    | Connected of 'kind t
    | Not_connected of Not_connected.t
end

(*=--------------------------- Tests of internal functions --------------------------- *)
open struct
  let%expect_test "[tag_callsite]" =
    let test () =
      let open For_testing.Do_not_move in
      Error.of_string "error message"
      |> tag_callsite line3
      |> tag_callsite line4
      |> tag_callsite line5
      |> tag_callsite line6
    in
    let original_bt = Dynamic.get Backtrace.elide in
    Dynamic.set_root Backtrace.elide false;
    print_s [%sexp (test () : Error.t)];
    [%expect
      {|
      ("error message"
       (("Called from" lib/vcaml/src/client.ml:3:16)
        ("Called from" lib/vcaml/src/client.ml:4:16)
        ("Called from" lib/vcaml/src/client.ml:5:16)
        ("Called from" lib/vcaml/src/client.ml:6:16)))
      |}];
    Dynamic.set_root Backtrace.elide true;
    print_s [%sexp (test () : Error.t)];
    [%expect
      {|
      ("error message"
       (("Called from" lib/vcaml/src/client.ml:LINE:COL)
        ("Called from" lib/vcaml/src/client.ml:LINE:COL)
        ("Called from" lib/vcaml/src/client.ml:LINE:COL)
        ("Called from" lib/vcaml/src/client.ml:LINE:COL)))
      |}];
    Dynamic.set_root Backtrace.elide original_bt;
    return ()
  ;;

  let%expect_test "[nvim_call_atomic_response_has_keyboard_interrupt]" =
    let module Scenario = struct
      type t =
        | Sleep
        | Atomic of
            { calls : t list
            ; interrupt : int option
            }

      let rec name_and_params = function
        | Sleep -> "nvim_exec2", [ Msgpack.String "sleep"; Map [] ]
        | Atomic { calls; interrupt = _ } ->
          let params =
            List.map calls ~f:(fun call ->
              let name, params = name_and_params call in
              Msgpack.Array [ String name; Array params ])
          in
          "nvim_call_atomic", params
      ;;

      let response t =
        let rec f = function
          | Sleep -> Msgpack.Map [], false
          | Atomic { calls; interrupt = None } ->
            let results, interrupted = List.map calls ~f |> List.unzip in
            let interrupted = List.exists interrupted ~f:Fn.id in
            Array [ Array results; Nil ], interrupted
          | Atomic { calls; interrupt = Some interrupt } ->
            assert (interrupt >= 0 && interrupt < List.length calls);
            let partial_results, interrupted =
              List.take calls (interrupt - 1) |> List.map ~f |> List.unzip
            in
            assert (not (List.exists interrupted ~f:Fn.id));
            ( Array
                [ Array partial_results
                ; Array [ Int interrupt; Int 0; String "Keyboard interrupt" ]
                ]
            , true )
        in
        f t |> fst
      ;;
    end
    in
    let test scenario =
      let name, params = Scenario.name_and_params scenario in
      let response = Scenario.response scenario in
      let result =
        nvim_call_atomic_response_has_keyboard_interrupt ~name ~params ~response
      in
      print_s [%sexp (result : bool)]
    in
    test Sleep;
    [%expect {| false |}];
    test (Atomic { calls = [ Sleep; Sleep ]; interrupt = None });
    [%expect {| false |}];
    test (Atomic { calls = [ Sleep; Sleep ]; interrupt = Some 1 });
    [%expect {| true |}];
    test
      (Atomic
         { calls = [ Sleep; Atomic { calls = [ Sleep ]; interrupt = None } ]
         ; interrupt = Some 1
         });
    [%expect {| true |}];
    test
      (Atomic
         { calls = [ Sleep; Atomic { calls = [ Sleep ]; interrupt = Some 0 } ]
         ; interrupt = None
         });
    [%expect {| true |}];
    test
      (Atomic
         { calls =
             [ Sleep
             ; Atomic { calls = [ Sleep ]; interrupt = None }
             ; Atomic { calls = [ Sleep; Sleep ]; interrupt = Some 0 }
             ]
         ; interrupt = None
         });
    [%expect {| true |}];
    test
      (Atomic
         { calls =
             [ Sleep
             ; Atomic { calls = [ Sleep ]; interrupt = None }
             ; Atomic { calls = [ Sleep; Sleep ]; interrupt = None }
             ]
         ; interrupt = None
         });
    [%expect {| false |}];
    return ()
  ;;

  let%test_unit "[vim_error] and [destruct_vim_error] roundtrip" =
    let test error_type message backtrace =
      let error = vim_error ~backtrace error_type message in
      let error_type', message', `Backtrace backtrace' =
        destruct_vim_error [%sexp (error : Error.t)] |> Option.value_exn
      in
      assert (Error_type.equal error_type error_type');
      assert (Sexp.equal message message');
      assert (List.equal Sexp.equal backtrace backtrace')
    in
    test Validation (Atom "Test message") [];
    test Exception (Atom "Test message") [ List [ Atom "Called from"; Atom "HERE" ] ]
  ;;
end
