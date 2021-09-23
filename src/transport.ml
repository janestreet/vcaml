open Core
open Async

let call_nvim_api_fn (type a b) ~on_keyboard_interrupt ~conn ~message_type f : b =
  let { Nvim_internal.Api_result.name; params; witness } = f in
  match (message_type : (a, b) Client.Private.Message_type.t) with
  | Notification -> Msgpack_rpc.notify conn ~method_name:name ~parameters:params
  | Request ->
    Msgpack_rpc.call conn ~method_name:name ~parameters:params
    >>| Extract.convert_msgpack_error ~on_keyboard_interrupt
    >>| Or_error.bind ~f:(Extract.value witness)
;;

(* The event subscription is just a bus that distributes all events that go through the
   client. *)
let create_event_subscription ~conn =
  let result =
    Bus.create
      [%here]
      Bus.Callback_arity.Arity1
      ~on_subscription_after_first_write:Allow_and_send_last_value
      ~on_callback_raise:Error.raise
  in
  Msgpack_rpc.subscribe conn [%here]
  |> Pipe.iter_without_pushback ~f:(Bus.write result)
  |> don't_wait_for;
  Bus.read_only result
;;

let get_rpc_channel_id ~conn =
  let open Deferred.Or_error.Let_syntax in
  let name = Uuid.to_string (Uuid_unix.create ()) in
  let request (type a) (api_result : a Nvim_internal.Api_result.t) =
    call_nvim_api_fn ~on_keyboard_interrupt:ignore ~conn ~message_type:Request api_result
  in
  let%bind () =
    Nvim_internal.nvim_set_client_info
      ~name
      ~version:[]
      ~type_:"remote"
      ~methods:[]
      ~attributes:[]
    |> request
  in
  let%bind channels =
    request Nvim_internal.nvim_list_chans
    >>| List.map ~f:Channel_info.of_msgpack
    >>| Or_error.combine_errors
    |> Deferred.map ~f:Or_error.join
  in
  let channel_id =
    List.find_map channels ~f:(fun channel ->
      let open Option.Let_syntax in
      let%bind client = channel.client in
      let%bind name' = client.name in
      match String.equal name name' with
      | true -> Some channel.id
      | false -> None)
  in
  match channel_id with
  | Some channel_id -> return channel_id
  | None ->
    Deferred.Or_error.error_string "Failed to find find current client in channel list"
;;

let attach (conn : Msgpack_rpc.t) ~on_error ~on_error_event ~time_source =
  match%map get_rpc_channel_id ~conn with
  | Error _ as error -> error
  | Ok rpc_channel_id ->
    let events = create_event_subscription ~conn in
    let keyboard_interrupts = Bvar.create () in
    let on_keyboard_interrupt = Bvar.broadcast keyboard_interrupts in
    let () =
      (* Send heartbeats every 100ms to see if the user pressed Ctrl-C. This is pretty
         wasteful, but we need to do this until Neovim pushes notifications about this
         proactively (https://github.com/neovim/neovim/issues/7546). Without heartbeating,
         if Neovim issues an [rpcrequest] that blocks on some OCaml logic, the plugin
         won't learn that the user pressed Ctrl-C until it calls back into Neovim. *)
      Time_source.every
        time_source
        ~stop:(Writer.close_started (Msgpack_rpc.writer conn))
        (Time_ns.Span.create ~ms:100 ())
        (fun () ->
           if not (Writer.is_closed (Msgpack_rpc.writer conn))
           then
             call_nvim_api_fn
               ~on_keyboard_interrupt
               ~conn
               ~message_type:Notification
               (Nvim_internal.nvim_eval ~expr:"0"))
    in
    Bus.iter_exn events [%here] ~f:(fun ({ method_name; params } as event) ->
      match method_name with
      | "nvim_error_event" ->
        (match params with
         | [ Integer error_type; String message ] ->
           (match Nvim_internal.Error_type.of_int error_type, message with
            | Ok Exception, "Keyboard interrupt" -> Bvar.broadcast keyboard_interrupts ()
            | Ok error_type, _ -> on_error_event error_type ~message
            | Error error, _ ->
              on_error
                (Error.create_s
                   [%message
                     "Unknown error event type"
                       (event : Msgpack_rpc.event)
                       (error : Error.t)]))
         | _ ->
           on_error
             (Error.create_s [%message "Argument mismatch" (event : Msgpack_rpc.event)]))
      | _ -> ());
    let register_request ~name ~f = Msgpack_rpc.register_method conn ~name ~f in
    let close () =
      let%map () = Writer.close (Msgpack_rpc.writer conn)
      and () = Reader.close (Msgpack_rpc.reader conn) in
      ()
    in
    { Client.Private.rpc_channel_id
    ; events
    ; call_nvim_api_fn =
        (fun f message_type ->
           call_nvim_api_fn ~on_keyboard_interrupt ~conn ~message_type f)
    ; register_request
    ; buffers_attached = Nvim_internal.Buffer.Table.create ()
    ; attach_sequencer = Throttle.Sequencer.create ~continue_on_error:false ()
    ; on_error
    ; keyboard_interrupts
    ; close
    }
    |> Type_equal.conv Client.Private.eq
    |> Or_error.return
;;
