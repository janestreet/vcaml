open Core
open Async

module Private = struct
  module Message_type = struct
    type ('in_, 'out) t =
      | Request : ('a, 'a Deferred.Or_error.t) t
      | Notification : ('a, unit) t
  end

  module rec Callbacks : sig
    type async = [ `connected ] T.t -> Msgpack.t list -> unit

    type blocking =
      keyboard_interrupted:unit Deferred.t
      -> [ `connected ] T.t
      -> Msgpack.t list
      -> Msgpack.t Deferred.Or_error.t
  end =
    Callbacks

  and State : sig
    module Connected : sig
      type t =
        { channel : int Set_once.t
        ; call_nvim_api_fn :
            'a 'b. 'a Nvim_internal.Api_result.t -> ('a, 'b) Message_type.t -> 'b
        ; buffers_attached : int Nvim_internal.Buffer.Table.t
        ; attach_sequencer : unit Sequencer.t
        ; close : unit -> unit Deferred.t
        }
    end

    module Not_connected : sig
      type t =
        { rpc : [ `not_connected ] Msgpack_rpc.t
        ; async_callbacks : Callbacks.async String.Table.t
        ; blocking_callbacks : Callbacks.blocking String.Table.t
        }
    end

    type 'state t =
      | Connected : Connected.t -> [ `connected ] t
      | Not_connected : Not_connected.t -> [ `not_connected ] t
  end =
    State

  and T : sig
    type 'state t =
      { keyboard_interrupts : (unit, read_write) Bvar.t
      ; events : (Msgpack_rpc.Event.t -> unit) Bus.Read_only.t
      ; register_request_async : name:string -> f:Callbacks.async -> unit
      ; register_request_blocking : name:string -> f:Callbacks.blocking -> unit
      ; on_error : Vcaml_error.t -> unit
      ; state : 'state State.t
      }
  end =
    T

  include T

  let eq = Type_equal.T
end

include Private

let call_nvim_api_fn (type a b) ~on_keyboard_interrupt ~rpc ~message_type f : b =
  let { Nvim_internal.Api_result.name; params; witness } = f in
  match (message_type : (a, b) Private.Message_type.t) with
  | Notification -> Msgpack_rpc.notify rpc ~method_name:name ~parameters:params
  | Request ->
    Msgpack_rpc.call rpc ~method_name:name ~parameters:params
    >>| Extract.convert_msgpack_error ~on_keyboard_interrupt
    >>| Or_error.bind ~f:(Extract.value witness)
;;

let get_channel ~rpc =
  let open Deferred.Or_error.Let_syntax in
  let name = Uuid.to_string (Uuid_unix.create ()) in
  let request (type a) (api_result : a Nvim_internal.Api_result.t) =
    call_nvim_api_fn ~on_keyboard_interrupt:ignore ~rpc ~message_type:Request api_result
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
  let channel =
    List.find_map channels ~f:(fun channel ->
      let open Option.Let_syntax in
      let%bind client = channel.client in
      let%bind name' = client.name in
      match String.equal name name' with
      | true -> Some channel.id
      | false -> None)
  in
  match channel with
  | Some channel -> return channel
  | None ->
    Deferred.Or_error.error_string "Failed to find find current client in channel list"
;;

let asynchronous_rpcs_must_be_unique ~name = function
  | `Ok -> ()
  | `Duplicate -> failwithf "Already defined asynchronous RPC '%s'" name ()
;;

let synchronous_rpcs_must_be_unique ~name = function
  | `Ok -> ()
  | `Duplicate -> failwithf "Already defined synchronous RPC '%s'" name ()
;;


let create ~on_error =
  let rpc =
    Msgpack_rpc.create ~on_error:(fun error ->
      on_error (Vcaml_error.Msgpack_rpc_error error))
  in
  let keyboard_interrupts = Bvar.create () in
  let events = Msgpack_rpc.notifications rpc in
  Bus.iter_exn events [%here] ~f:(fun ({ method_name; params } as event) ->
    match method_name with
    | "nvim_error_event" ->
      (match params with
       | [ Integer error_type; String message ] ->
         (match Nvim_internal.Error_type.of_int error_type, message with
          | Exception, "Keyboard interrupt" -> Bvar.broadcast keyboard_interrupts ()
          | error_type, _ -> on_error (Nvim_error_event (Error { error_type; message })))
       | _ -> on_error (Nvim_error_event (Parse_failure event)))
    | _ -> ());
  let async_callbacks = String.Table.create () in
  let blocking_callbacks = String.Table.create () in
  let register_request_async ~name ~f =
    Hashtbl.add async_callbacks ~key:name ~data:f
    |> asynchronous_rpcs_must_be_unique ~name
  in
  let register_request_blocking ~name ~f =
    Hashtbl.add blocking_callbacks ~key:name ~data:f
    |> synchronous_rpcs_must_be_unique ~name
  in
  let not_connected = { State.Not_connected.rpc; async_callbacks; blocking_callbacks } in
  { keyboard_interrupts
  ; events
  ; register_request_async
  ; register_request_blocking
  ; on_error
  ; state = Not_connected not_connected
  }
;;

let connect t reader writer ~close_reader_and_writer_on_disconnect ~time_source =
  let { keyboard_interrupts
      ; events
      ; register_request_async = _
      ; register_request_blocking = _
      ; on_error
      ; state = Not_connected { rpc; async_callbacks; blocking_callbacks }
      }
    =
    t
  in
  (* From the point where we connect the RPC it is crucial that we not yield to the Async
     scheduler until the pre-registered RPCs are registered. *)
  let rpc =
    Msgpack_rpc.connect rpc reader writer ~close_reader_and_writer_on_disconnect
  in
  let on_keyboard_interrupt = Bvar.broadcast keyboard_interrupts in
  let call_nvim_api_fn f message_type =
    call_nvim_api_fn ~on_keyboard_interrupt ~rpc ~message_type f
  in
  let () =
    (* Send heartbeats every 100ms to see if the user pressed Ctrl-C. This is pretty
       wasteful, but we need to do this until Neovim pushes notifications about this
       proactively (https://github.com/neovim/neovim/issues/7546). Without heartbeating,
       if Neovim issues an [rpcrequest] that blocks on some OCaml logic, the plugin
       won't learn that the user pressed Ctrl-C until it calls back into Neovim. *)
    Time_source.every
      time_source
      ~stop:(Writer.close_started (Msgpack_rpc.writer rpc))
      (Time_ns.Span.create ~ms:100 ())
      (fun () ->
         if not (Writer.is_closed (Msgpack_rpc.writer rpc))
         then call_nvim_api_fn (Nvim_internal.nvim_eval ~expr:"0") Notification)
  in
  let close () =
    let%map () = Writer.close (Msgpack_rpc.writer rpc)
    and () = Reader.close (Msgpack_rpc.reader rpc) in
    ()
  in
  let channel = Set_once.create () in
  let async_rpcs = String.Hash_set.create () in
  let rec t =
    { keyboard_interrupts
    ; events
    ; register_request_async
    ; register_request_blocking
    ; on_error
    ; state =
        Connected
          { channel
          ; call_nvim_api_fn
          ; buffers_attached = Nvim_internal.Buffer.Table.create ()
          ; attach_sequencer = Throttle.Sequencer.create ~continue_on_error:false ()
          ; close
          }
    }
  and register_request_async ~name ~f =
    match Hash_set.strict_add async_rpcs name with
    | Error _ -> asynchronous_rpcs_must_be_unique ~name `Duplicate
    | Ok () ->
      Bus.iter_exn events [%here] ~f:(fun { method_name; params } ->
        match String.equal method_name name with
        | false -> ()
        | true -> f t params)
  and register_request_blocking ~name ~f =
    Msgpack_rpc.register_method rpc ~name ~f:(fun args ->
      let keyboard_interrupted = Bvar.wait keyboard_interrupts in
      let result = f ~keyboard_interrupted t args in
      (* In the case of a keyboard interrupt we want to return an [Ok] result instead of
         an [Error] because we don't want to display an error message to the user. We also
         send ":<BS>" command because Neovim doesn't recognize that it's in a blocked
         context so it displays a message about how to exit, and we want to hide this
         message. We use [nvim_feedkeys] instead of [nvim_input] because the latter
         doesn't reliably clear the command line. [nvim_out_write "\n"] also does not work
         here to clear the command line even though it normally does. *)
      choose
        [ choice result Fn.id
        ; choice keyboard_interrupted (fun () ->
            call_nvim_api_fn
              (Nvim_internal.nvim_feedkeys ~keys:":\x08" ~mode:"n" ~escape_ks:false)
              Notification;
            Ok Msgpack.Nil)
        ])
    |> synchronous_rpcs_must_be_unique ~name
  in
  Hashtbl.iteri async_callbacks ~f:(fun ~key:name ~data:f ->
    register_request_async ~name ~f);
  Hashtbl.iteri blocking_callbacks ~f:(fun ~key:name ~data:f ->
    register_request_blocking ~name ~f);
  match%map get_channel ~rpc with
  | Error _ as error -> error
  | Ok channel_id ->
    Set_once.set_exn channel [%here] channel_id;
    Ok t
;;
