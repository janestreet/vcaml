open Core
open Async

module Make (U : Msgpack_rpc.S) = struct
  let call_nvim_api_fn (type a b) ~conn ~message_type f : b =
    let { Nvim_internal.Api_result.name; params; witness } = f in
    match (message_type : (a, b) Client.Private.Message_type.t) with
    | Notification -> U.notify conn ~method_name:name ~parameters:params
    | Request ->
      U.call conn ~method_name:name ~parameters:params
      >>| Extract.convert_msgpack_error
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
    U.subscribe conn [%here]
    |> Pipe.iter_without_pushback ~f:(Bus.write result)
    |> don't_wait_for;
    Bus.read_only result
  ;;

  let attach (conn : U.t) ~on_error =
    let events = create_event_subscription ~conn in
    let register_request ~name ~f = U.register_method ~name ~f in
    { Client.Private.events
    ; call_nvim_api_fn = (fun f message_type -> call_nvim_api_fn ~conn ~message_type f)
    ; register_request
    ; buffers_attached = Nvim_internal.Buffer.Table.create ()
    ; attach_sequencer = Throttle.Sequencer.create ~continue_on_error:false ()
    ; on_error
    }
    |> Type_equal.conv Client.Private.eq
    |> return
  ;;
end
