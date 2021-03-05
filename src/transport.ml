open Core
open Async

module Make (U : Msgpack_rpc.S) = struct
  let call_nvim_api_fn ~conn f =
    let { Types.Api_result.name; params; witness } = f in
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

  let attach (conn : U.t) =
    let result =
      let events = create_event_subscription ~conn in
      let register_request ~name ~f = U.register_method ~name ~f in
      Types.Client.Private.to_public
        { events
        ; call_nvim_api_fn = (fun f -> call_nvim_api_fn ~conn f)
        ; register_request
        ; buffers_attached = Types.Buffer.Table.create ()
        ; attach_sequencer = Throttle.Sequencer.create ~continue_on_error:false ()
        }
    in
    return result
  ;;
end
