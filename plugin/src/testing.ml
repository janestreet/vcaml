open! Async
open Vcaml

let run_rpc_call ~client ~chan_id ~name ~args =
  Vcaml.run_join
    client
    (Client.call_function
       ~fn:"rpcrequest"
       ~args:([ Msgpack.Integer chan_id; String name ] @ args))
;;

let run_rpc_calls ~client ~chan_id ~rpcs =
  rpcs
  |> Deferred.Or_error.List.iter ~f:(fun (name, args, after_call) ->
    let%bind.Deferred.Or_error result = run_rpc_call ~client ~chan_id ~name ~args in
    after_call result)
;;
