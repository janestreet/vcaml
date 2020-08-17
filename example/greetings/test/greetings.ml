open! Core
open! Async
open! Vcaml_greetings

let print_response msg = Deferred.Or_error.return (print_s ([%sexp_of: Msgpack.t] msg))
let print_shutdown _ = Deferred.Or_error.return (print_s (Sexp.Atom "Shutdown success"))
let make_test_rpc ~name ~fn ~args = name, args, fn
let before_plugin ~client:_ = Deferred.Or_error.return ()

let during_plugin ~client ~chan_id ~state:() =
  let rpcs =
    [ make_test_rpc ~name:"greeting" ~fn:print_response ~args:[ Msgpack.String "Jane" ]
    ; make_test_rpc ~name:"shutdown" ~fn:print_shutdown ~args:[ Msgpack.Nil ]
    ]
  in
  Vcaml_plugin.Testing.run_rpc_calls ~client ~chan_id ~rpcs
;;

let after_plugin ~client:_ ~state:_ = Deferred.Or_error.return ()

let%expect_test "plugin responds to RPC requests and shuts down" =
  let%map () = Greetings.test ~before_plugin ~during_plugin ~after_plugin () in
  [%expect {|
  (String "Hello, Jane!")
  "Shutdown success" |}]
;;
