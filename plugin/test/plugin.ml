open! Core
open! Async
open! Vcaml

let during_plugin ~chan_id:_ ~state:_ =
  print_endline "during_plugin";
  Deferred.Or_error.return ()
;;

module Printing_persistent_plugin = Vcaml_plugin.Persistent.Make (struct
    include Vcaml_plugin.Raise_on_any_error

    type state = string

    let rpc_handlers = []

    let startup _client ~shutdown =
      print_s [%message "Persistent.startup"];
      shutdown ();
      Deferred.Or_error.return "test_state"
    ;;

    let vimscript_notify_fn = None

    let on_shutdown _client state =
      print_s [%message "Persistent.on_shutdown" (state : string)];
      Deferred.Or_error.return ()
    ;;
  end)

let%expect_test "persistent plugin calls startup, during, on_shutdown in order" =
  let%map state =
    Vcaml_test.with_client (Printing_persistent_plugin.run_for_testing ~during_plugin)
  in
  printf "after plugin has access to the test state: %s\n" state;
  [%expect
    {|
    Persistent.startup
    during_plugin
    (Persistent.on_shutdown (state test_state))
    after plugin has access to the test state: test_state
    |}]
;;
