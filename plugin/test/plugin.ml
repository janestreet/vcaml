open! Core
open! Async
open! Vcaml

module Printing_oneshot_plugin = Vcaml_plugin.Oneshot.Make (struct
    include Vcaml_plugin.Raise_on_any_error

    let execute _client =
      print_endline "Oneshot.execute";
      Deferred.Or_error.return ()
    ;;
  end)

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

let%expect_test "oneshot plugin executes as expected" =
  let%map () = Vcaml_test.with_client Printing_oneshot_plugin.run_for_testing in
  [%expect {| Oneshot.execute |}]
;;

let%expect_test "oneshot plugin can be called multiple times with the same client" =
  let%map () =
    Vcaml_test.with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      print_endline "before_plugin";
      let%bind () = Printing_oneshot_plugin.run_for_testing client in
      let%bind () = Printing_oneshot_plugin.run_for_testing client in
      print_endline "after_plugin";
      return ())
  in
  [%expect
    {|
    before_plugin
    Oneshot.execute
    Oneshot.execute
    after_plugin
    |}]
;;

let%expect_test "persistent plugin calls  startup, during, on_shutdown in order" =
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
