open! Core
open! Async
open! Vcaml

module Printing_oneshot_plugin = Vcaml_plugin.Make_oneshot (struct
    let execute _client =
      print_endline "Oneshot.execute";
      Deferred.Or_error.return ()
    ;;
  end)

let before_plugin ~client:_ =
  print_endline "before_plugin";
  Deferred.Or_error.return ()
;;

let during_plugin ~client:_ ~chan_id:_ ~state:_ =
  print_endline "during_plugin";
  Deferred.Or_error.return ()
;;

let after_plugin ~client:_ ~state:_ =
  print_endline "after_plugin";
  Deferred.Or_error.return ()
;;

module Printing_persistent_plugin = Vcaml_plugin.Make_persistent (struct
    type state = string

    let rpc_handlers = []

    let startup (_client, shutdown) =
      print_s [%message "Persistent.startup"];
      shutdown ();
      Deferred.Or_error.return "test_state"
    ;;

    let vimscript_notify_fn = None

    let on_shutdown (_client, state) =
      print_s [%message "Persistent.on_shutdown" (state : string)];
      Deferred.Or_error.return ()
    ;;
  end)

let%expect_test "testing oneshot plugin calls before, execute, and after in order" =
  let%map () = Printing_oneshot_plugin.test ~before_plugin ~after_plugin () in
  [%expect {|
    before_plugin
    Oneshot.execute
    after_plugin |}]
;;

let%expect_test "testing oneshot plugin and providing a during_plugin function gives \
                 warning and does not call during_plugin"
  =
  let%map () =
    Printing_oneshot_plugin.test ~before_plugin ~during_plugin ~after_plugin ()
  in
  [%expect
    {|
    Warning: supplying a during_plugin function when testing a oneshot plugin will not call the during_plugin.
    before_plugin
    Oneshot.execute
    after_plugin
    |}]
;;

let%expect_test "testing persistent plugin calls before, startup, during, on_shutdown, \
                 and after in order"
  =
  let%map () =
    Printing_persistent_plugin.test ~before_plugin ~during_plugin ~after_plugin ()
  in
  [%expect
    {|
    before_plugin
    Persistent.startup
    during_plugin
    (Persistent.on_shutdown (state test_state))
    after_plugin |}]
;;
