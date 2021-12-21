open! Core
open! Async
open! Vcaml

let%expect_test "Oneshot" =
  let links = [ "../bin/main.exe", `In_temp_as, "main.exe" ] in
  let code =
    {|
       function! Rpcrequest(...) abort
         let l:job_id = jobstart(['./main.exe'], { 'rpc': 1 })
         let l:result = call('rpcrequest', [l:job_id] + a:000)
         " Verify that the job finished.
         echo "Job status / exit code: ".jobwait([ l:job_id ], 1000)[0]
         return l:result
       endfunction
       echo "Current buffer: ".Rpcrequest('bufnr')
     |}
  in
  let%map output =
    Vcaml_test.with_client ~links (fun client ->
      run_join [%here] client (Nvim.source ~code))
  in
  print_endline output;
  [%expect {|
    Job status / exit code: 0
    Current buffer: 1 |}]
;;

module Plugin = Vcaml_plugin.Persistent.Make (struct
    let name = "vcaml-test-persistent-plugin"

    type state = unit [@@deriving sexp_of]

    let rpc =
      Vcaml_plugin.Persistent.Rpc.create_async
        ~name:"rpc"
        ~type_:Defun.Ocaml.Async.unit
        ~f:(fun () ~shutdown:_ ~client:_ -> Deferred.Or_error.return ())
    ;;

    let rpc_handlers = [ rpc ]
    let init_state () = ()

    let on_startup _client () ~shutdown =
      print_s [%message "Persistent.on_startup"];
      shutdown ();
      Deferred.Or_error.return ()
    ;;

    let vimscript_notify_fn = None
    let on_error = `Raise

    let on_shutdown _client () =
      print_s [%message "Persistent.on_shutdown"];
      Deferred.Or_error.return ()
    ;;
  end)

let%expect_test "Persistent plugin calls [on_startup] on startup and [on_shutdown] on \
                 shutdown."
  =
  let%map () =
    Vcaml_test.with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind { plugin_state = (); shutdown = _; wait_for_shutdown } =
        Plugin.For_testing.start ~client
      in
      wait_for_shutdown)
  in
  [%expect {|
    Persistent.on_startup
    Persistent.on_shutdown
    |}]
;;

module type S = Vcaml_plugin.Persistent.S with type state := unit

let make_plugin ?on_startup ?on_shutdown ?vimscript_notify_fn ?async_rpc ?blocking_rpc () =
  (module Vcaml_plugin.Persistent.Make (struct
       let name = "vcaml-test-persistent-plugin"

       type state = unit [@@deriving sexp_of]

       let async_rpc =
         Vcaml_plugin.Persistent.Rpc.create_async
           ~name:"async-rpc"
           ~type_:Defun.Ocaml.Async.unit
           ~f:(fun () ~shutdown ~client ->
             print_s [%message "Called async-rpc"];
             match async_rpc with
             | Some f -> f ~shutdown ~client
             | None -> Deferred.Or_error.return ())
       ;;

       let blocking_rpc =
         Vcaml_plugin.Persistent.Rpc.create_sync
           ~name:"blocking-rpc"
           ~type_:Defun.Ocaml.Sync.(return Nil)
           ~f:(fun () ~shutdown ~keyboard_interrupted:_ ~client ->
             print_s [%message "Called blocking-rpc"];
             match blocking_rpc with
             | Some f -> f ~shutdown ~client
             | None -> Deferred.Or_error.return ())
       ;;

       let rpc_handlers = [ async_rpc; blocking_rpc ]
       let init_state () = ()

       let on_startup client () ~shutdown =
         print_s [%message "Persistent.on_startup"];
         match on_startup with
         | Some f -> f client ~shutdown
         | None -> Deferred.Or_error.return ()
       ;;

       let vimscript_notify_fn = vimscript_notify_fn
       let on_error = `Raise

       let on_shutdown client () =
         print_s [%message "Persistent.on_shutdown"];
         match on_shutdown with
         | Some f -> f client
         | None -> Deferred.Or_error.return ()
       ;;
     end) : S)
;;

let%expect_test "Persistent plugin shows errors when they occur during [on_startup]." =
  let on_startup _client ~shutdown:_ = Deferred.Or_error.error_string "Failure" in
  let (module Plugin) = make_plugin ~on_startup () in
  let%bind screen =
    Vcaml_test.with_ui_client (fun client ui ->
      let%bind (_ : Plugin.For_testing.State.t Or_error.t) =
        Plugin.For_testing.start ~client
      in
      Vcaml_test.wait_until_text
        [%here]
        ui
        ~f:(String.is_substring ~substring:"Failure"))
  in
  print_endline screen;
  [%expect
    {|
    Persistent.on_startup
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │                                                                                │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │[No Name]                                                     0,0-1          All│
    │(vcaml-test-persistent-plugin Failure)                                          │
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;

let%expect_test "Persistent plugin shows errors when they occur during \
                 [vimscript_notify_fn]."
  =
  let vimscript_fn =
    {| function! OnStartup(channel)
         echoerr "Failure"
       endfunction |}
  in
  let (module Plugin) = make_plugin ~vimscript_notify_fn:"OnStartup" () in
  let%bind screen =
    Vcaml_test.with_ui_client ~width:84 (fun client ui ->
      let%bind () =
        [ Nvim.source ~code:vimscript_fn |> Api_call.Or_error.map ~f:ignore
        ; Nvim.command ~command:"set cmdheight=3"
        ]
        |> Api_call.Or_error.all_unit
        |> run_join [%here] client
        >>| ok_exn
      in
      let%bind (_ : Plugin.For_testing.State.t Or_error.t) =
        Plugin.For_testing.start ~client
      in
      Vcaml_test.wait_until_text
        [%here]
        ui
        ~f:(String.is_substring ~substring:"Failure"))
  in
  print_endline screen;
  [%expect
    {|
    Persistent.on_startup
    ╭────────────────────────────────────────────────────────────────────────────────────╮
    │                                                                                    │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │~                                                                                   │
    │[No Name]                                                         0,0-1          All│
    │(vcaml-test-persistent-plugin                                                       │
    │ (("Called from" lib/vcaml/plugin/src/vcaml_plugin.ml:89:25)                        │
    │  ("Vim returned error" "Vim(echoerr):Failure" (error_type Exception))))            │
    ╰────────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;

let%expect_test "Persistent plugin shows errors when they occur during [on_shutdown]." =
  let on_shutdown _client = Deferred.Or_error.error_string "Failure" in
  let (module Plugin) = make_plugin ~on_shutdown () in
  let%bind screen =
    Vcaml_test.with_ui_client (fun client ui ->
      let%bind { plugin_state = (); shutdown; wait_for_shutdown } =
        Plugin.For_testing.start ~client >>| ok_exn
      in
      shutdown ();
      let%bind (_ : unit Or_error.t) = wait_for_shutdown in
      Vcaml_test.wait_until_text
        [%here]
        ui
        ~f:(String.is_substring ~substring:"Failure"))
  in
  print_endline screen;
  [%expect
    {|
    Persistent.on_startup
    Persistent.on_shutdown
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │                                                                                │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │[No Name]                                                     0,0-1          All│
    │(vcaml-test-persistent-plugin Failure)                                          │
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;

let%expect_test "Persistent plugin shows errors when they occur during async RPC." =
  let async_rpc ~shutdown:_ ~client:_ = Deferred.Or_error.error_string "Failure" in
  let (module Plugin) = make_plugin ~async_rpc () in
  let%bind screen =
    Vcaml_test.with_ui_client (fun client ui ->
      let%bind (_ : Plugin.For_testing.State.t Or_error.t) =
        Plugin.For_testing.start ~client
      in
      let rpcnotify =
        wrap_viml_function
          ~function_name:"rpcnotify"
          ~type_:Defun.Vim.(Integer @-> String @-> return Integer)
          (Client.rpc_channel_id client)
          "async-rpc"
        |> Api_call.Or_error.map ~f:ignore
      in
      let%bind () = run_join [%here] client rpcnotify >>| ok_exn in
      Vcaml_test.wait_until_text
        [%here]
        ui
        ~f:(String.is_substring ~substring:"Failure"))
  in
  print_endline screen;
  [%expect
    {|
    Persistent.on_startup
    "Called async-rpc"
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │                                                                                │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │[No Name]                                                     0,0-1          All│
    │(vcaml-test-persistent-plugin Failure)                                          │
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;

let%expect_test "Persistent plugin shows errors when they occur during blocking RPC." =
  let blocking_rpc ~shutdown:_ ~client:_ = Deferred.Or_error.error_string "Failure" in
  let (module Plugin) = make_plugin ~blocking_rpc () in
  let%bind screen =
    Vcaml_test.with_ui_client ~width:83 (fun client ui ->
      let%bind (_ : Plugin.For_testing.State.t Or_error.t) =
        Plugin.For_testing.start ~client
      in
      let channel = Client.rpc_channel_id client in
      let rpcrequest =
        Nvim.command
          ~command:
            [%string
              "call timer_start(0, { _ -> rpcrequest(%{channel#Int}, 'blocking-rpc') })"]
      in
      let request =
        let%map.Api_call.Or_error () = rpcrequest
        and () = Nvim.command ~command:"set cmdheight=4"
        and channel_info = Nvim.get_chan_info ~chan:channel in
        channel_info
      in
      let%bind channel_info = run_join [%here] client request >>| ok_exn in
      let client_info = Option.value_exn channel_info.client in
      let client_name = Option.value_exn client_info.name in
      let%map screen =
        Vcaml_test.wait_until_text
          [%here]
          ui
          ~f:(String.is_substring ~substring:"Failure")
      in
      Or_error.map screen ~f:(fun screen ->
        String.substr_replace_all
          screen
          ~pattern:client_name
          ~with_:(Uuid.Private.nil |> Uuid.to_string)))
  in
  print_endline screen;
  [%expect
    {|
    Persistent.on_startup
    "Called blocking-rpc"
    ╭───────────────────────────────────────────────────────────────────────────────────╮
    │                                                                                   │
    │~                                                                                  │
    │~                                                                                  │
    │~                                                                                  │
    │~                                                                                  │
    │~                                                                                  │
    │~                                                                                  │
    │~                                                                                  │
    │~                                                                                  │
    │~                                                                                  │
    │~                                                                                  │
    │~                                                                                  │
    │~                                                                                  │
    │~                                                                                  │
    │~                                                                                  │
    │~                                                                                  │
    │~                                                                                  │
    │~                                                                                  │
    │~                                                                                  │
    │~                                                                                  │
    │~                                                                                  │
    │~                                                                                  │
    │~                                                                                  │
    │~                                                                                  │
    │~                                                                                  │
    │[No Name]                                                        0,0-1          All│
    │Error detected while processing function <lambda>1:                                │
    │line    1:                                                                         │
    │Error invoking 'blocking-rpc' on channel 1 (00000000-0000-0000-0000-000000000000): │
    │Failure                                                                            │
    ╰───────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;
