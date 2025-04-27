open Core
open Async
open Vcaml
open Vcaml_test_helpers

(* Tests for what should happen to a running plugin if Neovim crashes. This is probably
   only relevant for plugins that are daemonized since Neovim by default kills its child
   processes on exit. *)

let rec omit_unstable_writer_info : Sexp.t -> Sexp.t = function
  | Atom _ as atom -> atom
  | List (Atom "info" :: _) -> List [ Atom "info"; Atom "<omitted>" ]
  | List [ Atom "String"; Atom msgpack_error ] ->
    let msgpack_error =
      try Sexp.of_string msgpack_error with
      | _ -> Atom msgpack_error
    in
    List [ Atom "String"; omit_unstable_writer_info msgpack_error ]
  | List sexps -> List (List.map sexps ~f:omit_unstable_writer_info)
;;

let%expect_test "Crash before sending a request results in an error" =
  Expect_test_helpers_async.within_temp_dir (fun () ->
    Dynamic.set_root Backtrace.elide true;
    let%bind working_dir = Sys.getcwd () in
    let%bind client, nvim =
      let client = Client.create ~name:"test-client" ~on_error:`Raise in
      Private.attach_client
        client
        (Embed
           { prog = Private.neovim_path
           ; args = [ "--headless"; "-n"; "--embed"; "--clean"; "--listen"; "./socket" ]
           ; working_dir
           ; env = `Extend [ "NVIM_RPLUGIN_MANIFEST", "rplugin.vim" ]
           })
        ~time_source:(Time_source.read_only (Time_source.create ~now:Time_ns.epoch ()))
      >>| ok_exn
    in
    Process.send_signal nvim Signal.term;
    let%bind exit_or_signal = Process.wait nvim in
    print_s [%message "nvim exited" (exit_or_signal : Unix.Exit_or_signal.t)];
    [%expect {| ("nvim exited" (exit_or_signal (Error (Exit_non_zero 1)))) |}];
    let%bind result = Nvim.get_current_buf client in
    let error = omit_unstable_writer_info [%sexp (result : Buffer.t Or_error.t)] in
    let expected_errors =
      (* There are some races around trying to send requests to Neovim as the writer is
         closing due to a closed connection - we really just want to test that some error
         is returned in this case, so both of these are fine. If an entry in this list
         needs to be modified, consider whether analogous modifications need to be made to
         the other entries. *)
      [ {|
        (Error
         (("Failed to send Msgpack RPC message: writer is closed"
           (Array ((Int 0) (Int 4) (String nvim_get_current_buf) (Array ()))))
          (("Called from" lib/vcaml/test/semantics/test_nvim_crash.ml:LINE:COL)))) |}
      ; {|
        (Error
         (("Failed to send Msgpack RPC message: consumer left"
           (Array ((Int 0) (Int 4) (String nvim_get_current_buf) (Array ()))))
          (("Called from" lib/vcaml/test/semantics/test_nvim_crash.ml:LINE:COL)))) |}
      ; {|
        (Error
         (("Failed to send Msgpack RPC message"
           (Array ((Int 0) (Int 4) (String nvim_get_current_buf) (Array ())))
           (error
            ("Writer error from inner_monitor"
             (Unix.Unix_error "Broken pipe" writev_assume_fd_is_nonblocking "")
             (writer ((file_descr _) (info <omitted>) (kind Fifo))))))
          (("Called from" lib/vcaml/test/semantics/test_nvim_crash.ml:LINE:COL)))) |}
      ]
      |> List.map ~f:Sexp.of_string
    in
    if not (List.mem expected_errors error ~equal:Sexp.equal) then print_s error;
    [%expect {| |}];
    Dynamic.set_root Backtrace.elide false;
    Client.close client)
;;

let%expect_test "Crash while waiting for a response results in an error" =
  Expect_test_helpers_async.within_temp_dir (fun () ->
    Dynamic.set_root Backtrace.elide true;
    let%bind working_dir = Sys.getcwd () in
    let%bind client, nvim =
      let client = Client.create ~name:"test-client" ~on_error:`Raise in
      Private.attach_client
        client
        (Embed
           { prog = Private.neovim_path
           ; args = [ "--headless"; "-n"; "--embed"; "--clean"; "--listen"; "./socket" ]
           ; working_dir
           ; env = `Extend [ "NVIM_RPLUGIN_MANIFEST", "rplugin.vim" ]
           })
        ~time_source:(Time_source.read_only (Time_source.create ~now:Time_ns.epoch ()))
      >>| ok_exn
    in
    let result = Command.exec client "quit" in
    let%bind exit_or_signal = Process.wait nvim in
    print_s [%message "nvim exited" (exit_or_signal : Unix.Exit_or_signal.t)];
    [%expect {| ("nvim exited" (exit_or_signal (Ok ()))) |}];
    let%bind result in
    print_s (omit_unstable_writer_info [%sexp (result : unit Or_error.t)]);
    [%expect
      {|
      (Error
       (("Consumer left without responding"
         (request
          (Array
           ((Int 0) (Int 4) (String nvim_cmd)
            (Array
             ((Map (((String cmd) (String quit))))
              (Map (((String output) (Bool false))))))))))
        (("Called from" lib/vcaml/test/semantics/test_nvim_crash.ml:LINE:COL))))
      |}];
    Dynamic.set_root Backtrace.elide false;
    Client.close client)
;;

let%expect_test "Crash during an RPC does not cause failure when sending response" =
  Expect_test_helpers_async.within_temp_dir (fun () ->
    Dynamic.set_root Backtrace.elide true;
    let%bind working_dir = Sys.getcwd () in
    let%bind client, nvim =
      let client = Client.create ~name:"test-client" ~on_error:`Raise in
      Private.attach_client
        client
        (Embed
           { prog = Private.neovim_path
           ; args = [ "--headless"; "-n"; "--embed"; "--clean"; "--listen"; "./socket" ]
           ; working_dir
           ; env = `Extend [ "NVIM_RPLUGIN_MANIFEST", "rplugin.vim" ]
           })
        ~time_source:(Time_source.read_only (Time_source.create ~now:Time_ns.epoch ()))
      >>| ok_exn
    in
    let entered = Ivar.create () in
    let exited = Ivar.create () in
    let result =
      block_nvim client ~f:(fun _ ->
        Ivar.fill_exn entered ();
        Ivar.read exited |> Deferred.ok)
    in
    let%bind () = Ivar.read entered in
    Process.send_signal nvim Signal.term;
    let%bind exit_or_signal = Process.wait nvim in
    print_s [%message "nvim exited" (exit_or_signal : Unix.Exit_or_signal.t)];
    (* Wait for Msgpack RPC to close the writer. *)
    let%bind () = Scheduler.yield_until_no_jobs_remain () in
    [%expect {| ("nvim exited" (exit_or_signal (Error (Exit_non_zero 1)))) |}];
    Ivar.fill_exn exited ();
    let%bind result in
    print_s [%sexp (result : unit Or_error.t)];
    [%expect
      {|
      (Error
       (("Consumer left without responding"
         (request
          (Array
           ((Int 0) (Int 4) (String nvim_call_function)
            (Array ((String rpcrequest) (Array ((Int 1) (String anon_rpc__0)))))))))
        (("Called from" lib/vcaml/test/semantics/test_nvim_crash.ml:LINE:COL))))
      |}];
    Dynamic.set_root Backtrace.elide false;
    Client.close client)
;;
