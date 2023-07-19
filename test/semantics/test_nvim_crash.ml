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

let%expect_test "Asynchronous write failure is returned to outstanding requests" =
  Backtrace.elide := true;
  let%bind () =
    Expect_test_helpers_async.within_temp_dir (fun () ->
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
      (* Wait for Msgpack RPC to close the writer. *)
      let%bind () = Scheduler.yield_until_no_jobs_remain () in
      let write_after_termination = Command.exec [%here] client "echo" ~args:[ "'hi'" ] in
      let%bind result = write_after_termination in
      print_s (omit_unstable_writer_info [%sexp (result : unit Or_error.t)]);
      Client.close client)
  in
  [%expect
    {|
    ("nvim exited" (exit_or_signal (Error (Exit_non_zero 1))))
    (Error
     (("Failed to send Msgpack RPC message: writer is closed"
       (Array
        ((Int 0) (Int 3) (String nvim_cmd)
         (Array
          ((Map
            (((String args) (Array ((String 'hi'))))
             ((String cmd) (String echo))))
           (Map (((String output) (Bool false)))))))))
      (("Called from" lib/vcaml/test/semantics/test_nvim_crash.ml:LINE:COL)))) |}];
  Backtrace.elide := false;
  return ()
;;

let%expect_test "Crash during an RPC does not cause failure when sending response" =
  Expect_test_helpers_async.within_temp_dir (fun () ->
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
    let (_ : unit Deferred.Or_error.t) =
      block_nvim [%here] client ~f:(fun _ ->
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
    (* Wait for RPC to attempt to return the response. *)
    let%bind () = Scheduler.yield_until_no_jobs_remain () in
    [%expect {||}];
    let%bind () = Client.close client in
    let%bind () = Scheduler.yield_until_no_jobs_remain () in
    [%expect {||}];
    return ())
;;
