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
  let test ~close_reader_and_writer_on_disconnect =
    Expect_test_helpers_async.within_temp_dir (fun () ->
      let%bind working_dir = Sys.getcwd () in
      let%bind client, nvim =
        let client = Client.create ~on_error:`Raise in
        Client.attach
          client
          (Embed
             { prog = neovim_path
             ; args = [ "--headless"; "-n"; "--embed"; "--clean"; "--listen"; "./socket" ]
             ; working_dir
             ; env = `Extend [ "NVIM_RPLUGIN_MANIFEST", "rplugin.vim" ]
             })
          ~close_reader_and_writer_on_disconnect
          ~time_source:(Time_source.read_only (Time_source.create ~now:Time_ns.epoch ()))
        >>| ok_exn
      in
      Process.send_signal nvim Signal.term;
      let%bind exit_or_signal = Process.wait nvim in
      print_s [%message "nvim exited" (exit_or_signal : Unix.Exit_or_signal.t)];
      (* Wait for Msgpack RPC to close the writer after disconnect if
         [close_reader_and_writer_on_disconnect] is set. *)
      let%bind () = Scheduler.yield_until_no_jobs_remain () in
      let write_after_termination = run_join [%here] client (Nvim.command "echo 'hi'") in
      let%bind result = write_after_termination in
      print_s (omit_unstable_writer_info [%sexp (result : unit Or_error.t)]);
      Client.close client)
  in
  let%bind () = test ~close_reader_and_writer_on_disconnect:true in
  [%expect
    {|
    ("nvim exited" (exit_or_signal (Error (Exit_non_zero 1))))
    (Error
     (("Called from" lib/vcaml/test/semantics/test_nvim_crash.ml:LINE:COL)
      ("Msgpack error response"
       (String
        ("Failed to send Msgpack RPC request: writer is closed"
         (method_name nvim_command) (parameters ((String "echo 'hi'")))))))) |}];
  let%bind () = test ~close_reader_and_writer_on_disconnect:false in
  [%expect
    {|
    ("nvim exited" (exit_or_signal (Error (Exit_non_zero 1))))
    (Error
     (("Called from" lib/vcaml/test/semantics/test_nvim_crash.ml:LINE:COL)
      ("Msgpack error response"
       (String
        ("Writer error from inner_monitor"
         (Unix.Unix_error "Broken pipe" writev_assume_fd_is_nonblocking "")
         (writer ((file_descr _) (info <omitted>) (kind Fifo)))))))) |}];
  Backtrace.elide := false;
  return ()
;;
