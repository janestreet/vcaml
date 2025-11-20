open Core
open Async
open Import
open Vcaml
open Vcaml_test_helpers

[%%import "config_ext.h"]
[%%if defined JSC_LINUX_EXT && defined JSC_UNIX_PTY]

let posix_openpt = ok_exn Unix_pseudo_terminal.posix_openpt
let grantpt = ok_exn Unix_pseudo_terminal.grantpt
let unlockpt = ok_exn Unix_pseudo_terminal.unlockpt
let ptsname = ok_exn Unix_pseudo_terminal.ptsname

let run_neovim_with_pty ~time_source ~f =
  let pty_master = posix_openpt [ O_RDWR; O_NOCTTY ] in
  grantpt pty_master;
  unlockpt pty_master;
  Expect_test_helpers_async.with_temp_dir (fun tmp_dir ->
    let socket = tmp_dir ^/ "socket" in
    match Core_unix.fork () with
    | `In_the_child ->
      let (_ : int) = Core_unix.Terminal_io.setsid () in
      let pty_slave = Core_unix.openfile ~mode:[ O_RDWR ] (ptsname pty_master) in
      Core_unix.dup2 ~src:pty_slave ~dst:Core_unix.stdin ();
      Core_unix.dup2 ~src:pty_slave ~dst:Core_unix.stdout ();
      Core_unix.dup2 ~src:pty_slave ~dst:Core_unix.stderr ();
      Core_unix.close pty_slave;
      (* There is some undocumented internal limit for the socket length (it doesn't
         appear in `:h limits`) so to ensure we create a socket we set the working dir to
         [tmp_dir] and create the socket with a relative path. *)
      Core_unix.chdir tmp_dir;
      let prog = Private.neovim_path in
      (* We do *not* want to run with --headless here. *)
      Core_unix.exec
        ()
        ~prog
        ~argv:[ prog; "-n"; "--clean"; "--listen"; "./socket" ]
        ~env:(`Extend [ "NVIM_RPLUGIN_MANIFEST", "rplugin.vim" ])
      |> never_returns
    | `In_the_parent nvim ->
      with_process_cleanup ~name:"nvim" nvim ~f:(fun () ->
        match%bind spin_until_nvim_creates_socket_file nvim ~socket with
        | `Nvim_crashed exit_or_signal -> return (`Already_reaped exit_or_signal)
        | `Socket_created ->
          let%bind client = socket_client socket ?time_source >>| ok_exn in
          let send_keys bytes =
            let buf = Bytes.of_string bytes in
            let bytes_written = Core_unix.single_write pty_master ~buf in
            assert (String.length bytes = bytes_written)
          in
          let%bind result = f ~tmp_dir ~client ~send_keys in
          let%map () = Client.close client in
          (match result with
           | `Closed -> `Need_to_reap `Patient
           | `Still_running -> `Need_to_reap `Impatient)))
;;

let%expect_test "Keyboard interrupt aborts simple RPC request" =
  Dynamic.set_root Backtrace.elide true;
  let%bind () =
    run_neovim_with_pty ~time_source:None ~f:(fun ~tmp_dir ~client ~send_keys ->
      let fifo = tmp_dir ^/ "fifo" in
      let%bind () = Unix.mkfifo fifo in
      let sleep =
        writefile
          client
          fifo
          ~contents:"Sleeping"
          ~then_do:
            [ T
                (Private.Nvim_internal.nvim_cmd
                   ~cmd:
                     ([ "cmd", Msgpack.String "sleep"; "args", Array [ String "100" ] ]
                      |> String.Map.of_alist_exn)
                   ~opts:String.Map.empty)
            ]
      in
      let%bind reader = Reader.open_file fifo in
      let%bind message = Reader.read_line reader in
      print_s [%sexp (message : string Reader.Read_result.t)];
      send_keys "\003";
      let%bind sleep in
      print_s [%message (sleep : unit Or_error.t)];
      let%bind () = attempt_to_quit ~tmp_dir ~client in
      return `Closed)
  in
  [%expect
    {|
    (Ok Sleeping)
    (sleep
     (Error
      (("One of the calls in the nvim_call_atomic batch failed"
        (partial_results ((Int 0))) (index_of_failure 1) (error_type Exception)
        "Keyboard interrupt")
       (("Called from"
         lib/vcaml/test/semantics/test_keyboard_interrupts.ml:LINE:COL)))))
    ("nvim exited" (exit_or_signal (Ok ())))
    |}];
  Dynamic.set_root Backtrace.elide false;
  return ()
;;

let on_keyboard_interrupt_abort_rpcrequest_and_notify_callback ~timeout ~time_source ~f =
  run_neovim_with_pty ~time_source ~f:(fun ~tmp_dir ~client ~send_keys ->
    let rpc_result =
      let%bind rpc_result =
        block_nvim client ~f:(fun client ->
          send_keys "\003";
          f client)
      in
      let%bind () = attempt_to_quit ~tmp_dir ~client in
      return rpc_result
    in
    match timeout with
    | None ->
      let%map rpc_result in
      print_s [%message (rpc_result : unit Or_error.t)];
      `Closed
    | Some timeout ->
      let%map rpc_result = with_timeout timeout rpc_result in
      print_s [%message (rpc_result : unit Or_error.t Clock_ns.Or_timeout.t)];
      (match rpc_result with
       | `Timeout -> `Still_running
       | `Result _ -> `Closed))
;;

let%expect_test "Keyboard interrupt learned by RPC response aborts [rpcrequest]" =
  Dynamic.set_root Backtrace.elide true;
  let%bind () =
    on_keyboard_interrupt_abort_rpcrequest_and_notify_callback
      ~timeout:None
      ~time_source:None
      ~f:(fun client ->
        let sleep_and_print_result ~(here : [%call_pos]) () =
          Command.exec ~here client "sleep" ~args:[ "100" ]
          >>| [%sexp_of: unit Or_error.t]
          >>| print_s
        in
        (* Sleep to make sure that this command will see the Ctrl-C that was sent. This
           works even if it was sent before the sleep began because Neovim has not yet had
           an opportunity to communicate the interrupt. *)
        let%bind () = sleep_and_print_result () in
        (* After a keyboard interrupt [client] should be rendered unusable, so we should
           not actually send the sleep command. *)
        let%bind () = sleep_and_print_result () in
        return (Ok ()))
  in
  [%expect
    {|
    (Error
     (("Vim returned error" "Keyboard interrupt" (error_type Exception))
      (("Called from"
        lib/vcaml/test/semantics/test_keyboard_interrupts.ml:LINE:COL))))
    (Error
     ("Keyboard interrupt"
      (("Called from"
        lib/vcaml/test/semantics/test_keyboard_interrupts.ml:LINE:COL))))
    (rpc_result
     (Error
      (("Vim returned error" "Keyboard interrupt" (error_type Exception))
       (("Called from"
         lib/vcaml/test/semantics/test_keyboard_interrupts.ml:LINE:COL)))))
    ("nvim exited" (exit_or_signal (Ok ())))
    |}];
  Dynamic.set_root Backtrace.elide false;
  return ()
;;

let%expect_test "Keyboard interrupt learned by heartbeating aborts [rpcrequest]" =
  Dynamic.set_root Backtrace.elide true;
  let%bind () =
    on_keyboard_interrupt_abort_rpcrequest_and_notify_callback
      ~timeout:None
      ~time_source:(Some (Time_source.wall_clock ()))
      ~f:(fun _ -> Deferred.never () |> Deferred.ok)
  in
  [%expect
    {|
    (rpc_result
     (Error
      (("Vim returned error" "Keyboard interrupt" (error_type Exception))
       (("Called from"
         lib/vcaml/test/semantics/test_keyboard_interrupts.ml:LINE:COL)))))
    ("nvim exited" (exit_or_signal (Ok ())))
    |}];
  Dynamic.set_root Backtrace.elide false;
  return ()
;;

let%expect_test "Keyboard interrupt learned by ??? - Neovim's semantics have changed!" =
  let%bind () =
    on_keyboard_interrupt_abort_rpcrequest_and_notify_callback
      ~timeout:(Some Time_float.Span.second)
      ~time_source:None
      ~f:(fun _ -> Deferred.never () |> Deferred.ok)
  in
  (* If this test succeeds then Neovim's semantics around when it alerts have changed. We
     should investigate - heartbeating may no longer be required. *)
  [%expect
    {|
    (rpc_result Timeout)
    ("nvim exited" (exit_or_signal (Error (Exit_non_zero 1))))
    |}];
  return ()
;;

[%%endif]
