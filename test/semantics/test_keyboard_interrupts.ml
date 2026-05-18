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

let run_neovim_with_pty ?verbose ~time_source f =
  let pty_master = posix_openpt [ O_RDWR; O_NOCTTY ] in
  grantpt pty_master;
  unlockpt pty_master;
  Expect_test_helpers_async.with_temp_dir (fun tmp_dir ->
    let socket = tmp_dir ^/ "socket" in
    let pty_slave_path = ptsname pty_master in
    let prog = Private.neovim_path in
    (* There is some undocumented internal limit for the socket length (it doesn't appear
       in `:h limits`) so to ensure we create a socket we set the working dir to [tmp_dir]
       and create the socket with a relative path. *)
    (* We do *not* want to run with --headless here. *)
    let nvim =
      Core_unix.fork_exec
        ~prog
        ~argv:[ prog; "-n"; "--clean"; "--listen"; "./socket" ]
        ~env:(`Extend [ "NVIM_RPLUGIN_MANIFEST", "rplugin.vim" ])
        ~preexec:
          [ Setsid ()
          ; Fd_open
              { fd = Core_unix.stdin
              ; filename = pty_slave_path
              ; flags = [ O_RDWR ]
              ; perm = 0
              }
          ; Fd_dup2 { src = Core_unix.stdin; dst = Core_unix.stdout }
          ; Fd_dup2 { src = Core_unix.stdout; dst = Core_unix.stderr }
          ; Chdir tmp_dir
          ]
        ()
    in
    with_process_cleanup ~name:"nvim" nvim ~f:(fun () ->
      match%bind spin_until_nvim_creates_socket_file nvim ~socket with
      | `Nvim_crashed exit_or_signal -> return (`Already_reaped exit_or_signal)
      | `Socket_created ->
        let%bind client = socket_client socket ?verbose ?time_source >>| ok_exn in
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

let test_timeout_secs = 10

let%expect_test "Keyboard interrupt aborts simple RPC request" =
  Dynamic.set_root Backtrace.elide true;
  let%bind () =
    run_neovim_with_pty
      ~verbose:false
      ~time_source:None
      (fun ~tmp_dir ~client ~send_keys ->
         let fifo = tmp_dir ^/ "fifo" in
         let%bind () = Unix.mkfifo fifo in
         let sleep_time = Int.to_string test_timeout_secs in
         let sleep =
           writefile
             client
             fifo
             ~contents:"Sleeping"
             ~then_do:
               [ T
                   (Private.Nvim_internal.nvim_cmd
                      ~cmd:
                        ([ "cmd", Msgpack.String "sleep"
                         ; "args", Array [ String sleep_time ]
                         ]
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
      (("One of the calls in the Atomic.run batch failed"
        (partial_results ((Int 0))) (index_of_failure 1) (error_type Exception)
        "Keyboard interrupt")
       (("Called from"
         lib/vcaml/test/semantics/test_keyboard_interrupts.ml:LINE:COL)))))
    ("nvim exited" (exit_or_signal (Ok ())))
    |}];
  Dynamic.set_root Backtrace.elide false;
  return ()
;;

let on_keyboard_interrupt_abort_rpcrequest_and_notify_callback
  ?verbose
  ?(timeout = Time_float.Span.of_int_sec test_timeout_secs)
  ~time_source
  f
  =
  run_neovim_with_pty ?verbose ~time_source (fun ~tmp_dir ~client ~send_keys ->
    let interrupt_every_second () =
      Deferred.repeat_until_finished 0 (fun repeats ->
        send_keys "\003";
        let%bind () =
          Time_source.after (Time_source.wall_clock ()) (Time_ns.Span.of_int_sec 1)
        in
        if repeats >= test_timeout_secs
        then return (`Finished ())
        else return (`Repeat (repeats + 1)))
    in
    let rpc_result =
      let%bind rpc_result =
        block_nvim client ~f:(fun client ->
          let (_ : unit Deferred.t) = interrupt_every_second () in
          f client)
      in
      let%bind () = attempt_to_quit ~tmp_dir ~client in
      return rpc_result
    in
    let%map rpc_result = with_timeout timeout rpc_result in
    print_s [%message (rpc_result : unit Or_error.t Clock_ns.Or_timeout.t)];
    match rpc_result with
    | `Timeout -> `Still_running
    | `Result _ -> `Closed)
;;

let%expect_test "Keyboard interrupt learned by RPC response aborts [rpcrequest]" =
  Dynamic.set_root Backtrace.elide true;
  let%bind () =
    on_keyboard_interrupt_abort_rpcrequest_and_notify_callback
      ~verbose:false
      ~timeout:(Time_float.Span.of_sec 5.0)
      ~time_source:None
      (fun client ->
         let sleep_time = Int.to_string test_timeout_secs in
         let sleep_and_print_result ~(here : [%call_pos]) () =
           Command.exec ~here client "sleep" ~args:[ sleep_time ]
           >>| [%sexp_of: unit Or_error.t]
           >>| print_s
         in
         (* Sleep to make sure that this command will see the Ctrl-C that was sent. This
            works even if it was sent before the sleep began because Neovim has not yet
            had an opportunity to communicate the interrupt. *)
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
     (Result
      (Error
       (("Vim returned error" "Keyboard interrupt" (error_type Exception))
        (("Called from"
          lib/vcaml/test/semantics/test_keyboard_interrupts.ml:LINE:COL))))))
    ("nvim exited" (exit_or_signal (Ok ())))
    |}];
  Dynamic.set_root Backtrace.elide false;
  return ()
;;

let%expect_test "Keyboard interrupt learned by heartbeating aborts [rpcrequest]" =
  Dynamic.set_root Backtrace.elide true;
  let%bind () =
    on_keyboard_interrupt_abort_rpcrequest_and_notify_callback
      ~verbose:false
      ~timeout:(Time_float.Span.of_sec 5.0)
      ~time_source:(Some (Time_source.wall_clock ()))
      (fun _ -> Deferred.never () |> Deferred.ok)
  in
  [%expect
    {|
    (rpc_result
     (Result
      (Error
       (("Vim returned error" "Keyboard interrupt" (error_type Exception))
        (("Called from"
          lib/vcaml/test/semantics/test_keyboard_interrupts.ml:LINE:COL))))))
    ("nvim exited" (exit_or_signal (Ok ())))
    |}];
  Dynamic.set_root Backtrace.elide false;
  return ()
;;

let%expect_test "Keyboard interrupt learned by ??? - Neovim's semantics have changed!" =
  let%bind () =
    on_keyboard_interrupt_abort_rpcrequest_and_notify_callback
      ~timeout:Time_float.Span.second
      ~time_source:None
      (fun _ -> Deferred.never () |> Deferred.ok)
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
