open Core
open Async
open Vcaml

let with_process_cleanup ~name pid ~f =
  let reap = ref (`Need_to_reap `Impatient) in
  let print_and_return exit_or_signal =
    print_s [%message (sprintf "%s exited" name) (exit_or_signal : Unix.Exit_or_signal.t)];
    return ()
  in
  Monitor.protect
    (fun () -> Deferred.map (f ()) ~f:(Ref.( := ) reap))
    ~finally:(fun () ->
      match !reap with
      | `Already_reaped exit_or_signal -> print_and_return exit_or_signal
      | `Need_to_reap patience ->
        let waitpid = Unix.waitpid pid in
        let timeout =
          match patience with
          | `Impatient -> Time_float.Span.zero
          | `Patient -> Time_float.Span.of_int_sec 20
        in
        (match%bind with_timeout timeout waitpid with
         | `Result exit_or_signal -> print_and_return exit_or_signal
         | `Timeout ->
           Signal_unix.send_i Signal.term (`Pid pid);
           waitpid >>= print_and_return))
;;

let spin_until_nvim_creates_socket_file pid ~socket =
  Deferred.repeat_until_finished 1 (fun attempt ->
    match attempt with
    | 10000 -> return (`Finished `Socket_missing)
    | _ ->
      (match Core_unix.wait_nohang (`Pid pid) with
       | Some (_, exit_or_signal) -> return (`Finished (`Nvim_crashed exit_or_signal))
       | None ->
         (match%bind Sys.file_exists_exn socket with
          | true -> return (`Finished `Socket_created)
          | false ->
            let%bind () = Clock_ns.after Time_ns.Span.millisecond in
            return (`Repeat (attempt + 1)))))
;;

(* We use [writefile] to enable Neovim to communicate from an atomic context that it is
   about to enter a state after which it will not be able to communicate (e.g., because
   it is exiting or because it will be uninterruptible for some reason). This function
   lets our tests wait for Neovim to reach this point before proceeding. *)
let writefile file ~contents =
  Nvim.eval (sprintf "writefile(['%s'], '%s', 's')" contents file) ~result_type:Integer
  |> Api_call.map ~f:(function
    | Ok 0 -> Ok ()
    | Ok -1 ->
      Or_error.error_s
        [%message "[writefile]: write failed" (file : string) (contents : string)]
    | Ok error_code ->
      Or_error.error_s
        [%message
          "[writefile]: unknown error code"
            (error_code : int)
            (file : string)
            (contents : string)]
    | Error _ as error -> error)
;;

(* This function lets us attempt to exit Neovim cleanly. It uses [writefile] to confirm
   that we have reached the [quit] command. By trying to exit Neovim cleanly instead of
   just sending it SIGTERM we can distinguish between cases where Neovim is able to handle
   incoming commands successfully and cases where Neovim is unresponsive. *)
let attempt_to_quit ~tmp_dir ~client =
  let fifo = tmp_dir ^/ "quit" in
  let%bind () = Unix.mkfifo fifo in
  don't_wait_for
    (run_join
       [%here]
       client
       ([ writefile fifo ~contents:":q"; Nvim.command "quit" ]
        |> Api_call.Or_error.all_unit)
     |> Deferred.Or_error.ignore_m
     |> Deferred.Or_error.ok_exn);
  Deferred.any
    [ Clock_ns.after Time_ns.Span.second
    ; (let%bind reader = Reader.open_file fifo in
       let%bind (_ : string Reader.Read_result.t) = Reader.read_line reader in
       Reader.close reader)
    ]
;;
