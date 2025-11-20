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
    | 10000 -> failwith "Socket was not created"
    | _ ->
      (match Core_unix.wait_nohang (`Pid pid) with
       | Some (_, exit_or_signal) -> return (`Finished (`Nvim_crashed exit_or_signal))
       | None ->
         (match%bind Sys.file_exists_exn socket with
          | true ->
            (* Test that we can establish a connection. Raises on timeout. *)
            Monitor.try_with ~extract_exn:true (fun () ->
              Tcp.connect_sock (Tcp.Where_to_connect.of_unix_address (`Unix socket)))
            >>| (function
             | Ok socket ->
               Socket.shutdown socket `Both;
               `Finished `Socket_created
             | Error (Unix.Unix_error (ECONNREFUSED, _, _)) -> `Repeat (attempt + 1)
             | Error exn -> raise exn)
          | false ->
            let%bind () = Clock_ns.after Time_ns.Span.millisecond in
            return (`Repeat (attempt + 1)))))
;;

(* We use [writefile] to enable Neovim to communicate from an atomic context that it is
   about to enter a state after which it will not be able to communicate (e.g., because it
   is exiting or because it will be uninterruptible for some reason). This function lets
   our tests wait for Neovim to reach this point before proceeding. *)
let writefile ~(here : [%call_pos]) client file ~contents ~then_do =
  let open Expert in
  let writefile =
    Atomic.T
      (Nvim_internal.nvim_eval
         ~expr:(sprintf "writefile(['%s'], '%s', 's')" contents file))
  in
  match%map Atomic.run ~here client (writefile :: then_do) with
  | Error error -> Error (Atomic.Error.to_error error)
  | Ok (Int result :: _) ->
    (match result with
     | 0 -> Ok ()
     | -1 ->
       Or_error.error_s
         [%message "[writefile]: write failed" (file : string) (contents : string)]
     | error_code ->
       Or_error.error_s
         [%message
           "[writefile]: unknown error code"
             (error_code : int)
             (file : string)
             (contents : string)])
  | Ok response ->
    Error (Atomic.Error.to_error (Unexpected_format (Array response), here))
;;

(* This function lets us attempt to exit Neovim cleanly. It uses [writefile] to confirm
   that we have reached the [quit] command. By trying to exit Neovim cleanly instead of
   just sending it SIGTERM we can distinguish between cases where Neovim is able to handle
   incoming commands successfully and cases where Neovim is unresponsive. *)
let attempt_to_quit ~tmp_dir ~client =
  let fifo = tmp_dir ^/ "quit" in
  let%bind () = Unix.mkfifo fifo in
  don't_wait_for
    (writefile
       client
       fifo
       ~contents:":q"
       ~then_do:
         [ T
             (Private.Nvim_internal.nvim_cmd
                ~cmd:(String.Map.singleton "cmd" (Msgpack.String "quit"))
                ~opts:String.Map.empty)
         ]
     |> Deferred.ignore_m);
  Deferred.any
    [ Clock_ns.after Time_ns.Span.second
    ; (let%bind reader = Reader.open_file fifo in
       let%bind (_ : string Reader.Read_result.t) = Reader.read_line reader in
       Reader.close reader)
    ]
;;
