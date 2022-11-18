open Core
open Async
open Import
open Vcaml
open Vcaml_test_helpers

(* [Child] clients are used by oneshot plugins. *)

let%expect_test "Simple test of [Child] client" =
  Expect_test_helpers_async.with_temp_dir (fun tmp_dir ->
    let socket = tmp_dir ^/ "socket" in
    let%bind nvim =
      (* There is some undocumented internal limit for the socket length (it doesn't
         appear in `:h limits`) so to ensure we create a socket we set the working dir
         to [tmp_dir] and create the socket with a relative path. *)
      Process.create_exn
        ()
        ~working_dir:tmp_dir
        ~prog:neovim_path
        ~args:[ "--headless"; "-n"; "--clean"; "--listen"; "./socket" ]
        ~env:(`Extend [ "NVIM_RPLUGIN_MANIFEST", "rplugin.vim" ])
    in
    let%bind () =
      with_process_cleanup ~name:"nvim" (Process.pid nvim) ~f:(fun () ->
        match%bind spin_until_nvim_creates_socket_file (Process.pid nvim) ~socket with
        | `Nvim_crashed exit_or_signal -> return (`Already_reaped exit_or_signal)
        | `Socket_missing -> raise_s [%message "Socket was not created"]
        | `Socket_created ->
          let saved_stdin = Core_unix.dup Core_unix.stdin in
          let saved_stdout = Core_unix.dup Core_unix.stdout in
          let socketfd =
            Core_unix.socket ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0 ()
          in
          Core_unix.connect socketfd ~addr:(ADDR_UNIX socket);
          Core_unix.dup2 ~src:socketfd ~dst:Core_unix.stdin ();
          Core_unix.dup2 ~src:socketfd ~dst:Core_unix.stdout ();
          let test () =
            let open Deferred.Or_error.Let_syntax in
            let%bind client =
              let client = Client.create ~on_error:`Raise in
              (* We don't close the reader and writer on disconnect because we want
                 [Reader.stdin] and [Writer.stdout] to continue to work after restoring
                 the original stdin and stdout. *)
              Client.attach
                ~close_reader_and_writer_on_disconnect:false
                ~time_source:
                  (Time_source.read_only (Time_source.create ~now:Time_ns.epoch ()))
                client
                Stdio
            in
            let%bind result =
              run_join
                [%here]
                client
                (Nvim.eval "'Hello, world!'" ~result_type:String)
            in
            let%map () = attempt_to_quit ~tmp_dir ~client |> Deferred.ok in
            result
          in
          let%bind result = Deferred.Or_error.try_with test >>| Or_error.join in
          Core_unix.dup2 ~src:saved_stdin ~dst:Core_unix.stdin ();
          Core_unix.dup2 ~src:saved_stdout ~dst:Core_unix.stdout ();
          Core_unix.close saved_stdin;
          Core_unix.close saved_stdout;
          Core_unix.close socketfd;
          print_s [%sexp (result : string Or_error.t)];
          return (`Need_to_reap `Patient))
    in
    [%expect
      {|
      (Ok "Hello, world!")
      ("nvim exited" (exit_or_signal (Ok ()))) |}];
    return ())
;;
