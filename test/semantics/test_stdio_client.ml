open Core
open Async
open Import
open Vcaml
open Vcaml_test_helpers

(* [Stdio] clients are used by oneshot plugins. *)

let%expect_test "Simple test of [Stdio] client" =
  Expect_test_helpers_async.with_temp_dir (fun tmp_dir ->
    let socket = tmp_dir ^/ "socket" in
    let%bind nvim =
      (* There is some undocumented internal limit for the socket length (it doesn't
         appear in `:h limits`) so to ensure we create a socket we set the working dir to
         [tmp_dir] and create the socket with a relative path. *)
      Process.create_exn
        ()
        ~working_dir:tmp_dir
        ~prog:Private.neovim_path
        ~args:[ "--headless"; "-n"; "--clean"; "--listen"; "./socket" ]
        ~env:(`Extend [ "NVIM_RPLUGIN_MANIFEST", "rplugin.vim" ])
    in
    let%bind () =
      with_process_cleanup ~name:"nvim" (Process.pid nvim) ~f:(fun () ->
        match%bind spin_until_nvim_creates_socket_file (Process.pid nvim) ~socket with
        | `Nvim_crashed exit_or_signal -> return (`Already_reaped exit_or_signal)
        | `Socket_created ->
          let test () =
            Tcp.with_connection
              (Tcp.Where_to_connect.of_unix_address (`Unix socket))
              (fun (_ : _ Socket.t) reader writer ->
                let open Deferred.Or_error.Let_syntax in
                let%bind client =
                  let client = Client.create ~name:"buffer-clock" ~on_error:`Raise in
                  Private.attach_client
                    ~stdio_override:(reader, writer)
                    ~time_source:
                      (Time_source.read_only (Time_source.create ~now:Time_ns.epoch ()))
                    client
                    Stdio
                in
                let%bind result =
                  Nvim.eval_viml_expression client "'Hello, world!'" ~result_type:String
                in
                let%map () = attempt_to_quit ~tmp_dir ~client |> Deferred.ok in
                result)
          in
          let%bind result = Deferred.Or_error.try_with_join test in
          print_s [%sexp (result : string Or_error.t)];
          return (`Need_to_reap `Patient))
    in
    [%expect
      {|
      (Ok "Hello, world!")
      ("nvim exited" (exit_or_signal (Ok ())))
      |}];
    return ())
;;
