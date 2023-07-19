open Core
open Async
open Async_unix

type t =
  { tmpdir : string
  ; nvim : Pid.t
  }

let socket ~tmpdir = tmpdir ^/ "socket"

let setup () =
  Thread_safe.block_on_async_exn (fun () ->
    let%bind tmpdir = Unix.mkdtemp "/tmp/vcaml" in
    let socket = socket ~tmpdir in
    let%bind nvim =
      Process.create_exn
        ()
        ~prog:Vcaml_test_helpers.Private.neovim_path
        ~args:[ "-n"; "-i"; "NONE"; "--headless"; "--listen"; socket ]
      >>| Process.pid
    in
    let%bind () =
      Deferred.repeat_until_finished 0 (fun n ->
        match%bind Sys.file_exists_exn socket with
        | true -> return (`Finished ())
        | false ->
          (match n with
           | 400 -> failwith "Neovim did not create a socket file"
           | _ ->
             let%map () = Clock.after (Time_float.Span.of_int_ms 25) in
             `Repeat (n + 1)))
    in
    return { tmpdir; nvim })
;;

let socket { tmpdir; _ } = socket ~tmpdir

let teardown t =
  Thread_safe.block_on_async_exn (fun () ->
    let exit_status = Unix.waitpid t.nvim in
    let%bind () =
      match%map with_timeout Time_float.Span.second exit_status with
      | `Result _ -> ()
      | `Timeout -> Signal_unix.send_exn Signal.kill (`Pid t.nvim)
    in
    match%bind exit_status with
    | Ok () -> Unix.rmdir t.tmpdir
    | Error error ->
      raise_s
        [%message "Neovim exited with error" ~_:(error : Core_unix.Exit_or_signal.error)])
;;
