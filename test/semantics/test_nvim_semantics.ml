open Core
open Async
open Import
open Vcaml
open Vcaml_test_helpers

(* The tests in this file demonstrate properties of Neovim's own semantics. VCaml makes
   assumptions about how Neovim operates, so if these fail that most likely indicates that
   a change to VCaml is necessary. *)

let hundred_ms = Time_ns.Span.create ~ms:100 ()

let%expect_test "[rpcrequest] blocks other channels" =
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
          let block_nvim ~(here : [%call_pos]) client =
            let blocking = Ivar.create () in
            let result = Ivar.create () in
            let result_deferred =
              block_nvim ~here client ~f:(fun _ ->
                Ivar.fill_exn blocking ();
                Ivar.read result |> Deferred.ok)
            in
            let%map () = Ivar.read blocking in
            fun response ->
              Ivar.fill_exn result response;
              result_deferred
          in
          let%bind client1 = socket_client socket >>| ok_exn in
          let%bind client2 = socket_client socket >>| ok_exn in
          let print_when_client2_is_unblocked () =
            don't_wait_for
              (let%map result =
                 Nvim.eval_viml_expression
                   client2
                   "'Client 2 is unblocked'"
                   ~result_type:String
               in
               print_s [%sexp (result : string Or_error.t)])
          in
          print_when_client2_is_unblocked ();
          let%bind () = Clock_ns.after hundred_ms in
          let%bind () = Scheduler.yield_until_no_jobs_remain () in
          print_s [%message "Blocking nvim (client1)"];
          let%bind respond_to_rpc = block_nvim client1 in
          print_when_client2_is_unblocked ();
          let%bind () = Clock_ns.after hundred_ms in
          let%bind () = Scheduler.yield_until_no_jobs_remain () in
          print_s [%message "Unblocking nvim (client1)"];
          let%bind () = respond_to_rpc () >>| ok_exn in
          let%bind () = Clock_ns.after hundred_ms in
          let%bind () = Scheduler.yield_until_no_jobs_remain () in
          let%bind () = attempt_to_quit ~tmp_dir ~client:client2 in
          let%bind () = Client.close client1 in
          let%bind () = Client.close client2 in
          return (`Need_to_reap `Patient))
    in
    [%expect
      {|
      (Ok "Client 2 is unblocked")
      "Blocking nvim (client1)"
      "Unblocking nvim (client1)"
      (Ok "Client 2 is unblocked")
      ("nvim exited" (exit_or_signal (Ok ())))
      |}];
    return ())
;;

let%expect_test "A -[rpcrequest]-> Neovim -[rpcrequest]-> B lets B communicate with \
                 Neovim until it returns from the original request, at which point \
                 control returns to A"
  =
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
          let%bind client1 = socket_client socket >>| ok_exn in
          let%bind client2 = socket_client socket >>| ok_exn in
          let client2_received_rpc = Ivar.create () in
          let tried_to_use_client1 = Ivar.create () in
          Ocaml_from_nvim.register_request_blocking
            (Connected client2)
            ~name:"rpc"
            ~type_:Ocaml_from_nvim.Blocking.(return Nil)
            ~f:(fun ~run_in_background:_ ~client ->
              Ivar.fill_exn client2_received_rpc ();
              let%bind (_ : Buffer.t) = Nvim.get_current_buf client >>| ok_exn in
              let%bind () = Ivar.read tried_to_use_client1 in
              Deferred.Or_error.return ());
          let () =
            Nvim.call_function
              client1
              ~name:(`Lua "vim.rpcrequest")
              ~type_:Nvim.Func.(Int @-> String @-> return Nil)
              (Client.channel client2)
              "rpc"
            >>| ok_exn
            |> don't_wait_for
          in
          let%bind () = Ivar.read client2_received_rpc in
          let print_client1_is_blocked =
            let deferred = Nvim.get_current_buf client1 >>| ok_exn in
            fun () ->
              let%bind () = Clock_ns.after hundred_ms in
              let%map () = Scheduler.yield_until_no_jobs_remain () in
              printf "%b\n" (not (Deferred.is_determined deferred))
          in
          let%bind () = print_client1_is_blocked () in
          [%expect {| true |}];
          Ivar.fill_exn tried_to_use_client1 ();
          let%bind () = print_client1_is_blocked () in
          [%expect {| false |}];
          let%bind () = attempt_to_quit ~tmp_dir ~client:client1 in
          let%bind () = Client.close client1 in
          let%bind () = Client.close client2 in
          return (`Need_to_reap `Patient))
    in
    [%expect {| ("nvim exited" (exit_or_signal (Ok ()))) |}];
    return ())
;;

let%expect_test "Plugin dying during [rpcrequest] does not bring down Neovim" =
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
          let block_nvim ~(here : [%call_pos]) client =
            let blocking = Ivar.create () in
            don't_wait_for
              (block_nvim ~here client ~f:(fun _ ->
                 Ivar.fill_exn blocking ();
                 Deferred.never ())
               >>| ok_exn);
            Ivar.read blocking
          in
          (match Core_unix.fork () with
           | `In_the_child ->
             Scheduler.reset_in_forked_process ();
             don't_wait_for
               (let%bind client = socket_client socket >>| ok_exn in
                let%bind () = block_nvim client in
                (* We don't want to allow the [at_exit] handler that expect test collector
                   registers to run for this child process. *)
                Core_unix.exit_immediately 0);
             never_returns (Scheduler.go ())
           | `In_the_parent child ->
             let%bind exit_or_signal = Unix.waitpid child in
             print_s [%message "child exited" (exit_or_signal : Unix.Exit_or_signal.t)];
             let%bind client = socket_client socket >>| ok_exn in
             let%bind result =
               Nvim.eval_viml_expression
                 client
                 "'nvim is still running'"
                 ~result_type:String
             in
             print_s [%sexp (result : string Or_error.t)];
             let%bind () = attempt_to_quit ~tmp_dir ~client in
             let%bind () = Client.close client in
             return (`Need_to_reap `Patient)))
    in
    [%expect
      {|
      ("child exited" (exit_or_signal (Ok ())))
      (Ok "nvim is still running")
      ("nvim exited" (exit_or_signal (Ok ())))
      |}];
    return ())
;;

let%expect_test "Nested [rpcrequest]s are supported" =
  let result =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let factorial ~(here : [%call_pos]) client =
        Nvim.call_function
          ~here
          client
          ~name:(`Viml "rpcrequest")
          ~type_:Nvim.Func.(Int @-> String @-> Int @-> return Int)
          (Client.channel client)
          "factorial"
      in
      Ocaml_from_nvim.register_request_blocking
        (Connected client)
        ~name:"factorial"
        ~type_:Ocaml_from_nvim.Blocking.(Int @-> return Int)
        ~f:(fun ~run_in_background:_ ~client n ->
          match n with
          | 0 -> return 1
          | _ ->
            let%map result = factorial client (n - 1) in
            n * result);
      factorial client 5)
  in
  let%bind result = with_timeout (Time_float.Span.of_int_sec 3) result in
  print_s [%sexp (result : [ `Result of int | `Timeout ])];
  [%expect {| (Result 120) |}];
  return ()
;;
