open Core
open Async
open Vcaml

module Oneshot = struct
  let command =
    let bufnr =
      Vcaml_plugin.Oneshot.Rpc.create
        "bufnr"
        ~type_:Vcaml.Ocaml_from_nvim.Blocking.(return Buffer)
        ~f:(fun ~client -> Vcaml.Nvim.get_current_buf client)
    in
    Vcaml_plugin.Oneshot.create
      ~name:"vcaml-test-oneshot-plugin"
      ~description:"Test Oneshot RPC"
      [ bufnr ]
  ;;
end

module Persistent = struct
  let make_plugin ?on_startup ?(notify_fn = `Lua "OnStartup") ?async_rpc ?blocking_rpc () =
    let async_rpc =
      Vcaml_plugin.Persistent.Rpc.create_async
        "async-rpc"
        ~type_:Ocaml_from_nvim.Async.unit
        ~f:(fun () ~client:_ ->
          match async_rpc with
          | Some f -> f ()
          | None -> Deferred.Or_error.return ())
    in
    let blocking_rpc =
      Vcaml_plugin.Persistent.Rpc.create_blocking
        "blocking-rpc"
        ~type_:Ocaml_from_nvim.Blocking.(return Nil)
        ~f:(fun () ~run_in_background:_ ~client:_ ->
          match blocking_rpc with
          | Some f -> f ()
          | None -> Deferred.Or_error.return ())
    in
    let on_startup (_ : [ `asynchronous ] Client.t) =
      Dynamic.set_root Backtrace.elide true;
      match on_startup with
      | Some f -> f ()
      | None -> Deferred.Or_error.return ()
    in
    Vcaml_plugin.Persistent.create
      ~name:"vcaml-test-persistent-plugin"
      ~description:"Test plugin"
      ~on_startup
      ~notify_fn
      [ async_rpc; blocking_rpc ]
  ;;

  let default = make_plugin ()

  let on_startup_returns_error =
    make_plugin ~on_startup:(fun () -> Deferred.Or_error.error_string "Failure") ()
  ;;

  let on_startup_raises =
    make_plugin ~on_startup:(fun () -> failwith "Failure" [@nontail]) ()
  ;;

  let notify_fn_viml = make_plugin ~notify_fn:(`Viml "OnStartup") ()

  let async_rpc_returns_error =
    make_plugin ~async_rpc:(fun () -> Deferred.Or_error.error_string "Failure") ()
  ;;

  let async_rpc_raises =
    make_plugin ~async_rpc:(fun () -> failwith "Failure" [@nontail]) ()
  ;;

  let async_rpc_raises_after_returning =
    make_plugin
      ~async_rpc:(fun () ->
        Clock_ns.run_after
          (Time_ns.Span.of_int_ms 10)
          (fun () -> failwith "Failure" [@nontail])
          ();
        return (Ok ()))
      ()
  ;;

  let blocking_rpc_returns_error =
    make_plugin ~blocking_rpc:(fun () -> Deferred.Or_error.error_string "Failure") ()
  ;;

  let blocking_rpc_raises =
    make_plugin ~blocking_rpc:(fun () -> failwith "Failure" [@nontail]) ()
  ;;

  let blocking_rpc_raises_after_returning =
    make_plugin
      ~blocking_rpc:(fun () ->
        Clock_ns.run_after
          (Time_ns.Span.of_int_ms 10)
          (fun () -> failwith "Failure" [@nontail])
          ();
        return (Ok ()))
      ()
  ;;

  let command =
    Core.Command.group
      ~summary:"Test Plugin"
      [ "async-rpc-raises", async_rpc_raises
      ; "async-rpc-raises-after-returning", async_rpc_raises_after_returning
      ; "async-rpc-returns-error", async_rpc_returns_error
      ; "blocking-rpc-raises", blocking_rpc_raises
      ; "blocking-rpc-raises-after-returning", blocking_rpc_raises_after_returning
      ; "blocking-rpc-returns-error", blocking_rpc_returns_error
      ; "default", default
      ; "notify-fn-viml", notify_fn_viml
      ; "on-startup-raises", on_startup_raises
      ; "on-startup-returns-error", on_startup_returns_error
      ]
  ;;
end

let command =
  Core.Command.group
    ~summary:"Test Plugin"
    [ "oneshot", Oneshot.command; "persistent", Persistent.command ]
;;

let () = Command_unix.run command
