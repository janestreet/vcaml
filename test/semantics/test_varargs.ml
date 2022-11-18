open Core
open Async
open Vcaml
open Vcaml_test_helpers

let%expect_test "Varargs" =
  let vimscript =
    {| function! TestDispatcher(dispatcher)
         let results = []
         let x = a:dispatcher("abs", -1)
         let y = a:dispatcher("stridx", "hello, world", "world")
         return [ x, y ]
       endfunction
     |}
    |> String.split ~on:'\n'
  in
  let result =
    with_client (fun client ->
      Expect_test_helpers_async.with_temp_dir (fun tmp_dir ->
        let open Deferred.Or_error.Let_syntax in
        let test_dispatcher_file = tmp_dir ^/ "test_dispatcher.vim" in
        let%bind () =
          Nvim.command (sprintf "edit %s" test_dispatcher_file)
          |> run_join [%here] client
        in
        let%bind () =
          Buffer.set_lines
            Current
            ~start:0
            ~end_:(-1)
            ~strict_indexing:true
            ~replacement:vimscript
          |> run_join [%here] client
        in
        let%bind () = Nvim.command "write" |> run_join [%here] client in
        let%bind () = Nvim.command "source %" |> run_join [%here] client in
        let nvim_call_function ~keyboard_interrupted:_ ~client func args =
          wrap_viml_function
            ~type_:Defun.Vim.(String @-> Array Object @-> return Object)
            ~function_name:"nvim_call_function"
            func
            args
          |> run_join [%here] client
        in
        register_request_blocking
          client
          ~name:"call"
          ~type_:
            Defun.Ocaml.Sync.(
              String @-> Expert.varargs ~args_type:Object ~return_type:Object)
          ~f:nvim_call_function;
        let expr =
          sprintf
            !"TestDispatcher(function(\"rpcrequest\", [ %d, \"call\" ]))"
            (Client.channel client)
        in
        Nvim.eval expr ~result_type:(Array Integer) |> run_join [%here] client))
  in
  let%bind result = with_timeout (Time_float.Span.of_int_sec 3) result in
  print_s [%sexp (result : [ `Result of int list | `Timeout ])];
  [%expect {| (Result (1 7)) |}];
  return ()
;;

let%expect_test "Varargs (async)" =
  let vimscript =
    {| function! TestPrinter(print)
         call a:print("abs", -1)
         call a:print("stridx", "hello, world", "world")
         return "Done!"
       endfunction
     |}
    |> String.split ~on:'\n'
  in
  let printed = Mvar.create () in
  let result =
    with_client (fun client ->
      Expect_test_helpers_async.with_temp_dir (fun tmp_dir ->
        let open Deferred.Or_error.Let_syntax in
        let test_printer_file = tmp_dir ^/ "test_printer.vim" in
        let%bind () =
          Nvim.command (sprintf "edit %s" test_printer_file)
          |> run_join [%here] client
        in
        let%bind () =
          Buffer.set_lines
            Current
            ~start:0
            ~end_:(-1)
            ~strict_indexing:true
            ~replacement:vimscript
          |> run_join [%here] client
        in
        let%bind () = Nvim.command "write" |> run_join [%here] client in
        let%bind () = Nvim.command "source %" |> run_join [%here] client in
        register_request_async
          client
          ~name:"print"
          ~type_:Defun.Ocaml.Async.(String @-> Expert.varargs Object)
          ~f:(fun ~client:_ name args ->
            print_s [%message (name : string) (args : Msgpack.t list)];
            Mvar.put printed () |> Deferred.ok);
        let expr =
          sprintf
            !"TestPrinter(function(\"rpcnotify\", [ %d, \"print\" ]))"
            (Client.channel client)
        in
        Nvim.eval expr ~result_type:String |> run_join [%here] client))
  in
  let%bind result =
    with_timeout
      (Time_float.Span.of_int_sec 3)
      (let%bind () = Mvar.take printed in
       let%bind () = Mvar.take printed in
       result)
  in
  print_s [%sexp (result : [ `Result of string | `Timeout ])];
  [%expect
    {|
    ((name abs) (args ((Integer -1))))
    ((name stridx) (args ((String "hello, world") (String world))))
    (Result Done!) |}];
  return ()
;;
