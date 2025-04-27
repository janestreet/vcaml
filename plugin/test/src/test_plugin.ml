open Core
open Async
open Vcaml
open Vcaml_test_helpers

let links = [ "../bin/main.exe", `In_temp_as, "main.exe" ]
let with_ui_client = with_ui_client ~links
let with_client = with_client ~links

let%expect_test "Oneshot" =
  let code =
    {| function! Rpcrequest(...) abort
         let l:job_id = jobstart(['./main.exe', 'oneshot'], { 'rpc': 1 })
         let l:result = call('rpcrequest', [l:job_id] + a:000)
         " Verify that the job finished.
         echo "Job status / exit code: ".jobwait([ l:job_id ], 1000)[0]
         return l:result
       endfunction
       echo "Current buffer: ".Rpcrequest('bufnr') |}
  in
  let%map output =
    with_client (fun client -> Nvim.exec_viml_and_capture_output client code)
  in
  print_endline output;
  [%expect
    {|
    Job status / exit code: 0
    Current buffer: 1
    |}]
;;

let start_plugin ~(here : [%call_pos]) ~client ~subcommand () =
  let%bind () =
    Nvim.exec_lua
      client
      {| if not OnStartup then
           function OnStartup(channel)
             vim.g.channel = channel
           end
         end |}
    >>| ok_exn
  in
  match%map
    Nvim.call_function
      ~here
      client
      ~name:(`Viml "jobstart")
      ~type_:Nvim.Func.(Array String @-> return Int)
      [ "./main.exe"; "persistent"; subcommand ]
    >>| ok_exn
  with
  | 0 -> failwith "[jobstart] got invalid arguments (or job table is full)"
  | -1 -> failwith "./main.exe (or 'shell') is not executable"
  | _job_id -> ()
;;

let%expect_test "Persistent plugin shows errors when they occur during [on_startup]." =
  let test ~subcommand =
    let%map screen =
      with_ui_client (fun client ui ->
        let%bind () = start_plugin ~client ~subcommand () in
        wait_until_text ui ~f:(String.is_substring ~substring:"Failure"))
    in
    print_endline screen
  in
  let%bind () = test ~subcommand:"on-startup-returns-error" in
  [%expect
    {|
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │                                                                                │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │                                                                                │
    │((vcaml-test-persistent-plugin Failure)                                         │
    │ (("Called from" lib/vcaml/plugin/src/vcaml_plugin.ml:LINE:COL)))               │
    │Press ENTER or type command to continue                                         │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  let%bind () = test ~subcommand:"on-startup-raises" in
  [%expect
    {|
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │                                                                                │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │                                                                                │
    │((vcaml-test-persistent-plugin                                                  │
    │  (monitor.ml.Error (Failure Failure)                                           │
    │   ("<backtrace elided in test>" "Caught by monitor try_with_join_or_error")))  │
    │ (("Called from" lib/vcaml/plugin/src/vcaml_plugin.ml:LINE:COL)))               │
    │Press ENTER or type command to continue                                         │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  return ()
;;

let%expect_test "Persistent plugin shows errors when they occur during VimL [notify_fn]." =
  let%bind () =
    let vimscript =
      {| lua function OnStartup() vim.cmd [[ echoerr "This should not be called" ]] end
         function! OnStartup(channel)
           echoerr "Failure in VimL"
         endfunction
       |}
    in
    let%map screen =
      with_ui_client (fun client ui ->
        let%bind () = Nvim.exec_viml client vimscript >>| ok_exn in
        let%bind () = start_plugin ~client ~subcommand:"notify-fn-viml" () in
        wait_until_text ui ~f:(String.is_substring ~substring:"Failure"))
    in
    print_endline screen
  in
  [%expect
    {|
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │                                                                                │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │                                                                                │
    │((vcaml-test-persistent-plugin                                                  │
    │  (("Vim returned error"                                                        │
    │    "function OnStartup, line 1: Vim(echoerr):Failure in VimL"                  │
    │    (error_type Exception))                                                     │
    │   (("Called from" lib/vcaml/plugin/src/vcaml_plugin.ml:LINE:COL))))            │
    │ (("Called from" lib/vcaml/plugin/src/vcaml_plugin.ml:LINE:COL)))               │
    │Press ENTER or type command to continue                                         │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  return ()
;;

let%expect_test "Persistent plugin shows errors when they occur during Lua [notify_fn]." =
  let%bind () =
    let lua = {| function OnStartup() vim.cmd [[ echoerr "Failure" ]] end |} in
    let%map screen =
      with_ui_client (fun client ui ->
        let%bind () = Nvim.exec_lua client lua >>| ok_exn in
        let%bind () =
          (* The [OnStartup] lua function is called by default since it's needed in other
             tests too. *)
          start_plugin ~client ~subcommand:"default" ()
        in
        wait_until_text ui ~f:(String.is_substring ~substring:"Failure"))
    in
    print_endline screen
  in
  [%expect
    {|
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │                                                                                │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │                                                                                │
    │((vcaml-test-persistent-plugin                                                  │
    │  (("Vim returned error"                                                        │
    │     "Error executing lua: vim/_editor.lua:0: nvim_exec2(): Vim(echoerr):Failure│
    │\                                                                               │
    │    \nstack traceback:\                                                         │
    │    \n\t[C]: in function 'nvim_exec2'\                                          │
    │    \n\tvim/_editor.lua: in function 'cmd'\                                     │
    │    \n\t[string \"<nvim>\"]:1: in function 'OnStartup'\                         │
    │    \n\t[string \"<nvim>\"]:1: in main chunk" (error_type Exception))           │
    │   (("Called from" lib/vcaml/plugin/src/vcaml_plugin.ml:LINE:COL))))            │
    │ (("Called from" lib/vcaml/plugin/src/vcaml_plugin.ml:LINE:COL)))               │
    │Press ENTER or type command to continue                                         │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  return ()
;;

let%expect_test "Persistent plugin shows errors when they occur during async RPC." =
  let test ~subcommand =
    let lua =
      {| function OnStartup(channel)
           vim.rpcnotify(channel, "async-rpc")
         end |}
    in
    let%map screen =
      with_ui_client (fun client ui ->
        let%bind () = Nvim.exec_lua client lua >>| ok_exn in
        let%bind () = start_plugin ~client ~subcommand () in
        wait_until_text ui ~f:(String.is_substring ~substring:"Failure"))
    in
    print_endline screen
  in
  let%bind () = test ~subcommand:"async-rpc-returns-error" in
  [%expect
    {|
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │                                                                                │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │                                                                                │
    │((vcaml-test-persistent-plugin Failure)                                         │
    │ (("Called from" lib/vcaml/plugin/test/bin/main.ml:LINE:COL)))                  │
    │Press ENTER or type command to continue                                         │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  let%bind () = test ~subcommand:"async-rpc-raises" in
  [%expect
    {|
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │                                                                                │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │                                                                                │
    │((vcaml-test-persistent-plugin                                                  │
    │  (monitor.ml.Error (Failure Failure)                                           │
    │   ("<backtrace elided in test>" "Caught by monitor try_with_join_or_error")))  │
    │ (("Called from" lib/vcaml/plugin/test/bin/main.ml:LINE:COL)))                  │
    │Press ENTER or type command to continue                                         │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  let%bind () = test ~subcommand:"async-rpc-raises-after-returning" in
  [%expect
    {|
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │                                                                                │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │                                                                                │
    │((vcaml-test-persistent-plugin                                                  │
    │  (monitor.ml.Error (Failure Failure)                                           │
    │   ("<backtrace elided in test>" "Caught by monitor try_with_join_or_error")))  │
    │ (("Called from" lib/vcaml/plugin/test/bin/main.ml:LINE:COL)))                  │
    │Press ENTER or type command to continue                                         │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  return ()
;;

let%expect_test "Persistent plugin shows errors when they occur during blocking RPC." =
  let test ~subcommand =
    let lua =
      {| function OnStartup(channel)
           vim.rpcrequest(channel, "blocking-rpc")
         end |}
    in
    let%map screen =
      with_ui_client (fun client ui ->
        let%bind () = Nvim.exec_lua client lua >>| ok_exn in
        let%bind () = start_plugin ~client ~subcommand () in
        wait_until_text ui ~f:(String.is_substring ~substring:"Failure"))
    in
    print_endline screen
  in
  let%bind () = test ~subcommand:"blocking-rpc-returns-error" in
  [%expect
    {|
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │                                                                                │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │                                                                                │
    │((vcaml-test-persistent-plugin                                                  │
    │  (("Vim returned error"                                                        │
    │     "Error executing lua: (Failure ((\"Called from\" lib/vcaml/plugin/test/bin/│
    │main.ml:LINE:COL)))\                                                            │
    │    \nstack traceback:\                                                         │
    │    \n\t[C]: in function 'rpcrequest'\                                          │
    │    \n\t[string \"<nvim>\"]:2: in function 'OnStartup'\                         │
    │    \n\t[string \"<nvim>\"]:1: in main chunk" (error_type Exception))           │
    │   (("Called from" lib/vcaml/plugin/src/vcaml_plugin.ml:LINE:COL))))            │
    │ (("Called from" lib/vcaml/plugin/src/vcaml_plugin.ml:LINE:COL)))               │
    │Press ENTER or type command to continue                                         │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  let%bind () = test ~subcommand:"blocking-rpc-raises" in
  [%expect
    {|
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │                                                                                │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │                                                                                │
    │((vcaml-test-persistent-plugin                                                  │
    │  (("Vim returned error"                                                        │
    │     "Error executing lua: ((monitor.ml.Error (Failure Failure)\                │
    │    \n  (\"<backtrace elided in test>\" \"Caught by monitor try_with_join_or_err│
    │or\"))\                                                                         │
    │    \n ((\"Called from\" lib/vcaml/plugin/test/bin/main.ml:LINE:COL)))\         │
    │    \nstack traceback:\                                                         │
    │    \n\t[C]: in function 'rpcrequest'\                                          │
    │    \n\t[string \"<nvim>\"]:2: in function 'OnStartup'\                         │
    │    \n\t[string \"<nvim>\"]:1: in main chunk" (error_type Exception))           │
    │   (("Called from" lib/vcaml/plugin/src/vcaml_plugin.ml:LINE:COL))))            │
    │ (("Called from" lib/vcaml/plugin/src/vcaml_plugin.ml:LINE:COL)))               │
    │Press ENTER or type command to continue                                         │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  let%bind () = test ~subcommand:"blocking-rpc-raises-after-returning" in
  [%expect
    {|
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │                                                                                │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │                                                                                │
    │((vcaml-test-persistent-plugin                                                  │
    │  (monitor.ml.Error (Failure Failure)                                           │
    │   ("<backtrace elided in test>" "Caught by monitor try_with_join_or_error")))  │
    │ (("Called from" lib/vcaml/plugin/test/bin/main.ml:LINE:COL)))                  │
    │Press ENTER or type command to continue                                         │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}];
  return ()
;;
