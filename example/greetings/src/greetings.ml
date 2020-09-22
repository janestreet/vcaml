open! Core
open! Async
open Vcaml

(* Simple Vcaml plugin to listen for and respond to rpc messages.
   Source greetings.vim to use it.*)

let greet _state name = Ok (Printf.sprintf "Hello, %s!" name)
let call_shutdown (_client, _state, shutdown) () = Ok (shutdown ())

module Greetings_plugin_arg = struct
  type state = unit

  let rpc_handlers =
    [ Vcaml_plugin.Rpc_handler.create_sync
        ~name:"greeting"
        ~type_:Defun.Ocaml.Sync.(Type.String @-> return Type.String)
        ~f:greet
    ; Vcaml_plugin.Rpc_handler.create_sync
        ~name:"shutdown"
        ~type_:Defun.Ocaml.Sync.(Type.Nil @-> return Type.Nil)
        ~f:call_shutdown
    ]
  ;;

  let startup = Fn.const (Deferred.Or_error.return ())
  let vimscript_notify_fn = Some "OnGreetingsPluginStart"
  let on_shutdown = Fn.const (Deferred.Or_error.return ())
end

module Greetings_plugin = Vcaml_plugin.Persistent.Make (Greetings_plugin_arg)

let main =
  Greetings_plugin.command
    ~summary:"start a Vcaml process which responds to rpc messages with greetings"
    ()
;;

module For_testing = struct
  module Plugin_for_test = Vcaml_plugin.Persistent.Make (struct
      include Greetings_plugin_arg

      let vimscript_notify_fn = None
    end)

  let run = Plugin_for_test.run_for_testing
end
