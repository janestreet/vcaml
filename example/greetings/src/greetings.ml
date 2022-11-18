open Core
open Async
open Vcaml

(* Simple VCaml plugin to listen for and respond to RPC messages. Source greetings.vim to
   use it.*)

let greet _state ~shutdown:_ ~keyboard_interrupted:_ ~client:_ name =
  Deferred.Or_error.return (Printf.sprintf "Hello, %s!" name)
;;

include Vcaml_plugin.Persistent.Make (struct
    let name = "greetings"
    let description = "Start a VCaml process that responds to RPC messages with greetings."

    type state = unit [@@deriving sexp_of]

    let init_state () = ()

    let rpc_handlers =
      [ Vcaml_plugin.Persistent.Rpc.create_sync
          ~name:"greeting"
          ~type_:Defun.Ocaml.Sync.(Type.String @-> return Type.String)
          ~f:greet
      ]
    ;;

    let on_startup _client () ~shutdown:_ = Deferred.Or_error.return ()
    let on_error = `Raise
    let vimscript_notify_fn = Some "OnGreetingsPluginStart"
    let on_shutdown _ _ = Deferred.Or_error.return ()
  end)
