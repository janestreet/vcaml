open! Core
open! Async
open Vcaml

(* Simple Vcaml plugin to echo Hello world! in the command line
   Source hello_world.vim to use it. *)

module Echo_hello_world = Vcaml_plugin.Make_oneshot (struct
    let execute client =
      Vcaml.run_join client (Client.command ~command:"echom 'Hello world!'")
    ;;
  end)

let main =
  Echo_hello_world.command ~summary:"print hello world in the current neovim instance" ()
;;

let test = Echo_hello_world.test
