open! Core
open! Async
open Vcaml

(* Simple Vcaml plugin to echo Hello world! in the command line
   Source hello_world.vim to use it. *)

module Echo_hello_world = Vcaml_plugin.Oneshot.Make (struct
    include Vcaml_plugin.Raise_on_any_error

    let execute client =
      Nvim.command ~command:"echom 'Hello world!'" |> run_join [%here] client
    ;;
  end)

let main =
  Echo_hello_world.command ~summary:"print hello world in the current neovim instance" ()
;;

module For_testing = struct
  let run = Echo_hello_world.run_for_testing
end
