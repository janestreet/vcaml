open! Core
open! Async
open Vcaml

(* Simple Vcaml plugin to echo Hello world! in the command line
   Source hello_world.vim to use it. *)

let echo_hello_world_in_client ~client =
  Vcaml.run_join client (Client.command ~command:"echom 'Hello world!'")
;;

let connect_and_echo pipe () =
  let open Deferred.Or_error.Let_syntax in
  let%bind client, _process = Client.attach (Unix pipe) in
  let%bind () = echo_hello_world_in_client ~client in
  return ()
;;

let main =
  Command.async_or_error
    ~summary:"print hello world in the current neovim instance"
    (let%map_open.Command () = return () in
     let pipe = Sys.getenv_exn "NVIM_LISTEN_ADDRESS" in
     connect_and_echo pipe)
;;

module For_testing = struct
  let run_plugin_for_testing = echo_hello_world_in_client
end
