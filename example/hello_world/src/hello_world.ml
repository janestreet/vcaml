open! Core
open! Async
open Vcaml

(* Simple Vcaml plugin to echo Hello world! in the command line
   Source hello_world.vim to use it. *)

let echo_hello_world client =
  Nvim.command ~command:"echom 'Hello world!'" |> run_join [%here] client
;;

let main =
  Async.Command.async_or_error
    ~summary:"print hello world in the current neovim instance"
    (let%map_open.Async.Command () = return () in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       let%bind client =
         Client.attach (Unix `Child) ~time_source:(Time_source.wall_clock ())
       in
       echo_hello_world client)
;;

module For_testing = struct
  let echo_hello_world = echo_hello_world
end
