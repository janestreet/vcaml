open! Core
open Async
open Vcaml

(* This is an example of a "persistent" plugin - the process starts up and continues to
   field RPC requests until it is shut down. *)

type state = { mutable most_recent_name : string option }

let say_hello =
  Vcaml_plugin.Persistent.Rpc.create_blocking
    "hello"
    ~type_:Ocaml_from_nvim.Blocking.(String @-> return Nil)
    ~f:(fun state ~run_in_background:_ ~client name ->
      state.most_recent_name <- Some name;
      Nvim.out_writeln client [%string "Hello, %{name}!"])
;;

let on_startup client =
  let open Deferred.Or_error.Let_syntax in
  let channel = Client.channel client in
  let state = { most_recent_name = None } in
  let%bind () =
    Command.create
      client
      ~bar:true
      ~nargs:One
      ~completion:User
      ()
      ~name:"SayHello"
      ~scope:`Global
      (* We cannot implement [:SayHello] with an anonymous RPC because we need to pass an
         argument. Instead, we define an RPC named "hello" and invoke it from VimL with
         the argument passed to the command. *)
      (Viml [%string {| call rpcrequest(%{channel#Int}, "hello", <q-args>) |}])
  in
  let%bind () =
    Command.create
      client
      ~bar:true
      ()
      ~name:"SayGoodbye"
      ~scope:`Global
      (Ocaml_from_nvim.Callback.anon_rpc (fun ~run_in_background ~client ->
         run_in_background (fun (_ : [ `asynchronous ] Client.t) ->
           (* This will only run after the RPC returns. *)
           exit 0);
         let message =
           match state.most_recent_name with
           | None -> "Goodbye!"
           | Some name -> [%string "Goodbye, %{name}!"]
         in
         Nvim.out_writeln client message))
  in
  Deferred.Or_error.return state
;;

let command =
  Vcaml_plugin.Persistent.create
    ~name:"hello-world-2"
    ~description:"Persistent plugin to say hello"
    ~on_startup
    ~notify_fn:(`Lua "function() end")
    [ say_hello ]
;;
