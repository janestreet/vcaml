open! Core
open! Async
open Vcaml

(* Simple Vcaml plugin to listen for and respond to rpc messages.
   Source greetings.vim to use it.*)

let greet name = Ok (Printf.sprintf "Hello, %s!" name)

let perform_registrations ~client ~terminate_var =
  let open Or_error.Let_syntax in
  let%bind () =
    register_request_blocking
      client
      ~name:"shutdown"
      ~type_:Defun.Ocaml.Sync.(Type.Nil @-> return Type.Nil)
      ~f:(fun () -> Ok (Ivar.fill terminate_var ()))
  in
  let%bind () =
    register_request_blocking
      client
      ~name:"greeting"
      ~type_:Defun.Ocaml.Sync.(Type.String @-> return Type.String)
      ~f:greet
  in
  return ()
;;

let is_greetings_plugin_chan ~channel_info:{ Channel_info.client = client_opt; _ } ~name =
  let open Option.Let_syntax in
  Option.value
    ~default:false
    (let%bind { name = name_opt; _ } = client_opt in
     let%map chan_name = name_opt in
     String.equal chan_name name)
;;

let get_rpc_channel_id ~client ~name =
  let open Deferred.Or_error.Let_syntax in
  let%bind channel_list = Vcaml.run_join client Client.list_chans in
  match
    List.find
      ~f:(fun channel_info -> is_greetings_plugin_chan ~channel_info ~name)
      channel_list
  with
  | Some { id; _ } -> return id
  | None ->
    Deferred.Or_error.error_string "Cannot find rpc with the correct name for plugin"
;;

let notify_client_of_startup ~client ~channel_id =
  Vcaml.run_join
    client
    (Client.call_function ~fn:"OnGreetingsPluginStart" ~args:[ Integer channel_id ])
;;

let setup_client_name ~client ~name =
  Vcaml.run_join client (Client.set_client_info ~name ~type_:`Plugin ())
;;

let start_client ~client ~name ~terminate_var =
  let open Deferred.Or_error.Let_syntax in
  let%bind () = setup_client_name ~client ~name in
  Deferred.return (perform_registrations ~client ~terminate_var)
;;

let connect_and_start_plugin pipe ~terminate_var () =
  let open Deferred.Or_error.Let_syntax in
  let name = Uuid.to_string (Uuid_unix.create ()) in
  let%bind client, _process = Client.attach (Unix pipe) in
  let%bind () = start_client ~client ~name ~terminate_var in
  let%bind channel_id = get_rpc_channel_id ~client ~name in
  let%bind (_ : Msgpack.t) = notify_client_of_startup ~client ~channel_id in
  Deferred.ok (Ivar.read terminate_var)
;;

let main =
  Command.async_or_error
    ~summary:"start a Vcaml process which responds to rpc messages with greetings"
    (let%map_open.Command () = return () in
     let terminate_var = Ivar.create () in
     let pipe = Sys.getenv_exn "NVIM_LISTEN_ADDRESS" in
     connect_and_start_plugin pipe ~terminate_var)
;;

module For_testing = struct
  let start_plugin_for_testing ~client ~name ~terminate_var =
    start_client ~client ~name ~terminate_var
  ;;

  let get_rpc_chan_for_testing = get_rpc_channel_id
end
