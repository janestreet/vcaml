open! Core
open! Async

module Man_in_the_middle_debugger = Man_in_the_middle_debugger.Make (struct
    type message = Msgpack.t

    let parser_ = Msgpack.Internal.Parser.msg
    let to_string = Msgpack.string_of_t_exn ?bufsize:None
  end)

let create_debug_printers
  ?(pp = Msgpack.pp ?pp_ext:None)
  ?(color = false)
  ~peer1
  ~peer2
  log
  =
  let formatter = Format.formatter_of_out_channel log in
  let maybe_color =
    match color with
    | false -> fun s _color -> s
    | true -> fun s color -> Console.Ansi.string_with_attr [ color; `Bright ] s
  in
  let peer1 = maybe_color peer1 `Blue in
  let peer2 = maybe_color peer2 `Yellow in
  let create_debug_printer ~label message =
    let open Format in
    pp_open_hovbox formatter 0;
    pp_print_string formatter label;
    pp_print_char formatter ':';
    pp_print_space formatter ();
    pp formatter message;
    pp_close_box formatter ();
    pp_print_newline formatter ()
  in
  let peer_1_to_2 = create_debug_printer ~label:[%string "%{peer1} -> %{peer2}"] in
  let peer_2_to_1 = create_debug_printer ~label:[%string "%{peer2} -> %{peer1}"] in
  `Peer_1_to_2 peer_1_to_2, `Peer_2_to_1 peer_2_to_1
;;

let connect_peers_and_listen ?pp ?color ~peer1 ~peer2 ~log () =
  let `Peer_1_to_2 peer_1_to_2, `Peer_2_to_1 peer_2_to_1 =
    create_debug_printers
      ?pp
      ?color
      ~peer1:peer1.Man_in_the_middle_debugger.Peer.name
      ~peer2:peer2.Man_in_the_middle_debugger.Peer.name
      log
  in
  let%map `Peer1 peer1_stopped, `Peer2 peer2_stopped =
    Man_in_the_middle_debugger.connect_peers_and_listen ~peer1 ~peer2 ~f:(function
      | `Peer_1_to_2 -> peer_1_to_2
      | `Peer_2_to_1 -> peer_2_to_1)
  in
  let exit_status angstrom_exit_status ~from ~to_ =
    [%message
      [%string "%{from} -> %{to_}"] (angstrom_exit_status : (unit, string) Result.t)]
  in
  [%message
    "Connection closed."
      ~_:(exit_status peer1_stopped ~from:peer1.name ~to_:peer2.name : Sexp.t)
      ~_:(exit_status peer2_stopped ~from:peer2.name ~to_:peer1.name : Sexp.t)]
  |> Sexp.to_string_hum
  |> Out_channel.output_string log;
  Out_channel.output_char log '\n';
  Out_channel.flush log
;;

let connect_unix_peers_and_listen
  ?pp
  ?color
  ~client_name
  ~client_socket
  ~server_name
  ~server_socket
  ()
  =
  let terminated = Ivar.create () in
  let%bind server =
    Tcp.Server.create
      ~on_handler_error:`Raise
      client_socket (* Act as a server to the client. *)
      (fun (_ : Socket.Address.Unix.t) client_reader client_writer ->
         let%bind (_ : _ Socket.t), server_reader, server_writer =
           Tcp.connect server_socket (* Act as a client to the server. *)
         in
         let%bind () =
           client_socket
           |> Tcp.Where_to_listen.address
           |> Socket.Address.Unix.to_string
           |> Unix.unlink
         in
         let%bind () =
           connect_peers_and_listen
             ?pp
             ?color
             ~peer1:{ name = client_name; reader = client_reader; writer = client_writer }
             ~peer2:{ name = server_name; reader = server_reader; writer = server_writer }
             ~log:stderr
             ()
         in
         Ivar.fill_exn terminated ();
         return ())
  in
  let%bind () = Ivar.read terminated in
  Tcp.Server.close server
;;

let connect_stdio_peers_and_listen ?pp ?color ~log ~parent_name ~child_name ~prog ~args ()
  =
  let log = Out_channel.create log in
  let%bind child = Process.create_exn ~prog ~args () in
  let done_transferring_stderr =
    let child_reader = Process.stderr child in
    let parent_writer = force Writer.stderr in
    let parent_pipe = Writer.pipe parent_writer in
    let%bind () = Reader.transfer child_reader parent_pipe in
    let%bind () = Reader.close child_reader in
    let%bind (`Ok | `Reader_closed) = Pipe.upstream_flushed parent_pipe in
    Writer.close parent_writer
  in
  let%bind () =
    connect_peers_and_listen
      ?pp
      ?color
      ~peer1:
        { name = parent_name; reader = force Reader.stdin; writer = force Writer.stdout }
      ~peer2:
        { name = child_name; reader = Process.stdout child; writer = Process.stdin child }
      ~log
      ()
  in
  let%bind () = done_transferring_stderr in
  let%bind exit_message = Process.wait child >>| Core_unix.Exit_or_signal.to_string_hum in
  Out_channel.output_string log [%string "%{child_name} %{exit_message}"];
  Out_channel.close log;
  return ()
;;

let unix_domain_sockets_command ?pp () =
  Command.async
    ~behave_nicely_in_pipeline:true
    ~summary:"Capture Msgpack RPC traffic over unix domain sockets"
    ~readme:(fun () ->
      {|
Each endpoint uses a different socket and the debugger ferries messages between them. The
server program should be listening on [server-socket] and the client program should try
connecting to [client-socket]. The debugger acts as a server to the client and a client
to the server.

First start the server program, then start this debugger specifying the [server-socket]
where that server program is listening and a new [client-socket] where the debugger will
start listening. Finally, start the client program and have it connect to [client-socket]. |})
    (let%map_open.Command () = return ()
     and client_name =
       flag "client-name" (required string) ~doc:"STRING Name to use for client in log"
     and client_socket =
       flag
         "client-socket"
         (required Filename_unix.arg_type)
         ~doc:"SOCKET Where the client will connect"
       >>| Tcp.Where_to_listen.of_file
     and server_name =
       flag "server-name" (required string) ~doc:"STRING Name to use for server in log"
     and server_socket =
       flag
         "server-socket"
         (required Filename_unix.arg_type)
         ~doc:"SOCKET Where the server is listening"
       >>| Tcp.Where_to_connect.of_file
     and color =
       let always_never =
         Command.Arg_type.of_alist_exn
           ~list_values_in_help:false
           [ "always", true; "never", false ]
       in
       flag "color" (optional always_never) ~doc:"(always|never) Color names in the log"
     in
     fun () ->
       let color =
         match color with
         | Some bool -> bool
         | None -> Core_unix.(isatty stderr)
       in
       connect_unix_peers_and_listen
         ?pp
         ~color
         ~client_name
         ~client_socket
         ~server_name
         ~server_socket
         ())
;;

let stdio_command ?pp () =
  Command.async
    ~behave_nicely_in_pipeline:true
    ~summary:"Capture Msgpack RPC traffic over stdio"
    ~readme:(fun () ->
      {|
Run an embedded program that expects to communicate Msgpack RPC over stdio and write the
communication to a file. Stderr from the child is passed through. |})
    (let%map_open.Command () = return ()
     and log =
       flag
         "log"
         (required Filename_unix.arg_type)
         ~doc:"FILENAME Where to log the communication"
     and parent_name =
       flag "parent-name" (required string) ~doc:"STRING Name to use for parent in log"
     and child_name =
       flag "child-name" (required string) ~doc:"STRING Name to use for child in log"
     and program = flag "--" escape ~doc:"PROG Child program to run"
     and color =
       flag
         "color"
         (optional_with_default false bool)
         ~doc:"BOOL Color names in the log (default: false)"
     in
     fun () ->
       match program with
       | None | Some [] -> failwith "Must provide an embedded program to run"
       | Some (prog :: args) ->
         connect_stdio_peers_and_listen
           ?pp
           ~color
           ~log
           ~parent_name
           ~child_name
           ~prog
           ~args
           ())
;;

let command ?pp () =
  Command.group
    ~summary:"Capture Msgpack RPC traffic"
    [ "stdio", stdio_command ?pp ()
    ; "unix", unix_domain_sockets_command ?pp ()
    ; "convert", Conversions.command
    ]
;;
