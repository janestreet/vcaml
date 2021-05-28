open! Core
open! Async

module Info = struct
  type t =
    | To_client of string
    | From_client of string

  let to_color = function
    | From_client _ -> `Red
    | To_client _ -> `Green
  ;;

  let to_string t =
    let dir_pipe_name =
      match t with
      | To_client st -> Printf.sprintf "< %s" st
      | From_client st -> Printf.sprintf "> %s" st
    in
    Console.Ansi.string_with_attr [ to_color t ] dir_pipe_name
  ;;
end

let print_with_passthrough info writer msg =
  print_endline (Info.to_string info);
  print_s ([%sexp_of: Msgpack.t] msg);
  Async.Writer.write writer (Msgpack.string_of_t_exn msg);
  return ()
;;

let debug_messages info reader writer =
  Angstrom_async.parse_many
    Msgpack.Internal.Parser.msg
    (print_with_passthrough info writer)
    reader
;;

let on_connect ~client_pipe _addr host_reader host_writer =
  let%bind socket = Msgpack_unix.Unix_socket.open_from_filename client_pipe in
  let client_reader = Msgpack_unix.Unix_socket.reader socket in
  let client_writer = Msgpack_unix.Unix_socket.writer socket in
  let%bind (_ : (unit, string) result list) =
    Deferred.all
      [ debug_messages (Info.To_client client_pipe) client_reader host_writer
      ; debug_messages (Info.From_client client_pipe) host_reader client_writer
      ]
  in
  return ()
;;

let start_server ~host_pipe ~client_pipe =
  Tcp.Server.create
    ~on_handler_error:`Raise
    (Tcp.Where_to_listen.of_file host_pipe)
    (on_connect ~client_pipe)
;;

let run_prog_with_fresh_pipe ~prog ~pipe =
  let tmp_dir = Filename_unix.temp_dir ~perm:0o777 "mitm" "pipe" in
  let tmp_pipe = Filename.concat tmp_dir "tmp.pipe" in
  let%bind connection = start_server ~host_pipe:tmp_pipe ~client_pipe:pipe in
  let%bind output =
    Process.run_exn
      ~prog:"bash"
      ~args:[ "-c"; prog ]
      ~env:(`Extend [ "NVIM_LISTEN_ADDRESS", tmp_pipe; "MITM_ADDRESS", tmp_pipe ])
      ()
  in
  printf "\nProcess terminated. Output:\n%s\n" output;
  Tcp.Server.close connection
;;

let main =
  Command.async
    ~summary:"debug msgpack unix sockets by serving as a man-in-the-middle"
    (let%map_open.Command () = return ()
     and original_pipe =
       flag
         "-socket"
         (optional_with_default
            (Sys.getenv_exn "NVIM_LISTEN_ADDRESS")
            Filename_unix.arg_type)
         ~doc:"the unix socket that we should intercept messages to"
     and prog =
       flag "-cmd" (required string) ~doc:"the command to run and intercept messages from"
     in
     fun () -> run_prog_with_fresh_pipe ~prog ~pipe:original_pipe)
;;
