open Async

let with_socket_file
      fname
      ~on_error
      ~close_reader_and_writer_on_disconnect
      ?buffer_age_limit
      ?interrupt
      ?reader_buffer_size
      ?writer_buffer_size
      ?timeout
      ?time_source
      f
  =
  let addr = Tcp.Where_to_connect.of_file fname in
  Tcp.with_connection
    addr
    ?buffer_age_limit
    ?interrupt
    ?reader_buffer_size
    ?writer_buffer_size
    ?timeout
    ?time_source
    (fun _ reader writer ->
       f
         (Msgpack_rpc.connect
            reader
            writer
            ~on_error
            ~close_reader_and_writer_on_disconnect))
;;

let open_from_filename fname ~on_error ~close_reader_and_writer_on_disconnect =
  let addr = Tcp.Where_to_connect.of_file fname in
  let%map _sock, reader, writer = Tcp.connect addr in
  Msgpack_rpc.connect reader writer ~on_error ~close_reader_and_writer_on_disconnect
;;
