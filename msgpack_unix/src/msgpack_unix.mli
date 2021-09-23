open Async

val with_socket_file
  :  string
  -> on_error:(message:string -> Msgpack.t -> unit)
  -> close_reader_and_writer_on_disconnect:bool
  -> ((Msgpack_rpc.t -> 'a Deferred.t) -> 'a Deferred.t) Tcp.with_connect_options

val open_from_filename
  :  string
  -> on_error:(message:string -> Msgpack.t -> unit)
  -> close_reader_and_writer_on_disconnect:bool
  -> Msgpack_rpc.t Deferred.t
