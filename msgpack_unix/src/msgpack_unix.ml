open Async
open Core

module Unix_socket : sig
  include Msgpack_rpc.Connection

  val with_socket_file
    :  string
    -> ((t -> 'a Deferred.t) -> 'a Deferred.t) Tcp.with_connect_options

  val open_from_filename : string -> t Deferred.t
end = struct
  type t =
    { reader : Reader.t
    ; writer : Writer.t
    }
  [@@deriving fields]

  let with_socket_file
        fname
        ?buffer_age_limit
        ?interrupt
        ?reader_buffer_size
        ?writer_buffer_size
        ?timeout
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
      (fun _ reader writer -> f { reader; writer })
  ;;

  let open_from_filename fname =
    let addr = Tcp.Where_to_connect.of_file fname in
    let%map _sock, reader, writer = Tcp.connect addr in
    { reader; writer }
  ;;
end

module M = Msgpack_rpc.Make (Unix_socket) ()
include M
