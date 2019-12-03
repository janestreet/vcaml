open Async
open Msgpack_rpc

module Unix_socket : sig
  include Connection

  val with_socket_file
    :  string
    -> ((t -> 'a Deferred.t) -> 'a Deferred.t) Tcp.with_connect_options

  val open_from_filename : string -> t Deferred.t
end

include Msgpack_rpc.S with type conn = Unix_socket.t
