open! Core

type t = Types.Channel_info.t =
  { id : int
  ; stream : [ `Stdio | `Stderr | `Socket | `Job ]
  ; mode : [ `Bytes | `Terminal | `Rpc ]
  ; pty : string option
  ; buffer : Types.Buffer.t option
  ; client : Types.Client_info.t option
  }
[@@deriving sexp_of]

val of_msgpack : Msgpack.t -> t Or_error.t
