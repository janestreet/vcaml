open! Core

type t = Types.Chan_info.t =
  { id : int
  ; stream : [ `Stdio | `Stderr | `Socket | `Job ]
  ; mode : [ `Bytes | `Terminal | `Rpc ]
  ; pty : string option
  ; buffer : Types.Buf.t option
  ; client : Types.Client_info.t option
  }
[@@deriving sexp_of]

val of_msgpack : Msgpack.t -> t Or_error.t
