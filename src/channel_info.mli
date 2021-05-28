open! Core

type t =
  { id : int
  ; stream : [ `Stdio | `Stderr | `Socket | `Job ]
  ; mode : [ `Bytes | `Terminal | `Rpc ]
  ; pty : string option
  ; buffer : Nvim_internal.Buffer.t option
  ; client : Client_info.t option
  }
[@@deriving sexp_of]

val of_msgpack : Msgpack.t -> t Or_error.t
