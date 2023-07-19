open Core

module Connected_to : sig
  type t =
    | Stdio
    | Stderr
    | Socket
    | Job
end

module Protocol : sig
  type t =
    | Ansi (** Indicates a connection to a terminal *)
    | Msgpack_rpc
end

(** See `:h nvim_get_chan_info` for details about this type. *)
type t =
  { id : int
  ; connected_to : Connected_to.t
  ; protocol : Protocol.t option
  ; pty : string option
  ; buffer : Nvim_internal.Buffer.t option
  ; client : Client_info.t option
  }
[@@deriving sexp_of]

val of_msgpack_map : Msgpack.t String.Map.t -> t Or_error.t
