open Core

(** A representation of a keybinding *)

type t =
  { silent : bool
  ; noremap : bool
  ; mode : string
  ; nowait : bool
  ; expr : bool
  ; sid : int
  ; lhs : string
  ; rhs : string
  ; buffer : Types.Buf.t option
  }

module Untested : sig
  val of_msgpack
    :  ?to_buf:(Msgpack.t -> Types.Buf.t Or_error.t)
    -> Msgpack.t
    -> t Or_error.t
end
