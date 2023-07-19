module Chunk : sig
  type t =
    { text : string
    ; hl_group : string option
    }
  [@@deriving sexp_of]

  val to_msgpack : t -> Msgpack.t
end

type t = Chunk.t list [@@deriving sexp_of]

val to_msgpack : t -> Msgpack.t list
