module Chunk : sig
  type t =
    { text : string
    ; hl_group : string option
    }

  val to_msgpack : t -> Msgpack.t
end

type t = Chunk.t list

val to_msgpack : t -> Msgpack.t list
