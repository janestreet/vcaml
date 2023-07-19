open Core

module Chunk = struct
  type t =
    { text : string
    ; hl_group : string option
    }
  [@@deriving sexp_of]

  let to_msgpack { text; hl_group } =
    match hl_group with
    | None -> Msgpack.Array [ String text ]
    | Some hl_group -> Array [ String text; String hl_group ]
  ;;
end

type t = Chunk.t list [@@deriving sexp_of]

let to_msgpack t = List.map t ~f:Chunk.to_msgpack
