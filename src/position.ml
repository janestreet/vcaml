open Core

module T = struct
  type t =
    { row : int
    ; col : int
    }
  [@@deriving sexp_of]
end

include T

module One_indexed_row = struct
  include T

  let to_zero t = { t with row = t.row - 1 }
  let of_zero t = { t with row = t.row + 1 }
end
