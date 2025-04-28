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

(* The Neovim API doesn't use one-indexed columns, but they are used in VimL functions, so
   having this type is useful for plugins that wrap them. *)
module One_indexed_row_and_column = struct
  include T
end
