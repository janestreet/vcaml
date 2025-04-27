type t =
  { row : int
  ; col : int
  }
[@@deriving sexp_of]

module One_indexed_row : sig
  type zero_indexed_row := t

  type t =
    { row : int
    ; col : int
    }
  [@@deriving sexp_of]

  val to_zero : t -> zero_indexed_row
  val of_zero : zero_indexed_row -> t
end

module One_indexed_row_and_column : sig
  type t =
    { row : int
    ; col : int
    }
  [@@deriving sexp_of]
end
