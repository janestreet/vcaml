type t =
  { row : int
  ; col : int
  }
[@@deriving fields, sexp_of]

module One_indexed_row : sig
  type zero_indexed_row := t

  type t =
    { row : int
    ; col : int
    }
  [@@deriving fields, sexp_of]

  val to_zero : t -> zero_indexed_row
  val of_zero : zero_indexed_row -> t
end
