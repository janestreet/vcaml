open Core

type t =
  { sym : char
  ; row : int
  ; col : int
  }
[@@deriving sexp_of]
