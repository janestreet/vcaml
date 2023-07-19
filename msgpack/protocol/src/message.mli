open Core

module Custom : sig
  type t =
    { type_id : int
    ; data : Bytes.t
    }
  [@@deriving compare, sexp]
end

type t =
  | Nil
  | Int of int
  | Int64 of Int64.t
  | Uint64 of Int64.t
  | Bool of bool
  | Float of float
  | Array of t list
  | Map of (t * t) list
  | String of string
  | Binary of Bytes.t
  | Ext of Custom.t
[@@deriving compare, sexp]

include Comparable.S with type t := t

val quickcheck_generator
  :  only_string_keys:bool
  -> only_finite_floats:bool
  -> t Quickcheck.Generator.t
