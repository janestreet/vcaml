open! Base

type custom =
  { type_id : int
  ; data : Bytes.t
  }
[@@deriving sexp]

type t =
  | Nil
  | Integer of int
  | Int64 of Int64.t
  | UInt64 of Int64.t
  | Boolean of bool
  | Floating of float
  | Array of t list
  (* The specification doesn't say what to do in the case of duplicate keys. Also,
     these objects are currently mutable, so [Map.t] might not be the best idea.
  *)
  | Map of (t * t) list
  | String of string
  | Binary of Bytes.t
  | Extension of custom
[@@deriving sexp]
