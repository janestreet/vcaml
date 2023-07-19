open Core

module True_color : sig
  type t =
    { red : char
    ; green : char
    ; blue : char
    }
  [@@deriving sexp_of]

  val to_string : t -> string
  val of_24bit_int : int -> t Or_error.t
end

module Color256 : sig
  type t [@@deriving sexp_of]

  val to_string : t -> string
  val of_8bit_int : int -> t Or_error.t
end

module Depth : sig
  type 'a t =
    | True_color : True_color.t t
    | Color256 : Color256.t t
  [@@deriving sexp_of]
end

module Highlight : sig
  type 'a t =
    { fg : 'a option
    ; bg : 'a option
    }
  [@@deriving sexp_of]
end

module Namespace : sig
  type t

  val global : t
  val of_int : int -> t
  val to_int : t -> int
end

type 'a t =
  | True_color : True_color.t -> True_color.t t
  | Color256 : Color256.t -> Color256.t t
[@@deriving sexp_of]

type packed = T : 'a t -> packed [@@unboxed]

val to_string : _ t -> string
val of_24bit_int : int -> True_color.t t Or_error.t
val of_8bit_int : int -> Color256.t t Or_error.t
