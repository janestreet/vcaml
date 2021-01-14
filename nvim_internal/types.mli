open! Base

module type Nvim_ext = sig
  type t [@@deriving compare, hash, sexp, equal]

  val of_msgpack : Msgpack.t -> t Or_error.t
  val to_msgpack : t -> Msgpack.t
end

module Buffer : Nvim_ext with type t = private int
module Window : Nvim_ext with type t = private int

module Tabpage : Nvim_ext with type t = private Int64.t

module Phantom : sig
  (** For use in pattern matching.  e.g. if you have a
      ['a Phantom.t * 'a], if a pattern match on the
      phantom succeeds on Phantom.Integer, then ['a]
      unifies with [int]. *)
  type _ t =
    | Nil : unit t
    | Integer : int t
    | Boolean : bool t
    | Array : 'a t -> 'a list t
    | Tuple : 'a t * int -> 'a list t
    | Dict : (Msgpack.t * Msgpack.t) list t
    | String : string t
    | Buffer : Buffer.t t
    | Tabpage : Tabpage.t t
    | Window : Window.t t
    | Object : Msgpack.t t
    | Custom :
        { of_msgpack : Msgpack.t -> 'a Or_error.t
        ; to_msgpack : 'a -> Msgpack.t
        }
        -> 'a t
end

type 'result api_result =
  { name : string
  ; params : Msgpack.t
  ; witness : 'result Phantom.t
  }
