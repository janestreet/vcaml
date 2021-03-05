open! Core_kernel

module type Nvim_id = sig
  type t = private int [@@deriving sexp_of]

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t
  include Msgpack.Msgpackable with type t := t

  module Unsafe : sig
    val of_int : int -> t
  end
end

module Buffer : Nvim_id
module Window : Nvim_id
module Tabpage : Nvim_id

module Phantom : sig
  (** For use in pattern matching.  e.g. if you have a ['a Phantom.t * 'a], if a pattern
      match on the phantom succeeds on Phantom.Integer, then ['a] unifies with [int]. *)
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
    | Custom : (module Msgpack.Msgpackable with type t = 'a) -> 'a t
  [@@deriving sexp_of]
end

module Api_result : sig
  type 'result t =
    { name : string
    ; params : Msgpack.t
    ; witness : 'result Phantom.t
    }
  [@@deriving sexp_of]
end
