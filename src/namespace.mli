open Core


(** A [Namespace.t] allows you to group virtual text and highlights together so that they
    can be cleared in a single function call. *)
type t = private
  { id : int
  ; name : string option
  }
[@@deriving fields, sexp_of]

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t

module Untested : sig
  (** If [name] is given and a namespace with [name] already exists, that namespace is
      returned. Otherwise a new namespace is created. *)
  val create : ?name:string -> unit -> t Api_call.Or_error.t

  val all_named : t String.Map.t Api_call.Or_error.t
end
