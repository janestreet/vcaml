open Core
open Async

(** A [Namespace.t] allows you to group virtual text and highlights together so that they
    can be cleared in a single function call. *)
type t = private
  { id : int
  ; name : string option
  }
[@@deriving fields ~getters, sexp_of]

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t

(** If [name] is given and a namespace with [name] already exists, that namespace is
    returned. Otherwise a new namespace is created. If [name] is omitted, the namespace
    will be anonymous. *)
val create
  :  here:[%call_pos]
  -> _ Client.t
  -> ?name:string
  -> unit
  -> t Deferred.Or_error.t

(** Retrieve a mapping of all named namespaces. *)
val all_named : here:[%call_pos] -> _ Client.t -> t String.Map.t Deferred.Or_error.t
