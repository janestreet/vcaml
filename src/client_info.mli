open Core

module Version : sig
  type t =
    { major : int option
    ; minor : int option
    ; patch : int option
    ; prerelease : string option
    ; commit : string option
    }

  val to_msgpack_map : t -> Msgpack.t String.Map.t
end

module Client_type : sig
  type t =
    | Remote
    | Ui
    | Embedder
    | Host
    | Plugin

  val to_string : t -> string
end

module How_to_call_method : sig
  type t =
    { async : bool option
    ; nargs : [ `Fixed of int | `Inclusive_range of int * int ] option
    }

  val to_msgpack : t -> Msgpack.t
end

(** See `:h nvim_set_client_info` for details about this type. *)
type t =
  { name : string option
  ; version : Version.t option
  ; client_type : Client_type.t option
  ; methods : How_to_call_method.t String.Map.t
  ; attributes : string String.Map.t
  }
[@@deriving sexp_of]

val of_msgpack : Msgpack.t -> t Or_error.t
