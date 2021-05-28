open Core

(** Client_info is present for every remote, ui, embedder, host, or plugin
    attached to neovim.

    For more details, run `:h nvim_set_client_info` *)

module Version : sig
  type t =
    { major : int option
    ; minor : int option
    ; patch : int option
    ; prerelease : string option
    ; commit : string option
    }
end

module Client_type : sig
  type t =
    [ `Remote
    | `Ui
    | `Embedder
    | `Host
    | `Plugin
    ]
end

module Client_method : sig
  type t =
    { async : bool
    ; nargs : [ `Fixed of int | `Range of int * int ] option
    }
end

type t =
  { version : Version.t option
  ; methods : Client_method.t String.Map.t
  ; attributes : string String.Map.t
  ; name : string option
  ; type_ : Client_type.t option
  }
[@@deriving sexp_of]

val of_msgpack : Msgpack.t -> t Or_error.t
