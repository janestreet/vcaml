open Core

(** Client_info is present for every remote, ui, embedder, host, or plugin
    attached to neovim.

    For more details, run `:h nvim_set_client_info` *)

module Version : sig
  type t = Types.Client_info.version =
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
  type t = Types.Client_info.client_method =
    { async : bool
    ; nargs : [ `Fixed of int | `Range of int * int ] option
    ; opts : Msgpack.t String.Map.t
    }
end

type t = Types.Client_info.t =
  { version : Types.Client_info.version option
  ; methods : Types.Client_info.client_method String.Map.t
  ; attributes : string String.Map.t
  ; name : string option
  ; type_ : Types.Client_info.client_type option
  }

val of_msgpack : Msgpack.t -> t Or_error.t
