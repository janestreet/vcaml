open Core

(** This module represents information about Neovim options that can change over time. The
    static option information is used to generate the typed VCaml bindings, and should not
    be of interest to plugins. *)

module Last_set_loc : sig
  type t =
    | Unknown
    | By_channel of int
    | By_script of
        { script_id : int
        ; line_nr : int
        }
  [@@deriving sexp_of]
end

type 'a t =
  { default : 'a
  (** Even though the default value for an option is not dynamic, it is system-dependent,
      so we cannot hardcode it in VCaml. *)
  ; last_set : Last_set_loc.t option
  }
[@@deriving sexp_of]

val of_msgpack_map
  :  Msgpack.t String.Map.t
  -> default_of_msgpack:(Msgpack.t -> 'a Or_error.t)
  -> 'a t Or_error.t
