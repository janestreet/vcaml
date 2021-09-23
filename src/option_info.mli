open Core

module Type : sig
  type 'a t =
    | String : string t
    | Integer : int t
    | Boolean : bool t
end

module Last_set_loc : sig
  type t =
    | Unknown
    | By_channel of int
    | By_script of
        { script_id : int
        ; line_nr : int
        }
end

(** If either [comma_list] or [flag_list] is set, the option is an array and the type will
    be a [String]. [flag_list] is a list of single-character flags. [comma_list] is a list
    separated by commas. The only option that has both of these is [whichwrap], which is
    probably a mistake but it dates all the way back to vi. *)
type 'a t =
  { name : string
  ; short_name : string
  ; type_ : 'a Type.t
  ; default : 'a
  ; last_set : Last_set_loc.t option
  ; scope : [ `Global | `Window | `Buffer ]
  ; global_local : bool
  ; comma_list : bool
  ; flag_list : bool
  }

type packed = T : 'a t -> packed [@@unboxed]

val of_msgpack : Msgpack.t -> packed Or_error.t
