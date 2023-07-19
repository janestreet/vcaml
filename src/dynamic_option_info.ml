open Core
open Import

module Last_set_loc = struct
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
  ; last_set : Last_set_loc.t option
  }
[@@deriving sexp_of]

let of_msgpack_map map ~default_of_msgpack =
  let open Or_error.Let_syntax in
  let find_key = find_or_error_and_convert in
  let%bind default = find_key map "default" default_of_msgpack in
  let%bind last_set =
    let open Last_set_loc in
    match%bind find_key map "was_set" (Type.of_msgpack Bool) with
    | false -> return None
    | true ->
      (match%bind find_key map "last_set_chan" (Type.of_msgpack Int) with
       | 0 ->
         (match%bind
            Or_error.both
              (find_key map "last_set_sid" (Type.of_msgpack Int))
              (find_key map "last_set_linenr" (Type.of_msgpack Int))
          with
          | 0, 0 -> return (Some Unknown)
          | 0, line_nr ->
            Or_error.error_s [%message "Missing script for line number" (line_nr : int)]
          | script_id, 0 ->
            Or_error.error_s [%message "Missing line number for script" (script_id : int)]
          | script_id, line_nr -> return (Some (By_script { script_id; line_nr })))
       | channel -> return (Some (By_channel channel)))
  in
  return { default; last_set }
;;
