open Core

module Type = struct
  type 'a t =
    | String : string t
    | Integer : int t
    | Boolean : bool t
end

module Last_set_loc = struct
  type t =
    | Unknown
    | By_channel of int
    | By_script of
        { script_id : int
        ; line_nr : int
        }
end

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

let of_msgpack msgpack =
  let open Or_error.Let_syntax in
  let%bind option_info = Extract.map_of_msgpack_map msgpack in
  let extract key convert =
    match%bind Extract.and_convert_optional option_info key convert with
    | Some result -> Ok result
    | None -> Or_error.error_s [%message "Missing key" key]
  in
  let%bind name = extract "name" Extract.string in
  let%bind short_name = extract "shortname" Extract.string in
  let%bind last_set =
    match%bind extract "was_set" Extract.bool with
    | false -> return None
    | true ->
      (match%bind extract "last_set_chan" Extract.int with
       | 0 ->
         (match%bind
            Or_error.both
              (extract "last_set_sid" Extract.int)
              (extract "last_set_linenr" Extract.int)
          with
          | 0, 0 -> return (Some Last_set_loc.Unknown)
          | 0, line_nr ->
            Or_error.error_s [%message "Missing script for line number" (line_nr : int)]
          | script_id, 0 ->
            Or_error.error_s [%message "Missing line number for script" (script_id : int)]
          | script_id, line_nr ->
            return (Some (Last_set_loc.By_script { script_id; line_nr })))
       | channel -> return (Some (Last_set_loc.By_channel channel)))
  in
  let%bind scope =
    match%bind extract "scope" Extract.string with
    | "global" -> return `Global
    | "win" -> return `Window
    | "buf" -> return `Buffer
    | scope -> Or_error.error_s [%message "Unrecognized scope" scope]
  in
  let%bind global_local = extract "global_local" Extract.bool in
  let%bind comma_list = extract "commalist" Extract.bool in
  let%bind flag_list = extract "flaglist" Extract.bool in
  match%bind extract "type" Extract.string with
  | "string" ->
    let%bind default = extract "default" Extract.string in
    T
      { name
      ; short_name
      ; type_ = String
      ; default
      ; last_set
      ; scope
      ; global_local
      ; comma_list
      ; flag_list
      }
    |> return
  | "number" ->
    let%bind default = extract "default" Extract.int in
    T
      { name
      ; short_name
      ; type_ = Integer
      ; default
      ; last_set
      ; scope
      ; global_local
      ; comma_list
      ; flag_list
      }
    |> return
  | "boolean" ->
    let%bind default = extract "default" Extract.bool in
    T
      { name
      ; short_name
      ; type_ = Boolean
      ; default
      ; last_set
      ; scope
      ; global_local
      ; comma_list
      ; flag_list
      }
    |> return
  | type_ -> Or_error.error_s [%message "Unrecognized option type" type_]
;;
