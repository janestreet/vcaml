open Core

type nargs =
  [ `Exactly of int
  | `List
  | `At_most_one
  | `At_least_one
  ]

let nargs_of_msgpack ?err_msg = function
  | Msgpack.Nil -> Ok (`Exactly 0)
  | Integer i -> Ok (`Exactly i)
  | String "*" -> Ok `List
  | String "?" -> Ok `At_most_one
  | String "+" -> Ok `At_least_one
  | _ ->
    (match err_msg with
     | Some msg -> Or_error.error_string (msg ^ ": malformed nargs")
     | None -> Or_error.error_string "malformed nargs")
;;

type addr_type =
  [ `Lines
  | `Args
  | `Buffers
  | `Loaded_bufs
  | `Windows
  | `Tabs
  ]

let addr_of_msgpack ?err_msg = function
  | Msgpack.Nil | String "lines" -> Ok `Lines
  | String "arguments" -> Ok `Args
  | String "buffers" -> Ok `Buffers
  | String "loaded_buffers" -> Ok `Loaded_bufs
  | String "windows" -> Ok `Windows
  | String "tabs" -> Ok `Tabs
  | _ ->
    (match err_msg with
     | Some msg -> Or_error.error_string (msg ^ ": malformed addr")
     | None -> Or_error.error_string "malformed addr")
;;

type range_type =
  | Range of
      [ `Defaults_to_current_line
      | `Defaults_to_whole_file
      | `Defaults_to_line_nr of int
      ]
  | Count of int

let convert_range range count =
  let open Or_error.Let_syntax in
  (* This way, we can handle [range] and [combine] with one joined error. *)
  let%bind both = Or_error.combine_errors [ range; count ] in
  let range, count =
    match both with
    | [ range; count ] -> range, count
    | _ -> assert false
  in
  (* These combinations were determined experimentally and may be subject to change with
     future versions of neovim.

     Observed combinations:

     -range   gives range = "." and count = Nil
     -range=% gives range = "%" and count = Nil
     -range=N gives range = "N" and count = Nil
     -count=N gives range = "N" and count = "N" *)
  match range, count with
  | Msgpack.Nil, Msgpack.Nil -> return None
  | String ".", Nil -> return (Some (Range `Defaults_to_current_line))
  | String "%", Nil -> return (Some (Range `Defaults_to_whole_file))
  | String s, Nil ->
    let%map n = Or_error.try_with (fun () -> Int.of_string s) in
    Some (Range (`Defaults_to_line_nr n))
  | String _, String s ->
    let%map n = Or_error.try_with (fun () -> Int.of_string s) in
    Some (Count n)
  | _, _ -> Or_error.error_string "invalid or unknown [range]/[count] combination."
;;

let string_or_nil ?err_msg = function
  | Msgpack.Nil -> Ok None
  | Msgpack.String s -> Ok (Some s)
  | _ ->
    (match err_msg with
     | Some msg -> Or_error.error_string (msg ^ ": called [string_or_nil] on wrong type")
     | None -> Or_error.error_string "called [string_or_nil] on wrong type")
;;

type t =
  { name : string
  ; definition : string
  ; script_id : int
  ; bang : bool
  ; bar : bool
  ; register : bool
  ; nargs : nargs
  ; complete : string option
  ; complete_arg : string option
  ; range : range_type option
  ; addr : addr_type
  }

let of_msgpack msg =
  let open Or_error.Let_syntax in
  let err_msg = "malformed command map" in
  let find_key (type t) m s (extract : ?err_msg:string -> Msgpack.t -> t Or_error.t)
    : t Or_error.t
    =
    Map.find_or_error m s |> Or_error.tag ~tag:err_msg >>= extract ~err_msg
  in
  let%bind map = Extract.map_of_msgpack_map msg in
  let%bind name = find_key map "name" Extract.string in
  let%bind definition = find_key map "definition" Extract.string in
  let%bind script_id = find_key map "script_id" Extract.int in
  let%bind bang = find_key map "bang" Extract.bool in
  let%bind bar = find_key map "bar" Extract.bool in
  let%bind register = find_key map "register" Extract.bool in
  let%bind nargs = find_key map "nargs" nargs_of_msgpack in
  let%bind complete = find_key map "complete" string_or_nil in
  let%bind complete_arg = find_key map "complete" string_or_nil in
  let range_input = Map.find_or_error map "range" in
  let count_input = Map.find_or_error map "count" in
  let%bind range = convert_range range_input count_input |> Or_error.tag ~tag:err_msg in
  let%bind addr = find_key map "addr" addr_of_msgpack in
  return
    { name
    ; definition
    ; script_id
    ; bang
    ; bar
    ; register
    ; nargs
    ; complete
    ; complete_arg
    ; range
    ; addr
    }
;;
