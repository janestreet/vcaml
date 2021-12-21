open Core
open Nvim_internal

let convert_msgpack_error result ~on_keyboard_interrupt =
  match result with
  | Ok _ as ok -> ok
  | Error (Msgpack.Array [ Integer error_type; String msg ]) ->
    let error_type = Error_type.of_int error_type in
    (match error_type, msg with
     | Exception, "Keyboard interrupt" -> on_keyboard_interrupt ()
     | _ -> ());
    Or_error.error_s [%message "Vim returned error" msg (error_type : Error_type.t)]
  | Error error -> Or_error.error "Msgpack error response" error [%sexp_of: Msgpack.t]
;;

(* Since native Vim functions don't support API extensions and since the API functions can
   take raw integer arguments, we use the raw encoding here. *)
let rec inject : type t. t Phantom.t -> t -> Msgpack.t =
  let open Phantom in
  fun witness obj ->
    match witness with
    | Nil -> Nil
    | Integer -> Integer obj
    | Boolean -> Boolean obj
    | Float -> Floating obj
    | Dict -> Map obj
    | String -> String obj
    | Buffer -> Nvim_internal.Buffer.to_msgpack obj
    | Tabpage -> Nvim_internal.Tabpage.to_msgpack obj
    | Window -> Nvim_internal.Window.to_msgpack obj
    | Luaref -> Nvim_internal.Luaref.to_msgpack obj
    | Object -> obj
    | Array t' -> Array (List.map ~f:(inject t') obj)
    | Tuple (t', _) -> Array (List.map ~f:(inject t') obj)
    | Custom (module M) -> M.to_msgpack obj
;;

let rec value : type t. t Phantom.t -> Msgpack.t -> t Or_error.t =
  fun witness msg ->
  let err_msg = "witness does not match message type" in
  match witness, msg with
  | Nil, Nil -> Ok ()
  | Nil, Array [] -> Ok ()
  | Nil, Map [] -> Ok ()
  | Integer, Integer i -> Ok i
  | Boolean, Boolean b -> Ok b
  | Boolean, Integer 0 -> Ok false
  | Boolean, Integer _ -> Ok true
  | Float, Floating f -> Ok f
  | Tuple (t, _), Array vs ->
    List.map vs ~f:(fun v -> value t v)
    |> Or_error.combine_errors
    |> Or_error.tag ~tag:err_msg
  | Array t, Array vs ->
    List.map vs ~f:(fun v -> value t v)
    |> Or_error.combine_errors
    |> Or_error.tag ~tag:err_msg
  | Dict, Map kvs -> Ok kvs
  | String, String s -> Ok s
  | Buffer, _ -> Nvim_internal.Buffer.of_msgpack msg
  | Window, _ -> Nvim_internal.Window.of_msgpack msg
  | Tabpage, _ -> Nvim_internal.Tabpage.of_msgpack msg
  | Luaref, _ -> Nvim_internal.Luaref.of_msgpack msg
  | Object, _ -> Ok msg
  | Custom (module M), obj -> M.of_msgpack obj
  | _ -> Or_error.error_s [%message err_msg (witness : _ Phantom.t) (msg : Msgpack.t)]
;;

let string msg =
  value String msg |> Or_error.tag ~tag:"called [extract_string] on non-string"
;;

let int msg = value Integer msg |> Or_error.tag ~tag:"called [extract_int] on non-int"
let bool msg = value Boolean msg |> Or_error.tag ~tag:"called [extract_bool] on non-bool"
let float msg = value Float msg |> Or_error.tag ~tag:"called [extract_float] on non-float"

let map_of_msgpack_alist kvs =
  let extract_key s =
    match s with
    | Msgpack.String s, i -> Ok (s, i)
    | _ -> Or_error.error_string "map key is not string"
  in
  let open Or_error.Let_syntax in
  let%bind stringed = List.map kvs ~f:extract_key |> Or_error.combine_errors in
  String.Map.of_alist_or_error stringed
;;

let map_of_msgpack_map = function
  | Msgpack.Map kvs -> map_of_msgpack_alist kvs
  | _ -> Or_error.error_string "called [map_of_msgpack_map] on a non-map"
;;

let map_to_msgpack_alist map =
  Map.to_alist map |> List.map ~f:(fun (k, v) -> Msgpack.String k, v)
;;

let map_to_msgpack_map map = Msgpack.Map (map_to_msgpack_alist map)

let and_convert_optional map key transform =
  match Map.find map key with
  | Some x -> Or_error.map (transform x) ~f:(fun y -> Some y)
  | None -> Ok None
;;
