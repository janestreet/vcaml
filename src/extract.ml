open Core
open Msgpack
module Buffer = Types.Buffer
module Window = Types.Window
module Tabpage = Types.Tabpage

let convert_msgpack_error =
  Result.map_error ~f:(function
    | Msgpack.Array [ Integer _; String s ] ->
      Error.create "Vim returned error" s [%sexp_of: string]
    | m -> Error.create "Msgpack error response" m [%sexp_of: Msgpack.t])
;;

(* Since native Vim functions don't support API extensions and since the API functions can
   take raw integer arguments, we use the raw encoding here. *)
let rec inject : type t. t Types.Phantom.t -> t -> Msgpack.t =
  let open Types.Phantom in
  fun witness obj ->
    match witness with
    | Nil -> Msgpack.Nil
    | Integer -> Msgpack.Integer obj
    | Boolean -> Msgpack.Boolean obj
    | Dict -> Msgpack.Map obj
    | String -> Msgpack.String obj
    | Buffer -> Buffer.to_msgpack obj
    | Tabpage -> Tabpage.to_msgpack obj
    | Window -> Window.to_msgpack obj
    | Object -> obj
    | Array t' -> Msgpack.Array (List.map ~f:(inject t') obj)
    | Tuple (t', _) -> Msgpack.Array (List.map ~f:(inject t') obj)
    | Custom (module M) -> M.to_msgpack obj
;;

let rec value : type t. ?err_msg:string -> t Types.Phantom.t -> Msgpack.t -> t Or_error.t =
  fun ?(err_msg = "witness does not match message type") ->
  let open Types.Phantom in
  fun witness msg ->
    match witness, msg with
    | Nil, Msgpack.Nil -> Ok ()
    | Nil, Msgpack.Array [] -> Ok ()
    | Nil, Msgpack.Map [] -> Ok ()
    | Integer, Msgpack.Integer i -> Ok i
    | Boolean, Msgpack.Boolean b -> Ok b
    | Boolean, Msgpack.Integer 0 -> Ok false
    | Boolean, Msgpack.Integer _ -> Ok true
    | Tuple (t, _), Msgpack.Array vs ->
      Ok (List.filter_map ~f:(fun v -> value ~err_msg t v |> Or_error.ok) vs)
    | Array t, Msgpack.Array vs ->
      Ok (List.filter_map ~f:(fun v -> value ~err_msg t v |> Or_error.ok) vs)
    | Dict, Msgpack.Map kvs -> Ok kvs
    | String, Msgpack.String s -> Ok s
    | Buffer, _ -> Buffer.of_msgpack msg
    | Window, _ -> Window.of_msgpack msg
    | Tabpage, _ -> Tabpage.of_msgpack msg
    | Object, _ -> Ok msg
    | Custom (module M), obj -> M.of_msgpack obj
    | _ ->
      Or_error.error_s [%message err_msg (witness : _ Types.Phantom.t) (msg : Msgpack.t)]
;;

let string ?(err_msg = "called [extract_string] on non-string") =
  value ~err_msg Types.Phantom.String
;;

let int ?(err_msg = "called [extract_int] on non-int") =
  value ~err_msg Types.Phantom.Integer
;;

let bool ?(err_msg = "called [extract_bool] on non-bool") =
  value ~err_msg Types.Phantom.Boolean
;;

let map_of_msgpack_map =
  let extract_key s =
    match s with
    | String s, i -> Ok (s, i)
    | _ -> Or_error.error_string "map key is not string"
  in
  function
  | Map kvs ->
    let open Or_error.Let_syntax in
    let%bind stringed = List.map kvs ~f:extract_key |> Or_error.combine_errors in
    String.Map.of_alist_or_error stringed
  | _ -> Or_error.error_string "called [map_of_msgpack_map] on a non-map"
;;

let and_convert_optional map key transform =
  match Map.find map key with
  | Some x -> Or_error.map (transform x) ~f:(fun y -> Some y)
  | None -> Ok None
;;
