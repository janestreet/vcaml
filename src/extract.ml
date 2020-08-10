open Core
open Msgpack

let convert_msgpack_error =
  Result.map_error ~f:(function
    | Msgpack.Array [ Integer _; String s ] ->
      Error.create "Vim returned error" s [%sexp_of: string]
    | m -> Error.create "Msgpack error response" m [%sexp_of: Msgpack.t])
;;

(* We don't actually need the [_to_msgpack] functions here, as it's valid to simply refer to the
   underlying objects via their handles (as raw integers).
*)
let rec inject : type t. t Types.Phantom.t -> t -> Msgpack.t =
  let open Types.Phantom in
  fun witness obj ->
    match witness with
    | Nil -> Msgpack.Nil
    | Integer -> Msgpack.Integer obj
    | Boolean -> Msgpack.Boolean obj
    | Dict -> Msgpack.Map obj
    | String -> Msgpack.String obj
    | Buffer -> Types.Buf.to_msgpack obj
    | Tabpage -> Types.Tabpage.to_msgpack obj
    | Window -> Types.Window.to_msgpack obj
    | Object -> obj
    | Array t' -> Msgpack.Array (List.map ~f:(inject t') obj)
    | Tuple (t', _) -> Msgpack.Array (List.map ~f:(inject t') obj)
    | Custom { to_msgpack; _ } -> to_msgpack obj
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
    | Buffer, Msgpack.Extension _ -> Types.Buf.of_msgpack msg
    | Window, Msgpack.Extension _ -> Types.Window.of_msgpack msg
    | Tabpage, Msgpack.Extension _ -> Types.Tabpage.of_msgpack msg
    | Object, _ -> Ok msg
    | Custom { of_msgpack; _ }, obj -> of_msgpack obj
    | _ -> Or_error.error_string err_msg
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
