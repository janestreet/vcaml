open Core
open Import0

module Version = struct
  type t =
    { major : int option
    ; minor : int option
    ; patch : int option
    ; prerelease : string option
    ; commit : string option
    }
  [@@deriving fields ~iterators:to_list, sexp_of]

  let of_msgpack_map map =
    let open Or_error.Let_syntax in
    let%bind major = find_and_convert map "major" (Type.of_msgpack Int) in
    let%bind minor = find_and_convert map "minor" (Type.of_msgpack Int) in
    let%bind patch = find_and_convert map "patch" (Type.of_msgpack Int) in
    let%bind prerelease = find_and_convert map "prerelease" (Type.of_msgpack String) in
    let%bind commit = find_and_convert map "commit" (Type.of_msgpack String) in
    return { major; minor; patch; prerelease; commit }
  ;;

  let to_msgpack_map t =
    let conv typ field =
      match Field.get field t with
      | None -> None
      | Some value -> Some (Field.name field, Type.to_msgpack typ value)
    in
    Fields.to_list
      ~major:(conv Int)
      ~minor:(conv Int)
      ~patch:(conv Int)
      ~prerelease:(conv String)
      ~commit:(conv String)
    |> List.filter_opt
    |> String.Map.of_alist_exn
  ;;
end

module Client_type = struct
  type t =
    | Remote
    | Ui
    | Embedder
    | Host
    | Plugin
  [@@deriving sexp_of]

  let of_msgpack msgpack =
    let open Or_error.Let_syntax in
    match%bind Type.of_msgpack String msgpack with
    | "remote" -> Ok Remote
    | "ui" -> Ok Ui
    | "embedder" -> Ok Embedder
    | "host" -> Ok Host
    | "plugin" -> Ok Plugin
    | client_type -> Or_error.error_s [%message "Unrecognized client type" client_type]
  ;;

  let to_string = function
    | Remote -> "remote"
    | Ui -> "ui"
    | Embedder -> "embedder"
    | Host -> "host"
    | Plugin -> "plugin"
  ;;
end

module How_to_call_method = struct
  type t =
    { async : bool option
    ; nargs : [ `Fixed of int | `Inclusive_range of int * int ] option [@sexp.option]
    }
  [@@deriving fields ~iterators:to_list, sexp_of]

  let of_msgpack msgpack =
    let open Or_error.Let_syntax in
    let%bind map = Type.of_msgpack Dict msgpack in
    let%bind async = find_and_convert map "async" (Type.of_msgpack Bool) in
    let%bind nargs =
      find_and_convert map "nargs" (function
        | Int n -> Ok (`Fixed n)
        | Array [ Int lo; Int hi ] -> Ok (`Inclusive_range (lo, hi))
        | nargs -> Or_error.error_s [%message "Malformed [nargs]" (nargs : Msgpack.t)])
    in
    return { async; nargs }
  ;;

  let to_msgpack t =
    let conv to_msgpack field =
      match Field.get field t with
      | None -> None
      | Some value -> Some (Field.name field, to_msgpack value)
    in
    Fields.to_list
      ~async:(conv (Type.to_msgpack Bool))
      ~nargs:
        (conv (function
          | `Fixed n -> Msgpack.Int n
          | `Inclusive_range (lo, hi) -> Array [ Int lo; Int hi ]))
    |> List.filter_opt
    |> String.Map.of_alist_exn
    |> Type.to_msgpack Dict
  ;;
end

type t =
  { name : string option
  ; version : Version.t option
  ; client_type : Client_type.t option
  ; methods : How_to_call_method.t String.Map.t
  ; attributes : string String.Map.t
  }
[@@deriving sexp_of]

let convert_methods msgpack =
  let open Or_error.Let_syntax in
  let%bind map = Type.of_msgpack Dict msgpack in
  map |> Map.map ~f:How_to_call_method.of_msgpack |> Map.combine_errors
;;

let convert_attributes msgpack =
  let open Or_error.Let_syntax in
  let%bind map = Type.of_msgpack Dict msgpack in
  map |> Map.map ~f:(Type.of_msgpack String) |> Map.combine_errors
;;

let of_msgpack msgpack =
  let open Or_error.Let_syntax in
  let%bind map = Type.of_msgpack Dict msgpack in
  let%bind name = find_and_convert map "name" (Type.of_msgpack String) in
  let%bind version =
    match%bind find_and_convert map "version" (Type.of_msgpack Dict) with
    | None -> return None
    | Some version -> Version.of_msgpack_map version >>| Option.return
  in
  let%bind client_type = find_and_convert map "type" Client_type.of_msgpack in
  let%bind methods =
    find_and_convert map "methods" convert_methods
    >>| Option.value ~default:String.Map.empty
  in
  let%bind attributes =
    find_and_convert map "attributes" convert_attributes
    >>| Option.value ~default:String.Map.empty
  in
  return { name; version; client_type; methods; attributes }
;;
