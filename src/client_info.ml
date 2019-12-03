open Core

module Version = struct
  type t = Types.Client_info.version =
    { major : int option
    ; minor : int option
    ; patch : int option
    ; prerelease : string option
    ; commit : string option
    }
end

module Client_type = struct
  type t =
    [ `Remote
    | `Ui
    | `Embedder
    | `Host
    | `Plugin
    ]
end

module Client_method = struct
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
[@@deriving sexp_of]

let convert_version obj =
  let open Or_error.Let_syntax in
  let%bind m = Extract.map_of_msgpack_map obj in
  let%bind major = Extract.and_convert_optional m "major" Extract.int in
  let%bind minor = Extract.and_convert_optional m "minor" Extract.int in
  let%bind patch = Extract.and_convert_optional m "patch" Extract.int in
  let%bind prerelease = Extract.and_convert_optional m "prerelease" Extract.string in
  let%bind commit = Extract.and_convert_optional m "commit" Extract.string in
  return { Types.Client_info.major; minor; patch; prerelease; commit }
;;

let convert_methods =
  let open Or_error.Let_syntax in
  let convert_single obj =
    let%bind m = Extract.map_of_msgpack_map obj in
    let%bind async = Extract.and_convert_optional m "async" Extract.bool in
    let%bind nargs =
      Extract.and_convert_optional m "nargs" (function
        | Integer i -> Ok (`Fixed i)
        | Array [ Integer lo; Integer hi ] -> Ok (`Range (lo, hi))
        | _ -> Or_error.error_string "malformed nargs")
    in
    let%bind opts =
      match Map.find m "attributes" with
      | None -> Ok String.Map.empty
      | Some (Map objs) ->
        let%bind map =
          List.map objs ~f:(function
            | String a, b -> Ok (a, b)
            | _ -> Or_error.error_string "malformed attributes")
          |> Or_error.combine_errors
        in
        String.Map.of_alist_or_error map
      | _ -> Or_error.error_string "malformed attributes"
    in
    return { Types.Client_info.async = Option.value ~default:false async; nargs; opts }
  in
  function
  | Msgpack.Map kvs ->
    let%bind strings_to_methods =
      List.map kvs ~f:(fun (a, b) ->
        let%bind s = Extract.string a in
        let%bind mthd = convert_single b in
        Ok (s, mthd))
      |> Or_error.combine_errors
    in
    String.Map.of_alist_or_error strings_to_methods
  | _ -> Or_error.error_string "malformed method"
;;

let convert_attrs =
  let open Or_error.Let_syntax in
  function
  | Msgpack.Map kvs ->
    let%bind result =
      List.map kvs ~f:(function
        | Msgpack.String a, Msgpack.String b -> Ok (a, b)
        | _, _ -> Or_error.error_string "malformed attribute map")
      |> Or_error.combine_errors
    in
    String.Map.of_alist_or_error result
  | _ -> Or_error.error_string "malformed attribute map"
;;

let convert_type = function
  | Msgpack.String "remote" -> Ok `Remote
  | String "ui" -> Ok `Ui
  | String "embedder" -> Ok `Embedder
  | String "host" -> Ok `Host
  | String "plugin" -> Ok `Plugin
  | _ -> Or_error.error_string "malformed client type"
;;

let of_msgpack obj =
  let open Or_error.Let_syntax in
  let%bind m = Extract.map_of_msgpack_map obj in
  let%bind version = Extract.and_convert_optional m "version" convert_version in
  let%bind methods =
    match Map.find m "methods" with
    | None -> return String.Map.empty
    | Some obj -> convert_methods obj
  in
  let%bind attributes =
    match Map.find m "attributes" with
    | Some obj -> convert_attrs obj
    | None -> return String.Map.empty
  in
  let%bind name = Extract.and_convert_optional m "name" Extract.string in
  let%bind type_ = Extract.and_convert_optional m "type" convert_type in
  return { Types.Client_info.version; methods; attributes; name; type_ }
;;
