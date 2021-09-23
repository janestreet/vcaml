open Core

module T = struct
  type t =
    { id : int
    ; name : string option
    }
  [@@deriving compare, fields, hash, sexp_of]
end

include T

module Untested = struct
  let create ?name () =
    Nvim_internal.nvim_create_namespace ~name:(Option.value name ~default:"")
    |> Api_call.of_api_result
    |> Api_call.Or_error.map ~f:(fun id -> { id; name })
  ;;

  let all_named =
    Nvim_internal.nvim_get_namespaces
    |> Api_call.of_api_result
    |> Api_call.map_bind ~f:(fun alist ->
      let open Or_error.Let_syntax in
      Extract.map_of_msgpack_alist alist
      >>| Map.map ~f:Extract.int
      >>= Map.combine_errors
      >>| Map.mapi ~f:(fun ~key:name ~data:id -> { id; name = Some name }))
  ;;
end

include Comparable.Make_plain (T)
include Hashable.Make_plain (T)
