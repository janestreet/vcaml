open Core
open Import

module T = struct
  type t =
    { id : int
    ; name : string option
    }
  [@@deriving compare, fields ~getters, hash, sexp_of]
end

include T

let create ~(here : [%call_pos]) client ?name () =
  Nvim_internal.nvim_create_namespace ~name:(Option.value name ~default:"")
  |> map_witness ~f:(fun id -> Ok { id; name })
  |> run ~here client
;;

let all_named ~(here : [%call_pos]) client =
  Nvim_internal.nvim_get_namespaces
  |> map_witness ~f:(fun map ->
    map
    |> Map.map ~f:(Type.of_msgpack Int)
    |> Map.combine_errors
    |> Or_error.map ~f:(Map.mapi ~f:(fun ~key:name ~data:id -> { id; name = Some name })))
  |> run ~here client
;;

include Comparable.Make_plain (T)
include Hashable.Make_plain (T)
