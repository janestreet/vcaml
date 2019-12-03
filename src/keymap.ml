open Core

type t =
  { silent : bool
  ; noremap : bool
  ; mode : string
  ; nowait : bool
  ; expr : bool
  ; sid : int
  ; lhs : string
  ; rhs : string
  ; buffer : Types.Buf.t option
  }

let bool_of_int = function
  | 0 -> false
  | _ -> true
;;

module Untested = struct
  (* We don't currently expose [to_msgpack] because neovim doesn't expose [set_keymap]. *)
  let of_msgpack ?to_buf msg =
    let open Or_error.Let_syntax in
    let to_or_error = function
      | Some i -> Ok i
      | None -> Or_error.error_string "malformed keycommand message"
    in
    let%bind result = Extract.map_of_msgpack_map msg in
    let lookup_bool_of_int key =
      let%bind msg = Map.find result key |> to_or_error in
      Extract.int msg >>| bool_of_int
    in
    let%bind silent = lookup_bool_of_int "silent" in
    let%bind noremap = lookup_bool_of_int "noremap" in
    let%bind mode = Map.find result "mode" |> to_or_error >>= Extract.string in
    let%bind nowait = lookup_bool_of_int "nowait" in
    let%bind expr = lookup_bool_of_int "expr" in
    let%bind sid = Map.find result "sid" |> to_or_error >>= Extract.int in
    let%bind lhs = Map.find result "lhs" |> to_or_error >>= Extract.string in
    let%bind rhs = Map.find result "rhs" |> to_or_error >>= Extract.string in
    let%bind buffer =
      match to_buf with
      | None -> Ok None
      | Some f ->
        (match Map.find result "buffer" with
         | None -> to_or_error None
         (* Buffer indexing starts at 1, so a buffer number of 0 signals that this is a global
            command. *)
         | Some (Msgpack.Integer 0) -> Ok None
         | Some b ->
           (match f b with
            | Ok buf -> Ok (Some buf)
            | Error e -> Error e))
    in
    return { silent; noremap; mode; nowait; expr; sid; lhs; rhs; buffer }
  ;;
end
