open Core

(** Note that (1) this is not the full set of modes that [nvim_get_mode] can return, and
    (2) the string representations of these modes and the modes returned by
    [nvim_get_mode] are different. Thus we should not try to unify the types. *)
module Mode = struct
  type t =
    | Normal
    | Operator_pending
    | Insert
    | Cmd_line
    | Select
    | Visual
    | Terminal
    | Visual_and_select
    | Normal_and_visual_and_operator_pending
    | Insert_and_command_line
    | Language
  [@@deriving compare, enumerate, equal, sexp_of]

  let of_string = function
    | "" | " " -> Normal_and_visual_and_operator_pending
    | "!" -> Insert_and_command_line
    | "n" -> Normal
    | "v" -> Visual_and_select
    | "o" -> Operator_pending
    | "i" -> Insert
    | "c" -> Cmd_line
    | "s" -> Select
    | "x" -> Visual
    | "l" -> Language
    | "t" -> Terminal
    | mode -> raise_s [%message "Unrecognized mode" (mode : string)]
  ;;

  let of_string_or_error t = Or_error.try_with (fun () -> of_string t)

  let to_string = function
    | Normal -> "n"
    | Operator_pending -> "o"
    | Insert -> "i"
    | Cmd_line -> "c"
    | Select -> "s"
    | Visual -> "x"
    | Terminal -> "t"
    | Visual_and_select -> "v"
    | Normal_and_visual_and_operator_pending -> ""
    | Insert_and_command_line -> "!"
    | Language -> "l"
  ;;
end

let scope_of_msgpack msg =
  (* Buffer indexing starts at 1, so a buffer number of 0 indicates a global mapping. *)
  match (msg : Msgpack.t) with
  | Integer 0 -> Ok `Global
  | _ ->
    let open Or_error.Let_syntax in
    let%map buffer = Extract.value Buffer msg in
    `Buffer_local buffer
;;

type t =
  { lhs : string
  ; rhs : string
  ; mode : Mode.t
  ; scope : [ `Global | `Buffer_local of Nvim_internal.Buffer.t ]
  ; expr : bool
  ; nowait : bool
  ; silent : bool
  ; recursive : bool
  ; sid : int
  }
[@@deriving sexp_of]

(* We don't currently expose [to_msgpack] because neovim doesn't expose [set_keymap]. *)
let of_msgpack msg ~queried_mode =
  let open Or_error.Let_syntax in
  let to_or_error = function
    | Some i -> Ok i
    | None -> Or_error.error_string "malformed keycommand message"
  in
  let%bind result = Extract.map_of_msgpack_map msg in
  let lookup key extract = Map.find result key |> to_or_error >>= extract in
  let%bind mode =
    lookup "mode" (fun msg -> Extract.string msg >>= Mode.of_string_or_error)
  in
  let%bind silent = lookup "silent" Extract.bool in
  let%bind noremap = lookup "noremap" Extract.bool in
  let%bind nowait = lookup "nowait" Extract.bool in
  let%bind expr = lookup "expr" Extract.bool in
  let%bind sid = lookup "sid" Extract.int in
  let%bind lhs = lookup "lhs" Extract.string in
  let%bind rhs = lookup "rhs" Extract.string in
  let%bind scope = lookup "buffer" scope_of_msgpack in
  let mode =
    match mode, queried_mode with
    | Normal_and_visual_and_operator_pending, Mode.Terminal -> Mode.Terminal
    | _, _ -> mode
  in
  let recursive = not noremap in
  return { lhs; rhs; mode; scope; expr; nowait; silent; recursive; sid }
;;

(* The semantics of [nvim_get_keymap] and [nvim_buf_get_keymap] are a bit undocumented
   and unintuitive in the following ways:

   1. Simple-mode queries return all mappings that apply in that mode. However, language
   mappings are not returned in queries for insert and command modes. To fix this, we
   need to explicitly query for language mappings and join the results.

   2. There is no way to query for mappings defined with '!' (see `:h mapmode-ic`).
   These queries (appear to) work by taking the first character in the query, returning
   the appropriate mappings if the character is a recognized mode, and returning
   mappings for mapmode-nvo by default. '!' is not a recognized mode, so it silently
   returns mappings for 'nvo' instead of 'ic'. To fix this, we need to individually
   query for 'i' and 'c' (and 'l') and join the results together. *)
let get ~scope ~mode =
  let query =
    match scope with
    | `Global -> Nvim_internal.nvim_get_keymap
    | `Buffer_local buffer -> Nvim_internal.nvim_buf_get_keymap ~buffer
  in
  let modes =
    match (mode : Mode.t) with
    | Normal
    | Operator_pending
    | Select
    | Visual
    | Terminal
    | Visual_and_select
    | Normal_and_visual_and_operator_pending
    | Language -> [ mode ]
    | Insert -> [ Insert; Language ]
    | Cmd_line -> [ Cmd_line; Language ]
    | Insert_and_command_line -> [ Insert; Cmd_line; Language ]
  in
  let open Api_call.Or_error.Let_syntax in
  modes
  |> List.map ~f:(fun mode ->
    let%map keymaps = query ~mode:(Mode.to_string mode) |> Api_call.of_api_result in
    List.map keymaps ~f:(fun keymap -> mode, keymap))
  |> Api_call.Or_error.all
  >>| List.concat
  (* Because 'i' and 'c' will produce duplicate entries for '!' mappings, we need to
     dedup the results after querying each. *)
  >>| List.dedup_and_sort ~compare:[%compare: _ * Msgpack.t]
  >>| List.map ~f:(fun (queried_mode, msg) -> of_msgpack msg ~queried_mode)
  >>| Or_error.combine_errors
  |> Api_call.map ~f:Or_error.join
;;

let set
      ?(recursive = false)
      ?(expr = false)
      ?(nowait = false)
      ?(silent = false)
      ?(scope = `Global)
      ~lhs
      ~rhs
      ~mode
      ()
  =
  let expr = Option.some_if expr "<expr>" in
  let nowait = Option.some_if nowait "<nowait>" in
  let silent = Option.some_if silent "<silent>" in
  let buffer =
    match scope with
    | `Global -> None
    | `Current_buffer -> Some "<buffer>"
  in
  let mapping =
    let core = if recursive then "map" else "noremap" in
    match (mode : Mode.t) with
    | Insert_and_command_line -> sprintf "%s!" core
    | Normal
    | Operator_pending
    | Insert
    | Cmd_line
    | Select
    | Visual
    | Terminal
    | Visual_and_select
    | Normal_and_visual_and_operator_pending
    | Language -> sprintf !"%{Mode}%s" mode core
  in
  let command =
    [ Some mapping; expr; nowait; silent; buffer; Some lhs; Some rhs ]
    |> List.filter_opt
    |> String.concat ~sep:" "
  in
  Nvim_internal.nvim_command ~command |> Api_call.of_api_result
;;
