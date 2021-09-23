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
    | " " -> Normal_and_visual_and_operator_pending
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

let of_msgpack msg =
  let open Or_error.Let_syntax in
  let to_or_error = function
    | Some i -> Ok i
    | None -> Or_error.error_string "malformed keycommand message"
  in
  let%bind result = Extract.map_of_msgpack_map msg in
  let lookup key extract = Map.find result key |> to_or_error >>= extract in
  let%bind modes =
    lookup "mode" (fun msg ->
      let%bind modes = Extract.string msg in
      List.map (String.to_list modes) ~f:(fun mode ->
        Mode.of_string_or_error (Char.to_string mode))
      |> Or_error.combine_errors)
  in
  let%bind silent = lookup "silent" Extract.bool in
  let%bind noremap = lookup "noremap" Extract.bool in
  let%bind nowait = lookup "nowait" Extract.bool in
  let%bind expr = lookup "expr" Extract.bool in
  let%bind sid = lookup "sid" Extract.int in
  let%bind lhs = lookup "lhs" Extract.string in
  let%bind rhs = lookup "rhs" Extract.string in
  let%bind scope = lookup "buffer" scope_of_msgpack in
  let recursive = not noremap in
  let keymaps =
    List.map modes ~f:(fun mode ->
      { lhs; rhs; mode; scope; expr; nowait; silent; recursive; sid })
  in
  return keymaps
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
  |> List.map ~f:(fun mode -> query ~mode:(Mode.to_string mode) |> Api_call.of_api_result)
  |> Api_call.Or_error.all
  >>| List.concat
  (* Because 'i' and 'c' will produce duplicate entries for '!' mappings, we need to
     dedup the results after querying each. *)
  >>| List.dedup_and_sort ~compare:[%compare: Msgpack.t]
  >>| List.map ~f:of_msgpack
  >>| Or_error.combine_errors
  >>| Or_error.map ~f:List.concat
  |> Api_call.map ~f:Or_error.join
;;

let set
      ?(recursive = false)
      ?(expr = false)
      ?(nowait = false)
      ?(silent = false)
      ~scope
      ~lhs
      ~rhs
      ~mode
      ()
  =
  let query =
    match scope with
    | `Global -> Nvim_internal.nvim_set_keymap
    | `Buffer_local buffer -> Nvim_internal.nvim_buf_set_keymap ~buffer
  in
  let mode = Mode.to_string mode in
  let opts =
    [ "noremap", not recursive; "expr", expr; "nowait", nowait; "silent", silent ]
    |> List.map ~f:(fun (key, value) -> Msgpack.String key, Msgpack.Boolean value)
  in
  query ~mode ~lhs ~rhs ~opts |> Api_call.of_api_result
;;

let unset ~scope ~lhs ~mode =
  let query =
    match scope with
    | `Global -> Nvim_internal.nvim_del_keymap
    | `Buffer_local buffer -> Nvim_internal.nvim_buf_del_keymap ~buffer
  in
  let mode = Mode.to_string mode in
  query ~mode ~lhs |> Api_call.of_api_result
;;
