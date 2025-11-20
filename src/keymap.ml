module Unshadow = struct
  module Buffer = Buffer
end

open Core
open Async
open Import
open Unshadow

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
    | mode -> raise_s [%message "Unrecognized mode" (mode : string)] [@nontail]
  ;;

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

let scope_of_msgpack msgpack =
  (* Buffer indexing starts at 1, so a buffer number of 0 indicates a global mapping. *)
  match (msgpack : Msgpack.t) with
  | Int 0 -> Ok `Global
  | _ ->
    let open Or_error.Let_syntax in
    let%map buffer = Type.of_msgpack Buffer msgpack in
    `Buffer_local buffer
;;

(* Fields are ordered in the way that would be most useful for reading a sorted list of
   mappings. *)
type t =
  { lhs : string
  ; mode : Mode.t
  ; scope : [ `Global | `Buffer_local of Nvim_internal.Buffer.t ]
  ; description : string option [@sexp.option]
  ; rhs : string
  ; expr : [ `Replace_keycodes of bool ] option [@sexp.option]
  ; nowait : bool
  ; silent : bool
  ; recursive : bool
  ; script_id : int
  }
[@@deriving compare, sexp_of]

let of_msgpack_map map =
  let open Or_error.Let_syntax in
  let find_key = find_or_error_and_convert in
  let%bind modes =
    let%bind modes = find_key map "mode" (Type.of_msgpack String) in
    List.map (String.to_list modes) ~f:(fun mode ->
      Or_error.try_with (fun () -> Mode.of_string (Char.to_string mode)))
    |> Or_error.combine_errors
  in
  let%bind silent = find_key map "silent" (Type.of_msgpack Bool) in
  let%bind noremap = find_key map "noremap" (Type.of_msgpack Bool) in
  let%bind nowait = find_key map "nowait" (Type.of_msgpack Bool) in
  let%bind expr =
    match%bind find_key map "expr" (Type.of_msgpack Bool) with
    | false -> return None
    | true ->
      let%map replace_keycodes =
        find_and_convert map "replace_keycodes" (Type.of_msgpack Bool)
        >>| Option.value ~default:false
      in
      Some (`Replace_keycodes replace_keycodes)
  in
  let%bind script_id = find_key map "sid" (Type.of_msgpack Int) in
  let%bind lhs = find_key map "lhs" (Type.of_msgpack String) in
  let%bind rhs = find_key map "rhs" (Type.of_msgpack String) in
  let%bind description = find_and_convert map "desc" (Type.of_msgpack String) in
  let%bind scope = find_key map "buffer" scope_of_msgpack in
  let recursive = not noremap in
  let keymaps =
    List.map modes ~f:(fun mode ->
      { description; lhs; rhs; mode; scope; expr; nowait; silent; recursive; script_id })
  in
  return keymaps
;;

(* The semantics of [nvim_get_keymap] and [nvim_buf_get_keymap] are a bit undocumented and
   unintuitive in the following ways:

   1. Simple-mode queries return all mappings that apply in that mode. However, language
      mappings are not returned in queries for insert and command modes. To fix this, we
      need to explicitly query for language mappings and join the results.

   2. There is no way to query for mappings defined with '!' (see `:h mapmode-ic`). These
      queries (appear to) work by taking the first character in the query, returning the
      appropriate mappings if the character is a recognized mode, and returning mappings
      for mapmode-nvo by default. '!' is not a recognized mode, so it silently returns
      mappings for 'nvo' instead of 'ic'. To fix this, we need to individually query for
      'i' and 'c' (and 'l') and join the results together. *)
let get ~(here : [%call_pos]) client ~scope ~mode =
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
  modes
  |> Deferred.Or_error.List.concat_map ~how:`Sequential ~f:(fun mode ->
    query ~mode:(Mode.to_string mode)
    |> map_witness ~f:(fun keymaps ->
      keymaps
      |> List.map ~f:of_msgpack_map
      |> Or_error.combine_errors
      |> Or_error.map ~f:List.concat)
    |> run ~here client)
  (* Because 'i' and 'c' will produce duplicate entries for '!' mappings, we need to dedup
     the results after querying each. *)
  >>|? List.dedup_and_sort ~compare
;;

let set_internal
  (type a)
  ~(return_type : a Type.t)
  ~expr
  ~here
  client
  ?(replace_keycodes = true)
  ?(recursive = false)
  ?(unique = false)
  ?(nowait = false)
  ?(silent = false)
  ?(description = "")
  ~mode
  ~scope
  ~lhs
  ~(rhs : a Ocaml_from_nvim.Callback.t)
  ()
  =
  let%bind.Deferred.Or_error scope =
    match scope with
    | `Global | `Buffer_local (Buffer.Or_current.Id _) -> Deferred.Or_error.return scope
    | `Buffer_local Current ->
      let%map.Deferred.Or_error buffer = Nvim.get_current_buf client in
      `Buffer_local (Buffer.Or_current.Id buffer)
  in
  let%bind rhs =
    match (rhs : a Ocaml_from_nvim.Callback.t) with
    | Viml rhs -> return rhs
    | Rpc rpc ->
      let name =
        Ocaml_from_nvim.Private.register_callback ~here client ~return_type rpc
      in
      let channel = Client.channel client in
      let%map () =
        match scope with
        | `Global -> return ()
        | `Buffer_local buffer ->
          Autocmd.create
            ~here
            client
            ~description:[%string "Unregister %{name}"]
            ~once:true
            ~patterns_or_buffer:(Buffer buffer)
            ~group:(Autocmd.Private.vcaml_internal_group client)
            ~events:[ BufDelete; BufWipeout ]
            (Viml
               [%string
                 {|
if !empty(nvim_get_chan_info(%{channel#Int}))
  call rpcnotify(%{channel#Int}, "%{Client.Private.unregister_blocking_rpc}", "%{name}")
endif |}])
          |> Deferred.ignore_m
      in
      (match expr with
       | true -> [%string {|rpcrequest(%{channel#Int}, "%{name}")|}]
       | false -> [%string {|<Cmd>call rpcrequest(%{channel#Int}, "%{name}")<CR>|}])
  in
  let query =
    match scope with
    | `Global -> Nvim_internal.nvim_set_keymap
    | `Buffer_local buffer -> Nvim_internal.nvim_buf_set_keymap ~buffer
  in
  let mode = Mode.to_string mode in
  let opts =
    let boolean_opts =
      let opts =
        [ "expr", expr
        ; "noremap", not recursive
        ; "nowait", nowait
        ; "silent", silent
        ; "unique", unique
        ]
      in
      let opts =
        match expr with
        | false -> opts
        | true -> ("replace_keycodes", replace_keycodes) :: opts
      in
      List.map opts ~f:(fun (key, value) -> key, Msgpack.Bool value)
    in
    match description with
    | "" -> boolean_opts
    | _ -> ("desc", String description) :: boolean_opts
  in
  query ~mode ~lhs ~rhs ~opts:(String.Map.of_alist_exn opts) |> run ~here client
;;

let set ~(here : [%call_pos]) client =
  set_internal ~return_type:Nil ~expr:false ~here client ?replace_keycodes:None
;;

let set_expr ~(here : [%call_pos]) client =
  set_internal ~return_type:String ~expr:true ~here client
;;

let unset ~(here : [%call_pos]) client ~scope ~lhs ~mode =
  let query =
    match scope with
    | `Global -> Nvim_internal.nvim_del_keymap
    | `Buffer_local buffer -> Nvim_internal.nvim_buf_del_keymap ~buffer
  in
  let mode = Mode.to_string mode in
  query ~mode ~lhs |> run ~here client
;;
