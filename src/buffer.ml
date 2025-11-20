open Core
open Async
open Import
include Nvim_internal.Buffer
module Event = Subscription_manager.Buffer_event

type changedtick = Event.changedtick [@@deriving sexp_of]

let int_of_changedtick = Type_equal.conv Event.Private.changedtick_eq
let changedtick_of_int = Type_equal.(conv (sym Event.Private.changedtick_eq))

module With_changedtick = struct
  type 'a t =
    { value : 'a
    ; changedtick : changedtick
    }
  [@@deriving sexp_of]

  let value t = t.value
end

let run_with_changedtick_check here client ?changedtick t api_result =
  match changedtick with
  | None -> run ~here client api_result
  | Some changedtick ->
    let changedtick = int_of_changedtick changedtick in
    let bufnr =
      match (t : Or_current.t) with
      | Current -> 0
      | Id t -> (t :> int)
    in
    let changedtick_now = sprintf "nvim_buf_get_var(%d, 'changedtick')" bufnr in
    (match%map
       run2
         ~here
         client
         (* Note that even though we call [echoerr] here, the semantics of Neovim's remote
            API are that when an error is encountered it's returned to the plugin over RPC
            and not displayed to the user or written to the message log. *)
         (Nvim_internal.nvim_exec2
            ~src:
              [%string
                "if %{changedtick#Int} < %{changedtick_now} | echoerr 'Buffer updated \
                 since changedtick (wanted %{changedtick#Int} but is now \
                 '.%{changedtick_now}.')' | endif"]
            ~opts:String.Map.empty)
         api_result
     with
     | Error _ as error -> error
     | Ok (_, result) -> Ok result)
;;

let run_and_get_changedtick here client t api_result =
  let%map.Deferred.Or_error value, changedtick =
    run2 ~here client api_result (Nvim_internal.nvim_buf_get_changedtick ~buffer:t)
  in
  { With_changedtick.value; changedtick = changedtick_of_int changedtick }
;;

let get_changedtick ~(here : [%call_pos]) client t =
  let%map.Deferred.Or_error changedtick =
    run ~here client (Nvim_internal.nvim_buf_get_changedtick ~buffer:t)
  in
  changedtick_of_int changedtick
;;

let get_name ~(here : [%call_pos]) client t =
  Nvim_internal.nvim_buf_get_name ~buffer:t |> run ~here client
;;

let set_name ~(here : [%call_pos]) client t name =
  Nvim_internal.nvim_buf_set_name ~buffer:t ~name |> run ~here client
;;

let get_lines ~(here : [%call_pos]) client t ~start ~end_ ~strict_indexing =
  Nvim_internal.nvim_buf_get_lines ~buffer:t ~start ~end_ ~strict_indexing
  |> map_witness ~f:(fun lines -> Ok (List.map lines ~f:String.Utf8.of_string_unchecked))
  |> run_and_get_changedtick here client t
;;

let set_lines
  ~(here : [%call_pos])
  client
  ?changedtick
  t
  ~start
  ~end_
  ~strict_indexing
  replacement
  =
  Nvim_internal.nvim_buf_set_lines ~buffer:t ~start ~end_ ~strict_indexing ~replacement
  |> run_with_changedtick_check here client ?changedtick t
;;

let get_text ~(here : [%call_pos]) client t ~start_row ~start_col ~end_row ~end_col =
  (* [opts] is not used by this version of Neovim, but may be used in the future. If we
     expose it, we should do so in a typeful way rather than asking the user to build
     [Msgpack.t] values. *)
  Nvim_internal.nvim_buf_get_text
    ~buffer:t
    ~start_row
    ~start_col
    ~end_row
    ~end_col
    ~opts:String.Map.empty
  |> map_witness ~f:(fun lines -> Ok (List.map lines ~f:String.Utf8.of_string_unchecked))
  |> run_and_get_changedtick here client t
;;

let set_text
  ~(here : [%call_pos])
  client
  ?changedtick
  t
  ~start_row
  ~start_col
  ~end_row
  ~end_col
  replacement
  =
  Nvim_internal.nvim_buf_set_text
    ~buffer:t
    ~start_row
    ~start_col
    ~end_row
    ~end_col
    ~replacement
  |> run_with_changedtick_check here client ?changedtick t
;;

let create ~(here : [%call_pos]) client ~listed ~scratch =
  Nvim_internal.nvim_create_buf ~listed ~scratch
  |> map_witness ~f:(fun t ->
    match (t :> int) with
    | 0 -> Or_error.error_string "nvim_create_buf failed (returned 0)"
    | _ -> Ok t)
  |> run ~here client
;;

let find_by_name_or_create ~(here : [%call_pos]) client name =
  Nvim_internal.nvim_call_function ~fn:"bufadd" ~args:[ String name ]
  |> map_witness ~f:of_msgpack
  |> run ~here client
;;

let nvim_buf_delete ~(here : [%call_pos]) client t ~only_unload ~even_if_modified =
  let opts =
    [ "force", Msgpack.Bool even_if_modified; "unload", Msgpack.Bool only_unload ]
    |> String.Map.of_alist_exn
  in
  (* This function's name is a bit misleading because it actually only ever does :bwipeout
     or :bunload depending on the passed options, but it never does :bdelete. *)
  Nvim_internal.nvim_buf_delete ~buffer:t ~opts |> run ~here client
;;

let unload = nvim_buf_delete ~only_unload:true
let wipeout = nvim_buf_delete ~only_unload:false

let loaded ~(here : [%call_pos]) client t =
  Nvim_internal.nvim_buf_is_loaded ~buffer:t |> run ~here client
;;

let exists ~(here : [%call_pos]) client t =
  Nvim_internal.nvim_buf_is_valid ~buffer:t |> run ~here client
;;

let get_buffer ~(here : [%call_pos]) client or_current =
  match (or_current : Or_current.t) with
  | Current -> run ~here client Nvim_internal.nvim_get_current_buf
  | Id id -> return (Ok id)
;;

(* When an entry for a buffer is absent from the [buffer_subscriptions] table, a
   subscription can begin. The state transitions to [Subscribing] to fail any new calls to
   [subscribe]. If the subscription fails, the entry is removed from the table. If it
   succeeds, the entry is advanced to the [Subscribed] state. When [subscribe] is called
   and the subscription is in the [Subscribed] state, the pipe may or may not be open. If
   it is open, the call fails. If it's closed, we transition the state to [Subscribing] to
   again fail any new calls to [subscribe], and fill the ivar that enables the
   resubscription once we have finished detaching. *)
let subscribe ~(here : [%call_pos]) client ?(send_buffer = true) t =
  let%bind.Deferred.Or_error t = get_buffer ~here client t in
  let subscription_manager =
    (Type_equal.(conv Client.Private.eq) client).subscription_manager
  in
  let%bind.Deferred.Or_error reader =
    Subscription_manager.subscribe_to_buffer subscription_manager ~buffer:t
  in
  let failed_to_attach ?error () =
    Pipe.close_read reader;
    Subscription_manager.cleanup_failed_buffer_subscription subscription_manager ~buffer:t;
    Or_error.error_s
      [%message.omit_nil
        "Failed to attach to buffer" ~buffer:(t : t) (error : Error.t option)]
  in
  (* As of Neovim 0.9.1, most of [opts] are Lua-specific. The one option that appeared
     possibly not to be, [preview], proved invalid in tests. *)
  match%map
    Nvim_internal.nvim_buf_attach ~buffer:(Id t) ~send_buffer ~opts:String.Map.empty
    |> run ~here client
  with
  | Ok false -> failed_to_attach ()
  | Error error -> failed_to_attach ~error ()
  | Ok true ->
    upon (Pipe.closed reader) (fun () ->
      don't_wait_for
        (Nvim_internal.nvim_buf_detach ~buffer:(Id t)
         |> run ~here:[%here] client
         |> Deferred.ignore_m));
    Ok reader
;;

let get_var ~(here : [%call_pos]) client t name ~type_ =
  Nvim_internal.nvim_buf_get_var ~buffer:t ~name
  |> map_witness ~f:(Type.of_msgpack type_)
  |> run ~here client
;;

let set_var ~(here : [%call_pos]) client t name ~type_ ~value =
  let value = Type.to_msgpack type_ value in
  Nvim_internal.nvim_buf_set_var ~buffer:t ~name ~value |> run ~here client
;;

let delete_var ~(here : [%call_pos]) client t name =
  Nvim_internal.nvim_buf_del_var ~buffer:t ~name |> run ~here client
;;

let get_mark ~(here : [%call_pos]) client t ~sym =
  Nvim_internal.nvim_buf_get_mark ~buffer:t ~name:(Char.to_string sym)
  |> map_witness ~f:(function
    | 0, 0 ->
      Or_error.error_s
        [%message "Mark not set in buffer" ~buffer:(t : Or_current.t) (sym : char)]
    | row, col -> Ok { Mark.sym; pos = { row; col } })
  |> run_and_get_changedtick here client t
;;

let set_mark ~(here : [%call_pos]) client ?changedtick t mark =
  (* [opts] is not used by this version of Neovim, but may be used in the future. If we
     expose it, we should do so in a typeful way rather than asking the user to build
     [Msgpack.t] values. *)
  Nvim_internal.nvim_buf_set_mark
    ~buffer:t
    ~name:(Char.to_string mark.Mark.sym)
    ~line:mark.pos.row
    ~col:mark.pos.col
    ~opts:String.Map.empty
  |> map_witness ~f:(function
    | true -> Ok ()
    | false ->
      Or_error.error_s
        [%message "Failed to set mark" ~buffer:(t : Or_current.t) (mark : Mark.t)])
  |> run_with_changedtick_check here client ?changedtick t
;;

let delete_mark ~(here : [%call_pos]) client t sym =
  Nvim_internal.nvim_buf_del_mark ~buffer:t ~name:(Char.to_string sym)
  |> map_witness ~f:(function
    | true -> Ok ()
    | false ->
      Or_error.error_s
        [%message "Mark not set in buffer" ~buffer:(t : Or_current.t) (sym : char)])
  |> run ~here client
;;

let line_count ~(here : [%call_pos]) client t =
  Nvim_internal.nvim_buf_line_count ~buffer:t |> run_and_get_changedtick here client t
;;

let get_byte_offset_of_line ~(here : [%call_pos]) client t ~line:index =
  Nvim_internal.nvim_buf_get_offset ~buffer:t ~index
  |> run_and_get_changedtick here client t
;;

module Untested = struct
  let add_highlight
    ~(here : [%call_pos])
    client
    ?changedtick
    t
    ~namespace
    ~hl_group
    ~line
    ~col_start
    ~col_end
    =
    Nvim_internal.nvim_buf_add_highlight
      ~buffer:t
      ~ns_id:(Namespace.id namespace)
      ~hl_group
      ~line
      ~col_start
      ~col_end
    |> run_with_changedtick_check here client ?changedtick t
    |> Deferred.Or_error.ignore_m
  ;;

  let clear_namespace ~(here : [%call_pos]) client t ~namespace ~line_start ~line_end =
    Nvim_internal.nvim_buf_clear_namespace
      ~buffer:t
      ~ns_id:(Namespace.id namespace)
      ~line_start
      ~line_end
    |> run ~here client
  ;;

  module Extmark = struct
    module T = struct
      type buffer = t [@@deriving compare, hash, sexp_of]

      type t =
        { id : int
        ; namespace : Namespace.t
        ; buffer : buffer
        }
      [@@deriving compare, hash, sexp_of]
    end

    include T
    include Comparable.Make_plain (T)
    include Hashable.Make_plain (T)
  end

  let set_extmark_internal
    ~map_witness_f
    ~(here : [%call_pos])
    client
    ~buffer
    ~namespace
    ?id
    ?changedtick
    ~start_inclusive
    ?end_exclusive
    ?hl_group
    ?virtual_text
    ?virtual_text_pos
    ?hide_virtual_text_when_overlaying_selection
    ?virtual_lines
    ?virtual_lines_pos
    ?bypass_sign_and_number_columns
    ?when_underlying_highlight_conflicts
    ?extend_highlight_across_screen
    ?ephemeral
    ?start_gravity
    ?end_gravity
    ?priority
    ?strict
    ?sign_text
    ?sign_hl_group
    ?number_hl_group
    ?line_hl_group
    ?cursorline_hl_group
    ?conceal
    ?spell
    ?ui_watched
    ()
    =
    let open Deferred.Or_error.Let_syntax in
    let%bind buffer = get_buffer ~here client buffer in
    let opts =
      let module M = Msgpack in
      (* [bind] is bind-like but passes from the option monad to the list monad. *)
      let bind value ~f = Option.map value ~f |> Option.value ~default:[] in
      let pack_highlighted_text text = M.Array (Highlighted_text.to_msgpack text) in
      [ bind id ~f:(fun id -> [ "id", M.Int id ])
      ; bind end_exclusive ~f:(fun { Position.row; col } ->
          [ "end_row", M.Int row; "end_col", Int col ])
      ; bind hl_group ~f:(fun hl_group -> [ "hl_group", M.String hl_group ])
      ; bind virtual_text ~f:(fun virtual_text ->
          [ "virt_text", pack_highlighted_text virtual_text ])
      ; bind virtual_text_pos ~f:(function
          | `Eol -> [ "virt_text_pos", M.String "eol" ]
          | `Overlay -> [ "virt_text_pos", String "overlay" ]
          | `Right_align -> [ "virt_text_pos", String "right_align" ]
          | `At_column col -> [ "virt_text_win_col", Int col ])
      ; bind hide_virtual_text_when_overlaying_selection ~f:(fun hide_virtual_text ->
          [ "virt_text_hide", M.Bool hide_virtual_text ])
      ; bind virtual_lines ~f:(fun virtual_lines ->
          [ "virt_lines", virtual_lines |> List.map ~f:pack_highlighted_text |> M.Array ])
      ; bind virtual_lines_pos ~f:(fun pos ->
          let virtual_lines_above =
            match pos with
            | `Above -> true
            | `Below -> false
          in
          [ "virt_lines_above", M.Bool virtual_lines_above ])
      ; bind bypass_sign_and_number_columns ~f:(fun virtual_lines_leftcol ->
          [ "virt_lines_leftcol", M.Bool virtual_lines_leftcol ])
      ; bind when_underlying_highlight_conflicts ~f:(fun what_to_do ->
          let hl_mode =
            match what_to_do with
            | `Override -> "replace"
            | `Combine_with_bg -> "combine"
            | `Blend -> "blend"
          in
          [ "hl_mode", M.String hl_mode ])
      ; bind extend_highlight_across_screen ~f:(fun hl_eol -> [ "hl_eol", M.Bool hl_eol ])
      ; bind ephemeral ~f:(fun ephemeral -> [ "ephemeral", M.Bool ephemeral ])
      ; bind start_gravity ~f:(fun gravity ->
          let right_gravity =
            match gravity with
            | `Right -> true
            | `Left -> false
          in
          [ "right_gravity", M.Bool right_gravity ])
      ; bind end_gravity ~f:(fun end_gravity ->
          let end_right_gravity =
            match end_gravity with
            | `Right -> true
            | `Left -> false
          in
          [ "end_right_gravity", M.Bool end_right_gravity ])
      ; bind priority ~f:(fun priority -> [ "priority", M.Int priority ])
      ; bind strict ~f:(fun strict -> [ "strict", M.Bool strict ])
      ; bind sign_text ~f:(fun sign_text -> [ "sign_text", M.String sign_text ])
      ; bind sign_hl_group ~f:(fun sign_hl_group ->
          [ "sign_hl_group", M.String sign_hl_group ])
      ; bind number_hl_group ~f:(fun number_hl_group ->
          [ "number_hl_group", M.String number_hl_group ])
      ; bind line_hl_group ~f:(fun line_hl_group ->
          [ "line_hl_group", M.String line_hl_group ])
      ; bind cursorline_hl_group ~f:(fun cursorline_hl_group ->
          [ "cursorline_hl_group", M.String cursorline_hl_group ])
      ; bind conceal ~f:(fun conceal ->
          [ ( "conceal"
            , M.String
                (match conceal with
                 | `With_default -> ""
                 | `With ch -> Char.to_string ch) )
          ])
      ; bind spell ~f:(fun spell -> [ "spell", M.Bool spell ])
      ; bind ui_watched ~f:(fun ui_watched -> [ "ui_watched", M.Bool ui_watched ])
      ]
      |> List.concat
      |> String.Map.of_alist_exn
    in
    let { Position.row; col } = start_inclusive in
    Nvim_internal.nvim_buf_set_extmark
      ~buffer:(Id buffer)
      ~ns_id:(Namespace.id namespace)
      ~line:row
      ~col
      ~opts
    |> map_witness ~f:(map_witness_f ~buffer)
    |> run_with_changedtick_check here client ?changedtick (Id buffer)
  ;;

  let create_extmark ~(here : [%call_pos]) client ?changedtick buffer ~namespace =
    set_extmark_internal
      ~map_witness_f:(fun ~buffer id -> Ok { Extmark.id; namespace; buffer })
      ~here
      client
      ~buffer
      ~namespace
      ?id:None
      ?changedtick
  ;;

  let update_extmark
    ~(here : [%call_pos])
    client
    ?changedtick
    { Extmark.id; namespace; buffer }
    =
    set_extmark_internal
      ~map_witness_f:(fun ~buffer:(_ : t) (_ : int) -> Ok ())
      ~here
      client
      ~buffer:(Id buffer)
      ~namespace
      ~id
      ?changedtick
  ;;

  let delete_extmark ~(here : [%call_pos]) client extmark =
    Nvim_internal.nvim_buf_del_extmark
      ~buffer:(Id extmark.Extmark.buffer)
      ~ns_id:(Namespace.id extmark.namespace)
      ~id:extmark.id
    |> map_witness ~f:(function
      | true -> Ok ()
      | false -> Or_error.error_s [%message "Invalid extmark" ~_:(extmark : Extmark.t)])
    |> run ~here client
  ;;

  let get_extmark ~(here : [%call_pos]) client { Extmark.id; namespace; buffer } =
    let opts = [ "details", Msgpack.Bool false ] |> String.Map.of_alist_exn in
    Nvim_internal.nvim_buf_get_extmark_by_id
      ~buffer:(Id buffer)
      ~ns_id:(Namespace.id namespace)
      ~id
      ~opts
    |> map_witness ~f:(function
      | [] -> Ok None
      | [ row; col ] -> Ok (Some { Position.row; col })
      | _ -> Or_error.error_string "malformed result for [nvim_buf_get_extmark_by_id]")
    |> run_and_get_changedtick here client (Id buffer)
  ;;

  let get_extmark_with_details
    ~(here : [%call_pos])
    client
    ?hl_groups
    { Extmark.id; namespace; buffer }
    =
    let opts =
      let maybe name var conv = Option.map var ~f:(fun var -> name, conv var) in
      [ Some ("details", Msgpack.Bool true)
      ; maybe "hl_name" hl_groups (function
          | `Names -> Msgpack.Bool true
          | `Ids -> Bool false)
      ]
      |> List.filter_opt
      |> String.Map.of_alist_exn
    in
    let api_result =
      Nvim_internal.nvim_buf_get_extmark_by_id
        ~buffer:(Id buffer)
        ~ns_id:(Namespace.id namespace)
        ~id
        ~opts
    in
    (* The API info is misleading in this case - it says [Array(Integer)], but when
       [details] is provided the last item in the array is a dictionary. *)
    { api_result with witness = Array Object }
    |> map_witness ~f:(function
      | [] -> Ok None
      | [ Int row; Int col; details ] ->
        let%map.Or_error details = Type.of_msgpack Dict details in
        Some ({ Position.row; col }, details)
      | _ -> Or_error.error_string "malformed result for [nvim_buf_get_extmark_by_id]")
    |> run_and_get_changedtick here client (Id buffer)
  ;;

  let all_extmarks_internal
    ~map_witness_f
    ~details
    ~(here : [%call_pos])
    client
    ?start_inclusive
    ?end_inclusive
    ?limit
    ?type_
    ?hl_groups
    t
    ~namespace
    =
    let%bind.Deferred.Or_error buffer = get_buffer ~here client t in
    let opts =
      let maybe name var conv = Option.map var ~f:(fun var -> name, conv var) in
      [ Some ("details", Msgpack.Bool details)
      ; maybe "hl_name" hl_groups (function
          | `Names -> Msgpack.Bool true
          | `Ids -> Bool false)
      ; maybe "type" type_ (function
          | `Highlight -> Msgpack.String "highlight"
          | `Sign -> String "sign"
          | `Virtual_text -> String "virt_text"
          | `Virtual_lines -> String "virt_lines")
      ; maybe "limit" limit (Type.to_msgpack Int)
      ]
      |> List.filter_opt
      |> String.Map.of_alist_exn
    in
    let start =
      match start_inclusive with
      | None -> Msgpack.Int 0
      | Some { Position.row; col } -> Array [ Int row; Int col ]
    in
    let end_ =
      match end_inclusive with
      | None -> Msgpack.Int (-1)
      | Some { Position.row; col } -> Array [ Int row; Int col ]
    in
    let api_result =
      Nvim_internal.nvim_buf_get_extmarks
        ~buffer:(Id buffer)
        ~ns_id:(Namespace.id namespace)
        ~start
        ~end_
        ~opts
    in
    (* The API info could be more informative here. *)
    { api_result with witness = Array (Array Object) }
    |> map_witness ~f:(map_witness_f ~buffer ~namespace)
    |> run_and_get_changedtick here client t
  ;;

  let all_extmarks ~(here : [%call_pos]) client =
    all_extmarks_internal
      ~map_witness_f:(fun ~buffer ~namespace extmarks ->
        extmarks
        |> List.map ~f:(function
          | [ Int id; Int row; Int col ] ->
            let extmark = { Extmark.id; namespace; buffer } in
            let pos = { Position.row; col } in
            Ok (extmark, pos)
          | _ -> Or_error.error_string "malformed result from [nvim_buf_get_extmarks]")
        |> Or_error.combine_errors)
      ~details:false
      ?hl_groups:None
      ~here
      client
  ;;

  let all_extmarks_with_details ~(here : [%call_pos]) client =
    all_extmarks_internal
      ~map_witness_f:(fun ~buffer ~namespace extmarks ->
        extmarks
        |> List.map ~f:(function
          | [ Int id; Int row; Int col; details ] ->
            let%map.Or_error details = Type.of_msgpack Dict details in
            let extmark = { Extmark.id; namespace; buffer } in
            let pos = { Position.row; col } in
            extmark, pos, details
          | _ -> Or_error.error_string "malformed result from [nvim_buf_get_extmarks]")
        |> Or_error.combine_errors)
      ~details:true
      ~here
      client
  ;;
end

module Option = struct
  (*$ Vcaml_cinaps.generate_options_impl ~scope:Buffer *)
  type ('a, 'global) t =
    | Autoindent : (bool, [ `copied ]) t
    | Autoread : (bool, [ `global ]) t
    | Backupcopy : (string list, [ `global ]) t
    | Binary : (bool, [ `copied ]) t
    | Bomb : (bool, [ `copied ]) t
    | Bufhidden : (string, [ `none ]) t
    | Buflisted : (bool, [ `copied ]) t
    | Buftype : (string, [ `none ]) t
    | Channel : (int, [ `none ]) t
    | Cindent : (bool, [ `copied ]) t
    | Cinkeys : (string list, [ `copied ]) t
    | Cinoptions : (string list, [ `copied ]) t
    | Cinscopedecls : (string list, [ `copied ]) t
    | Cinwords : (string list, [ `copied ]) t
    | Comments : (string list, [ `copied ]) t
    | Commentstring : (string, [ `copied ]) t
    | Complete : (string list, [ `copied ]) t
    | Completefunc : (string, [ `copied ]) t
    | Copyindent : (bool, [ `copied ]) t
    | Define : (string, [ `global ]) t
    | Dictionary : (string list, [ `global ]) t
    | Endoffile : (bool, [ `copied ]) t
    | Endofline : (bool, [ `copied ]) t
    | Equalprg : (string, [ `global ]) t
    | Errorformat : (string list, [ `global ]) t
    | Expandtab : (bool, [ `copied ]) t
    | Fileencoding : (string, [ `copied ]) t
    | Fileformat : (string, [ `copied ]) t
    | Filetype : (string, [ `none ]) t
    | Fixendofline : (bool, [ `copied ]) t
    | Formatexpr : (string, [ `copied ]) t
    | Formatlistpat : (string, [ `copied ]) t
    | Formatoptions : (char list, [ `copied ]) t
    | Formatprg : (string, [ `global ]) t
    | Grepprg : (string, [ `global ]) t
    | Iminsert : (int, [ `copied ]) t
    | Imsearch : (int, [ `copied ]) t
    | Include : (string, [ `global ]) t
    | Includeexpr : (string, [ `copied ]) t
    | Indentexpr : (string, [ `copied ]) t
    | Indentkeys : (string list, [ `copied ]) t
    | Infercase : (bool, [ `copied ]) t
    | Iskeyword : (string list, [ `copied ]) t
    | Keymap : (string, [ `copied ]) t
    | Keywordprg : (string, [ `global ]) t
    | Lisp : (bool, [ `copied ]) t
    | Lispoptions : (string list, [ `copied ]) t
    | Lispwords : (string list, [ `global ]) t
    | Makeencoding : (string, [ `global ]) t
    | Makeprg : (string, [ `global ]) t
    | Matchpairs : (string list, [ `copied ]) t
    | Modeline : (bool, [ `copied ]) t
    | Modifiable : (bool, [ `copied ]) t
    | Modified : (bool, [ `none ]) t
    | Nrformats : (string list, [ `copied ]) t
    | Omnifunc : (string, [ `copied ]) t
    | Path : (string list, [ `global ]) t
    | Preserveindent : (bool, [ `copied ]) t
    | Quoteescape : (string, [ `copied ]) t
    | Readonly : (bool, [ `none ]) t
    | Scrollback : (int, [ `copied ]) t
    | Shiftwidth : (int, [ `copied ]) t
    | Smartindent : (bool, [ `copied ]) t
    | Softtabstop : (int, [ `copied ]) t
    | Spellcapcheck : (string, [ `copied ]) t
    | Spellfile : (string list, [ `copied ]) t
    | Spelllang : (string list, [ `copied ]) t
    | Spelloptions : (string list, [ `copied ]) t
    | Suffixesadd : (string list, [ `copied ]) t
    | Swapfile : (bool, [ `copied ]) t
    | Synmaxcol : (int, [ `copied ]) t
    | Syntax : (string, [ `none ]) t
    | Tabstop : (int, [ `copied ]) t
    | Tagcase : (string, [ `global ]) t
    | Tagfunc : (string, [ `copied ]) t
    | Tags : (string list, [ `global ]) t
    | Textwidth : (int, [ `copied ]) t
    | Thesaurus : (string list, [ `global ]) t
    | Thesaurusfunc : (string, [ `global ]) t
    | Undofile : (bool, [ `copied ]) t
    | Undolevels : (int, [ `global ]) t
    | Varsofttabstop : (string list, [ `copied ]) t
    | Vartabstop : (string list, [ `copied ]) t
    | Wrapmargin : (int, [ `copied ]) t
  [@@deriving sexp_of]

  let to_string (type a g) : (a, g) t -> string = function
    | Autoindent -> "autoindent"
    | Autoread -> "autoread"
    | Backupcopy -> "backupcopy"
    | Binary -> "binary"
    | Bomb -> "bomb"
    | Bufhidden -> "bufhidden"
    | Buflisted -> "buflisted"
    | Buftype -> "buftype"
    | Channel -> "channel"
    | Cindent -> "cindent"
    | Cinkeys -> "cinkeys"
    | Cinoptions -> "cinoptions"
    | Cinscopedecls -> "cinscopedecls"
    | Cinwords -> "cinwords"
    | Comments -> "comments"
    | Commentstring -> "commentstring"
    | Complete -> "complete"
    | Completefunc -> "completefunc"
    | Copyindent -> "copyindent"
    | Define -> "define"
    | Dictionary -> "dictionary"
    | Endoffile -> "endoffile"
    | Endofline -> "endofline"
    | Equalprg -> "equalprg"
    | Errorformat -> "errorformat"
    | Expandtab -> "expandtab"
    | Fileencoding -> "fileencoding"
    | Fileformat -> "fileformat"
    | Filetype -> "filetype"
    | Fixendofline -> "fixendofline"
    | Formatexpr -> "formatexpr"
    | Formatlistpat -> "formatlistpat"
    | Formatoptions -> "formatoptions"
    | Formatprg -> "formatprg"
    | Grepprg -> "grepprg"
    | Iminsert -> "iminsert"
    | Imsearch -> "imsearch"
    | Include -> "include"
    | Includeexpr -> "includeexpr"
    | Indentexpr -> "indentexpr"
    | Indentkeys -> "indentkeys"
    | Infercase -> "infercase"
    | Iskeyword -> "iskeyword"
    | Keymap -> "keymap"
    | Keywordprg -> "keywordprg"
    | Lisp -> "lisp"
    | Lispoptions -> "lispoptions"
    | Lispwords -> "lispwords"
    | Makeencoding -> "makeencoding"
    | Makeprg -> "makeprg"
    | Matchpairs -> "matchpairs"
    | Modeline -> "modeline"
    | Modifiable -> "modifiable"
    | Modified -> "modified"
    | Nrformats -> "nrformats"
    | Omnifunc -> "omnifunc"
    | Path -> "path"
    | Preserveindent -> "preserveindent"
    | Quoteescape -> "quoteescape"
    | Readonly -> "readonly"
    | Scrollback -> "scrollback"
    | Shiftwidth -> "shiftwidth"
    | Smartindent -> "smartindent"
    | Softtabstop -> "softtabstop"
    | Spellcapcheck -> "spellcapcheck"
    | Spellfile -> "spellfile"
    | Spelllang -> "spelllang"
    | Spelloptions -> "spelloptions"
    | Suffixesadd -> "suffixesadd"
    | Swapfile -> "swapfile"
    | Synmaxcol -> "synmaxcol"
    | Syntax -> "syntax"
    | Tabstop -> "tabstop"
    | Tagcase -> "tagcase"
    | Tagfunc -> "tagfunc"
    | Tags -> "tags"
    | Textwidth -> "textwidth"
    | Thesaurus -> "thesaurus"
    | Thesaurusfunc -> "thesaurusfunc"
    | Undofile -> "undofile"
    | Undolevels -> "undolevels"
    | Varsofttabstop -> "varsofttabstop"
    | Vartabstop -> "vartabstop"
    | Wrapmargin -> "wrapmargin"
  ;;

  let of_msgpack (type a g) (t : (a, g) t) msgpack : a Or_error.t =
    let open Option_helpers in
    match t with
    | Autoindent -> Type.of_msgpack Bool msgpack
    | Autoread -> Type.of_msgpack Bool msgpack
    | Backupcopy -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Binary -> Type.of_msgpack Bool msgpack
    | Bomb -> Type.of_msgpack Bool msgpack
    | Bufhidden -> Type.of_msgpack String msgpack
    | Buflisted -> Type.of_msgpack Bool msgpack
    | Buftype -> Type.of_msgpack String msgpack
    | Channel -> Type.of_msgpack Int msgpack
    | Cindent -> Type.of_msgpack Bool msgpack
    | Cinkeys -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Cinoptions -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Cinscopedecls -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Cinwords -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Comments -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Commentstring -> Type.of_msgpack String msgpack
    | Complete -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Completefunc -> Type.of_msgpack String msgpack
    | Copyindent -> Type.of_msgpack Bool msgpack
    | Define -> Type.of_msgpack String msgpack
    | Dictionary -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Endoffile -> Type.of_msgpack Bool msgpack
    | Endofline -> Type.of_msgpack Bool msgpack
    | Equalprg -> Type.of_msgpack String msgpack
    | Errorformat -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Expandtab -> Type.of_msgpack Bool msgpack
    | Fileencoding -> Type.of_msgpack String msgpack
    | Fileformat -> Type.of_msgpack String msgpack
    | Filetype -> Type.of_msgpack String msgpack
    | Fixendofline -> Type.of_msgpack Bool msgpack
    | Formatexpr -> Type.of_msgpack String msgpack
    | Formatlistpat -> Type.of_msgpack String msgpack
    | Formatoptions -> Type.of_msgpack (Custom (module Char_list)) msgpack
    | Formatprg -> Type.of_msgpack String msgpack
    | Grepprg -> Type.of_msgpack String msgpack
    | Iminsert -> Type.of_msgpack Int msgpack
    | Imsearch -> Type.of_msgpack Int msgpack
    | Include -> Type.of_msgpack String msgpack
    | Includeexpr -> Type.of_msgpack String msgpack
    | Indentexpr -> Type.of_msgpack String msgpack
    | Indentkeys -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Infercase -> Type.of_msgpack Bool msgpack
    | Iskeyword -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Keymap -> Type.of_msgpack String msgpack
    | Keywordprg -> Type.of_msgpack String msgpack
    | Lisp -> Type.of_msgpack Bool msgpack
    | Lispoptions -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Lispwords -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Makeencoding -> Type.of_msgpack String msgpack
    | Makeprg -> Type.of_msgpack String msgpack
    | Matchpairs -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Modeline -> Type.of_msgpack Bool msgpack
    | Modifiable -> Type.of_msgpack Bool msgpack
    | Modified -> Type.of_msgpack Bool msgpack
    | Nrformats -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Omnifunc -> Type.of_msgpack String msgpack
    | Path -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Preserveindent -> Type.of_msgpack Bool msgpack
    | Quoteescape -> Type.of_msgpack String msgpack
    | Readonly -> Type.of_msgpack Bool msgpack
    | Scrollback -> Type.of_msgpack Int msgpack
    | Shiftwidth -> Type.of_msgpack Int msgpack
    | Smartindent -> Type.of_msgpack Bool msgpack
    | Softtabstop -> Type.of_msgpack Int msgpack
    | Spellcapcheck -> Type.of_msgpack String msgpack
    | Spellfile -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Spelllang -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Spelloptions -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Suffixesadd -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Swapfile -> Type.of_msgpack Bool msgpack
    | Synmaxcol -> Type.of_msgpack Int msgpack
    | Syntax -> Type.of_msgpack String msgpack
    | Tabstop -> Type.of_msgpack Int msgpack
    | Tagcase -> Type.of_msgpack String msgpack
    | Tagfunc -> Type.of_msgpack String msgpack
    | Tags -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Textwidth -> Type.of_msgpack Int msgpack
    | Thesaurus -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Thesaurusfunc -> Type.of_msgpack String msgpack
    | Undofile -> Type.of_msgpack Bool msgpack
    | Undolevels -> Type.of_msgpack Int msgpack
    | Varsofttabstop -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Vartabstop -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Wrapmargin -> Type.of_msgpack Int msgpack
  ;;

  let to_msgpack (type a g) (t : (a, g) t) (value : a) =
    let open Option_helpers in
    match t with
    | Autoindent -> Type.to_msgpack Bool value
    | Autoread -> Type.to_msgpack Bool value
    | Backupcopy -> Type.to_msgpack (Custom (module String_list)) value
    | Binary -> Type.to_msgpack Bool value
    | Bomb -> Type.to_msgpack Bool value
    | Bufhidden -> Type.to_msgpack String value
    | Buflisted -> Type.to_msgpack Bool value
    | Buftype -> Type.to_msgpack String value
    | Channel -> Type.to_msgpack Int value
    | Cindent -> Type.to_msgpack Bool value
    | Cinkeys -> Type.to_msgpack (Custom (module String_list)) value
    | Cinoptions -> Type.to_msgpack (Custom (module String_list)) value
    | Cinscopedecls -> Type.to_msgpack (Custom (module String_list)) value
    | Cinwords -> Type.to_msgpack (Custom (module String_list)) value
    | Comments -> Type.to_msgpack (Custom (module String_list)) value
    | Commentstring -> Type.to_msgpack String value
    | Complete -> Type.to_msgpack (Custom (module String_list)) value
    | Completefunc -> Type.to_msgpack String value
    | Copyindent -> Type.to_msgpack Bool value
    | Define -> Type.to_msgpack String value
    | Dictionary -> Type.to_msgpack (Custom (module String_list)) value
    | Endoffile -> Type.to_msgpack Bool value
    | Endofline -> Type.to_msgpack Bool value
    | Equalprg -> Type.to_msgpack String value
    | Errorformat -> Type.to_msgpack (Custom (module String_list)) value
    | Expandtab -> Type.to_msgpack Bool value
    | Fileencoding -> Type.to_msgpack String value
    | Fileformat -> Type.to_msgpack String value
    | Filetype -> Type.to_msgpack String value
    | Fixendofline -> Type.to_msgpack Bool value
    | Formatexpr -> Type.to_msgpack String value
    | Formatlistpat -> Type.to_msgpack String value
    | Formatoptions -> Type.to_msgpack (Custom (module Char_list)) value
    | Formatprg -> Type.to_msgpack String value
    | Grepprg -> Type.to_msgpack String value
    | Iminsert -> Type.to_msgpack Int value
    | Imsearch -> Type.to_msgpack Int value
    | Include -> Type.to_msgpack String value
    | Includeexpr -> Type.to_msgpack String value
    | Indentexpr -> Type.to_msgpack String value
    | Indentkeys -> Type.to_msgpack (Custom (module String_list)) value
    | Infercase -> Type.to_msgpack Bool value
    | Iskeyword -> Type.to_msgpack (Custom (module String_list)) value
    | Keymap -> Type.to_msgpack String value
    | Keywordprg -> Type.to_msgpack String value
    | Lisp -> Type.to_msgpack Bool value
    | Lispoptions -> Type.to_msgpack (Custom (module String_list)) value
    | Lispwords -> Type.to_msgpack (Custom (module String_list)) value
    | Makeencoding -> Type.to_msgpack String value
    | Makeprg -> Type.to_msgpack String value
    | Matchpairs -> Type.to_msgpack (Custom (module String_list)) value
    | Modeline -> Type.to_msgpack Bool value
    | Modifiable -> Type.to_msgpack Bool value
    | Modified -> Type.to_msgpack Bool value
    | Nrformats -> Type.to_msgpack (Custom (module String_list)) value
    | Omnifunc -> Type.to_msgpack String value
    | Path -> Type.to_msgpack (Custom (module String_list)) value
    | Preserveindent -> Type.to_msgpack Bool value
    | Quoteescape -> Type.to_msgpack String value
    | Readonly -> Type.to_msgpack Bool value
    | Scrollback -> Type.to_msgpack Int value
    | Shiftwidth -> Type.to_msgpack Int value
    | Smartindent -> Type.to_msgpack Bool value
    | Softtabstop -> Type.to_msgpack Int value
    | Spellcapcheck -> Type.to_msgpack String value
    | Spellfile -> Type.to_msgpack (Custom (module String_list)) value
    | Spelllang -> Type.to_msgpack (Custom (module String_list)) value
    | Spelloptions -> Type.to_msgpack (Custom (module String_list)) value
    | Suffixesadd -> Type.to_msgpack (Custom (module String_list)) value
    | Swapfile -> Type.to_msgpack Bool value
    | Synmaxcol -> Type.to_msgpack Int value
    | Syntax -> Type.to_msgpack String value
    | Tabstop -> Type.to_msgpack Int value
    | Tagcase -> Type.to_msgpack String value
    | Tagfunc -> Type.to_msgpack String value
    | Tags -> Type.to_msgpack (Custom (module String_list)) value
    | Textwidth -> Type.to_msgpack Int value
    | Thesaurus -> Type.to_msgpack (Custom (module String_list)) value
    | Thesaurusfunc -> Type.to_msgpack String value
    | Undofile -> Type.to_msgpack Bool value
    | Undolevels -> Type.to_msgpack Int value
    | Varsofttabstop -> Type.to_msgpack (Custom (module String_list)) value
    | Vartabstop -> Type.to_msgpack (Custom (module String_list)) value
    | Wrapmargin -> Type.to_msgpack Int value
  ;;

  module Global = struct
    type 'a t =
      | Copied : [ `copied ] t
      | Global : [ `global ] t
      | None : [ `none ] t
  end

  let kind (type a g) : (a, g) t -> g Global.t = function
    | Autoindent -> Copied
    | Autoread -> Global
    | Backupcopy -> Global
    | Binary -> Copied
    | Bomb -> Copied
    | Bufhidden -> None
    | Buflisted -> Copied
    | Buftype -> None
    | Channel -> None
    | Cindent -> Copied
    | Cinkeys -> Copied
    | Cinoptions -> Copied
    | Cinscopedecls -> Copied
    | Cinwords -> Copied
    | Comments -> Copied
    | Commentstring -> Copied
    | Complete -> Copied
    | Completefunc -> Copied
    | Copyindent -> Copied
    | Define -> Global
    | Dictionary -> Global
    | Endoffile -> Copied
    | Endofline -> Copied
    | Equalprg -> Global
    | Errorformat -> Global
    | Expandtab -> Copied
    | Fileencoding -> Copied
    | Fileformat -> Copied
    | Filetype -> None
    | Fixendofline -> Copied
    | Formatexpr -> Copied
    | Formatlistpat -> Copied
    | Formatoptions -> Copied
    | Formatprg -> Global
    | Grepprg -> Global
    | Iminsert -> Copied
    | Imsearch -> Copied
    | Include -> Global
    | Includeexpr -> Copied
    | Indentexpr -> Copied
    | Indentkeys -> Copied
    | Infercase -> Copied
    | Iskeyword -> Copied
    | Keymap -> Copied
    | Keywordprg -> Global
    | Lisp -> Copied
    | Lispoptions -> Copied
    | Lispwords -> Global
    | Makeencoding -> Global
    | Makeprg -> Global
    | Matchpairs -> Copied
    | Modeline -> Copied
    | Modifiable -> Copied
    | Modified -> None
    | Nrformats -> Copied
    | Omnifunc -> Copied
    | Path -> Global
    | Preserveindent -> Copied
    | Quoteescape -> Copied
    | Readonly -> None
    | Scrollback -> Copied
    | Shiftwidth -> Copied
    | Smartindent -> Copied
    | Softtabstop -> Copied
    | Spellcapcheck -> Copied
    | Spellfile -> Copied
    | Spelllang -> Copied
    | Spelloptions -> Copied
    | Suffixesadd -> Copied
    | Swapfile -> Copied
    | Synmaxcol -> Copied
    | Syntax -> None
    | Tabstop -> Copied
    | Tagcase -> Global
    | Tagfunc -> Copied
    | Tags -> Global
    | Textwidth -> Copied
    | Thesaurus -> Global
    | Thesaurusfunc -> Global
    | Undofile -> Copied
    | Undolevels -> Global
    | Varsofttabstop -> Copied
    | Vartabstop -> Copied
    | Wrapmargin -> Copied
  ;;

  (*$*)

  let set_local ~(here : [%call_pos]) client buffer t value =
    Nvim_internal.nvim_set_option_value
      ~name:(to_string t)
      ~value:(to_msgpack t value)
      ~opts:(String.Map.singleton "buf" (Or_current.to_msgpack buffer))
    |> run ~here client
  ;;

  let get_global ~(here : [%call_pos]) client t =
    Nvim_internal.nvim_get_option_value
      ~name:(to_string t)
      ~opts:(String.Map.singleton "scope" (Msgpack.String "global"))
    |> map_witness ~f:(of_msgpack t)
    |> run ~here client
  ;;

  let set_global ~(here : [%call_pos]) client t value =
    Nvim_internal.nvim_set_option_value
      ~name:(to_string t)
      ~value:(to_msgpack t value)
      ~opts:(String.Map.singleton "scope" (Msgpack.String "global"))
    |> run ~here client
  ;;

  let get (type a g) ~(here : [%call_pos]) client buffer (t : (a, g) t) =
    let bufnr = Or_current.to_msgpack buffer in
    match kind t with
    | None | Copied ->
      Nvim_internal.nvim_get_option_value
        ~name:(to_string t)
        ~opts:(String.Map.singleton "buf" bufnr)
      |> map_witness ~f:(of_msgpack t)
      |> run ~here client
    | Global ->
      (* Unfortunately, [nvim_option_get_value] does not provide a way to reliably get the
         effective option value for an arbitrary buffer. Even if we first called it with
         "buf" and then fell back to "global", we wouldn't be able to tell whether the
         "buf" call succeeded or failed, because it does not reliably return [Nil] on
         failure - sometimes it returns a default dummy value, such as -123456 in the case
         of [Undolevels]. Therefore, we go through Lua as a work-around. *)
      let code =
        {| local buffer, option = ...
         return vim.api.nvim_buf_call(buffer, function() return vim.o[option] end) |}
      in
      Nvim_internal.nvim_exec_lua ~code ~args:[ bufnr; String (to_string t) ]
      |> map_witness ~f:(of_msgpack t)
      |> run ~here client
  ;;

  let set = set_local
  let set_default = set_global
  let get_default = get_global
  let set_for_new_buffers = set_global
  let get_for_new_buffers = get_global

  let get_dynamic_info (type a g) ~(here : [%call_pos]) client (t : (a, g) t) =
    Nvim_internal.nvim_get_option_info ~name:(to_string t)
    |> map_witness
         ~f:(Dynamic_option_info.of_msgpack_map ~default_of_msgpack:(of_msgpack t))
    |> run ~here client
  ;;
end

let open_term ~(here : [%call_pos]) client t =
  let open Deferred.Or_error.Let_syntax in
  (* Calling [nvim_open_term] in a modified buffer loses the modifications, so we first
     ensure that the buffer is not modified. *)
  let%bind buffer = get_buffer ~here client t in
  match%bind Option.get ~here client (Id buffer) Modified with
  | true -> Deferred.Or_error.error_string "Cannot open terminal in modified buffer."
  | false ->
    (* As of Neovim 0.9.1, [opts] is Lua-specific. *)
    Nvim_internal.nvim_open_term ~buffer:(Id buffer) ~opts:String.Map.empty
    |> map_witness ~f:(function
      | 0 ->
        Or_error.error_s
          [%message "nvim_open_term failed (returned 0)" ~buffer:(t : Or_current.t)]
      | channel -> Ok channel)
    |> run ~here client
;;
