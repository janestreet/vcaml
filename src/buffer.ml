module Unshadow = struct
  module Command = Command
end

open Core
open Async
open Import
module Command = Unshadow.Command
include Nvim_internal.Buffer

module Event = struct
  exception Parse_error

  type nonrec t =
    | Lines of
        { buffer : t
        ; changedtick : int option
        ; firstline : int
        ; lastline : int
        ; linedata : string list
        ; more : bool
        }
    | Changed_tick of
        { buffer : t
        ; changedtick : int
        }
    | Detach of t
  [@@deriving sexp_of]

  let parse { Msgpack_rpc.Event.method_name; params } =
    let of_msgpack_exn msg =
      match of_msgpack msg with
      | Ok buffer -> buffer
      | Error _ -> raise Parse_error
    in
    match method_name with
    | "nvim_buf_lines_event" ->
      (match params with
       | [ (Extension _ as buffer)
         ; changedtick
         ; Integer firstline
         ; Integer lastline
         ; Array changes
         ; Boolean more
         ] ->
         let linedata =
           List.map changes ~f:(function
             | String s -> s
             | _ -> raise Parse_error)
         in
         let buffer = of_msgpack_exn buffer in
         let changedtick =
           match changedtick with
           | Nil -> None
           | Integer i -> Some i
           | _ -> raise Parse_error
         in
         Some (Lines { buffer; changedtick; firstline; lastline; linedata; more })
       | _ -> raise Parse_error)
    | "nvim_buf_changedtick_event" ->
      (match params with
       | [ (Extension _ as buffer); Integer changedtick ] ->
         let buffer = of_msgpack_exn buffer in
         Some (Changed_tick { buffer; changedtick })
       | [ (Extension _ as buffer); Nil ] ->
         let buffer = of_msgpack_exn buffer in
         Some (Changed_tick { buffer; changedtick = 0 })
       | _ -> raise Parse_error)
    | "nvim_buf_detach_event" ->
      (match params with
       | [ (Extension _ as buffer) ] ->
         let buffer = of_msgpack_exn buffer in
         Some (Detach buffer)
       | _ -> raise Parse_error)
    | _ -> None
  ;;

  let for_buffer t = function
    | Lines { buffer; _ } | Changed_tick { buffer; _ } | Detach buffer -> t = buffer
  ;;
end

let get_name t = Nvim_internal.nvim_buf_get_name ~buffer:t |> Api_call.of_api_result

let set_name t ~name =
  Nvim_internal.nvim_buf_set_name ~buffer:t ~name |> Api_call.of_api_result
;;

let get_lines t ~start ~end_ ~strict_indexing =
  let open Api_call.Let_syntax in
  let%map result =
    Nvim_internal.nvim_buf_get_lines ~buffer:t ~start ~end_ ~strict_indexing
    |> Api_call.of_api_result
  in
  let open Or_error.Let_syntax in
  let%bind result = result in
  Or_error.try_with (fun () ->
    List.map result ~f:(function
      | String s -> s
      | _ -> failwith "malformed result"))
;;

let set_lines t ~start ~end_ ~strict_indexing ~replacement =
  let replacement = List.map ~f:(fun v -> Msgpack.String v) replacement in
  Nvim_internal.nvim_buf_set_lines ~buffer:t ~start ~end_ ~strict_indexing ~replacement
  |> Api_call.of_api_result
;;

let create ~listed ~scratch =
  Nvim_internal.nvim_create_buf ~listed ~scratch
  |> Api_call.of_api_result
  |> Api_call.map ~f:(function
    | Error _ as error -> error
    | Ok t ->
      (match (t :> int) with
       | 0 -> Or_error.error_string "nvim_create_buf failed (returned 0)"
       | _ -> Ok t))
;;

let find_by_name_or_create ~name =
  Nvim_internal.nvim_call_function ~fn:"bufadd" ~args:[ String name ]
  |> Api_call.of_api_result
  |> Api_call.map_bind ~f:(Extract.value Buffer)
;;

let get_buffer or_current =
  match (or_current : Or_current.t) with
  | Current -> Api_call.of_api_result Nvim_internal.nvim_get_current_buf
  | Id id -> Api_call.Or_error.return id
;;

module Subscriber = struct
  type buffer = t [@@deriving sexp_of]

  type t =
    { client : [ `connected ] Client.Private.t
    ; on_parse_error : Msgpack_rpc.Event.t -> unit
    }

  let create client ~on_parse_error =
    let client = Type_equal.conv Client.Private.eq client in
    let on_parse_error =
      match on_parse_error with
      | `Call f -> f
      | `Ignore -> ignore
      | `Raise ->
        fun event ->
          raise_s
            [%message "Failed to parse buffer event" ~_:(event : Msgpack_rpc.Event.t)]
    in
    { client; on_parse_error }
  ;;

  let buf_events t here =
    let reader, writer = Pipe.create () in
    let subscriber =
      Bus.subscribe_exn t.client.events here ~f:(fun event ->
        match Event.parse event with
        | Some event -> Pipe.write_without_pushback_if_open writer event
        | None -> ()
        | exception Event.Parse_error -> t.on_parse_error event)
    in
    upon
      (Deferred.any [ Pipe.closed reader; Pipe.closed writer ])
      (fun () ->
         Pipe.close_read reader;
         Pipe.close writer;
         Bus.unsubscribe t.client.events subscriber);
    reader
  ;;

  let subscribe
        ?(on_detach_failure = `Ignore)
        ?on_lines
        ?on_bytes
        ?on_changed_tick
        ?on_detach
        ?on_reload
        ?utf_sizes
        ?preview
        t
        here
        ~buffer
        ~send_buffer
    =
    let buf_events = buf_events t here in
    let run_join here call =
      let client = Type_equal.conv (Type_equal.sym Client.Private.eq) t.client in
      Api_call.run_join here client call
    in
    let attach =
      let opts =
        [ "on_lines", `Luaref on_lines
        ; "on_bytes", `Luaref on_bytes
        ; "on_changed_tick", `Luaref on_changed_tick
        ; "on_detach", `Luaref on_detach
        ; "on_reload", `Luaref on_reload
        ; "utf_sizes", `Boolean utf_sizes
        ; "preview", `Boolean preview
        ]
        |> List.filter_map ~f:(function
          | _, (`Luaref None | `Boolean None) -> None
          | label, `Luaref (Some callback) ->
            Some (Msgpack.String label, Nvim_internal.Luaref.to_msgpack callback)
          | label, `Boolean (Some flag) -> Some (String label, Boolean flag))
      in
      Nvim_internal.nvim_buf_attach ~buffer ~send_buffer ~opts |> Api_call.of_api_result
    in
    let call = Api_call.Or_error.both attach (get_buffer buffer) in
    match%bind run_join [%here] call |> Deferred.map ~f:(tag_callsite here) with
    | Error _ as error ->
      Pipe.close_read buf_events;
      return error
    | Ok (false, buffer) ->
      Pipe.close_read buf_events;
      Deferred.Or_error.error_s [%message "Failed to attach to buffer" (buffer : buffer)]
    | Ok (true, buffer) ->
      let open Deferred.Or_error.Let_syntax in
      (* We always run the actual attach because it's possible that the user has deleted
         the buffer (and so we want to fail with an error). *)
      let (Connected state) = t.client.state in
      let run_attach () =
        Hashtbl.change state.buffers_attached buffer ~f:(function
          | Some x -> Some (x + 1)
          | None -> Some 1);
        let incoming = Pipe.filter buf_events ~f:(Event.for_buffer buffer) in
        let r =
          Pipe.create_reader ~close_on_exception:false (fun w ->
            Pipe.iter incoming ~f:(function
              | Event.Detach _ as evt ->
                let open Deferred.Let_syntax in
                (* Write without pushback here because we don't want the scheduler
                   interrupting us *)
                Pipe.write_without_pushback_if_open w evt;
                Pipe.close w;
                Hashtbl.remove state.buffers_attached buffer;
                return ()
              | evt -> Pipe.write_if_open w evt))
        in
        upon (Pipe.closed r) (fun () ->
          upon
            (run_join
               [%here]
               (Nvim_internal.nvim_buf_detach ~buffer:(Id buffer)
                |> Api_call.of_api_result))
            (function
              | Ok true -> Hashtbl.remove state.buffers_attached buffer
              | _ ->
                (match on_detach_failure with
                 | `Ignore -> ()
                 | `Raise ->
                   raise_s [%message "Failed to detach from buffer" (buffer : buffer)]
                 | `Call f -> f buffer)));
        return r
      in
      Throttle.enqueue state.attach_sequencer run_attach
  ;;
end

let get_option t ~name ~type_ =
  Nvim_internal.nvim_buf_get_option ~buffer:t ~name
  |> Api_call.of_api_result
  |> Api_call.map_bind ~f:(Extract.value type_)
;;

let set_option t ~scope ~name ~type_ ~value =
  let vcaml_tmp = "__vcaml_tmp" in
  let value = Extract.inject type_ value in
  let command command = Nvim_internal.nvim_command ~command in
  let set_option = Nvim_internal.nvim_buf_set_option ~buffer:t ~name ~value in
  let api_results =
    match scope with
    | `Global -> [ set_option ]
    | `Local ->
      [ command [%string "let %{vcaml_tmp} = &g:%{name}"]
      ; set_option
      ; command [%string "let &g:%{name} = %{vcaml_tmp}"]
      ; command [%string "unlet %{vcaml_tmp}"]
      ]
  in
  api_results |> List.map ~f:Api_call.of_api_result |> Api_call.Or_error.all_unit
;;

let get_mark t ~sym =
  let open Api_call.Let_syntax in
  let%map pos =
    Nvim_internal.nvim_buf_get_mark ~buffer:t ~name:(Char.to_string sym)
    |> Api_call.of_api_result
  in
  let open Or_error.Let_syntax in
  match%bind pos with
  | [ Integer 0; Integer 0 ] ->
    Or_error.error_s
      [%message "Mark not set in buffer" ~buffer:(t : Or_current.t) (sym : char)]
  | [ Integer row; Integer col ] -> Ok { Mark.sym; pos = { row; col } }
  | _ -> Or_error.error_string "malformed result from [nvim_buf_get_mark]"
;;

module Untested = struct
  let line_count t = Api_call.of_api_result (Nvim_internal.nvim_buf_line_count ~buffer:t)

  let get_var t ~name ~type_ =
    Nvim_internal.nvim_buf_get_var ~buffer:t ~name
    |> Api_call.of_api_result
    |> Api_call.map_bind ~f:(Extract.value type_)
  ;;

  let get_changedtick t =
    Nvim_internal.nvim_buf_get_changedtick ~buffer:t |> Api_call.of_api_result
  ;;

  let get_commands t =
    let open Api_call.Let_syntax in
    let%map result =
      (* [opts] is not used by this version of Neovim, but may be used in the future. If
         we expose it, we should do so in a typeful way rather than asking the user to
         build [Msgpack.t] values. *)
      Nvim_internal.nvim_buf_get_commands ~buffer:t ~opts:[] |> Api_call.of_api_result
    in
    let open Or_error.Let_syntax in
    let%bind result = result in
    let%bind commands_with_names =
      List.map result ~f:(fun (name, command) ->
        let open Or_error.Let_syntax in
        let%bind n = Extract.string name in
        let%bind c = Command.of_msgpack command in
        return (n, c))
      |> Or_error.combine_errors
    in
    String.Map.of_alist_or_error commands_with_names
  ;;

  let set_var t ~name ~type_ ~value =
    let value = Extract.inject type_ value in
    Nvim_internal.nvim_buf_set_var ~buffer:t ~name ~value |> Api_call.of_api_result
  ;;

  let delete_var t ~name =
    Nvim_internal.nvim_buf_del_var ~buffer:t ~name |> Api_call.of_api_result
  ;;

  let is_loaded t = Nvim_internal.nvim_buf_is_loaded ~buffer:t |> Api_call.of_api_result
  let is_valid t = Nvim_internal.nvim_buf_is_valid ~buffer:t |> Api_call.of_api_result

  (* This function's name is a bit misleading because it actually only ever does :bwipeout
     or :bunload depending on the passed options, but it never does :bdelete. *)
  let nvim_buf_delete t ~only_unload ~even_if_modified =
    let opts =
      [ Msgpack.String "force", Msgpack.Boolean even_if_modified
      ; Msgpack.String "unload", Msgpack.Boolean only_unload
      ]
    in
    Nvim_internal.nvim_buf_delete ~buffer:t ~opts |> Api_call.of_api_result
  ;;

  let unload = nvim_buf_delete ~only_unload:true
  let wipeout = nvim_buf_delete ~only_unload:false

  let get_byte_offset_of_line t ~line:index =
    Nvim_internal.nvim_buf_get_offset ~buffer:t ~index |> Api_call.of_api_result
  ;;

  let add_highlight t ~namespace ~hl_group ~line ~col_start ~col_end =
    Nvim_internal.nvim_buf_add_highlight
      ~buffer:t
      ~ns_id:(Namespace.id namespace)
      ~hl_group
      ~line
      ~col_start
      ~col_end
    |> Api_call.of_api_result
  ;;

  let clear_namespace t ~namespace ~line_start ~line_end =
    Nvim_internal.nvim_buf_clear_namespace
      ~buffer:t
      ~ns_id:(Namespace.id namespace)
      ~line_start
      ~line_end
    |> Api_call.of_api_result
  ;;

  let set_text t ~start_row ~start_col ~end_row ~end_col ~replacement =
    let replacement = List.map ~f:(fun v -> Msgpack.String v) replacement in
    Nvim_internal.nvim_buf_set_text
      ~buffer:t
      ~start_row
      ~start_col
      ~end_row
      ~end_col
      ~replacement
    |> Api_call.of_api_result
  ;;

  let set_mark t mark =
    (* [opts] is not used by this version of Neovim, but may be used in the future. If we
       expose it, we should do so in a typeful way rather than asking the user to build
       [Msgpack.t] values. *)
    Nvim_internal.nvim_buf_set_mark
      ~buffer:t
      ~name:(Char.to_string mark.Mark.sym)
      ~line:mark.pos.row
      ~col:mark.pos.col
      ~opts:[]
    |> Api_call.of_api_result
    |> Api_call.map_bind ~f:(function
      | true -> Ok ()
      | false ->
        Or_error.error_s
          [%message "Failed to set mark" ~buffer:(t : Or_current.t) (mark : Mark.t)])
  ;;

  let delete_mark t sym =
    Nvim_internal.nvim_buf_del_mark ~buffer:t ~name:(Char.to_string sym)
    |> Api_call.of_api_result
    |> Api_call.map_bind ~f:(function
      | true -> Ok ()
      | false ->
        Or_error.error_s
          [%message "Mark not set in buffer" ~buffer:(t : Or_current.t) (sym : char)])
  ;;

  module Extmark = struct
    module T = struct
      type buffer = t [@@deriving compare, hash, sexp_of]

      type t =
        { id : int
        ; namespace : Namespace.t
        ; buffer : buffer
        }
      [@@deriving compare, fields, hash, sexp_of]
    end

    include T
    include Comparable.Make_plain (T)
    include Hashable.Make_plain (T)
  end

  let set_extmark_internal
        ~k
        ~buffer
        ~namespace
        ~start_inclusive
        ?id
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
        ()
    =
    let opts =
      let module M = Msgpack in
      (* [bind] is bind-like but passes from the option monad to the list monad. *)
      let bind value ~f = Option.map value ~f |> Option.value ~default:[] in
      let pack_highlighted_text text = M.Array (Highlighted_text.to_msgpack text) in
      [ bind id ~f:(fun id -> [ "id", M.Integer id ])
      ; bind end_exclusive ~f:(fun { Position.row; col } ->
          [ "end_row", M.Integer row; "end_col", Integer col ])
      ; bind hl_group ~f:(fun hl_group -> [ "hl_group", M.String hl_group ])
      ; bind virtual_text ~f:(fun virtual_text ->
          [ "virt_text", pack_highlighted_text virtual_text ])
      ; bind virtual_text_pos ~f:(function
          | `Eol -> [ "virt_text_pos", M.String "eol" ]
          | `Overlay -> [ "virt_text_pos", String "overlay" ]
          | `Right_align -> [ "virt_text_pos", String "right_align" ]
          | `At_column col -> [ "virt_text_win_col", Integer col ])
      ; bind hide_virtual_text_when_overlaying_selection ~f:(fun () ->
          [ "virt_text_hide", M.Boolean true ])
      ; bind virtual_lines ~f:(fun virtual_lines ->
          [ "virt_lines", virtual_lines |> List.map ~f:pack_highlighted_text |> M.Array
          ])
      ; bind virtual_lines_pos ~f:(fun pos ->
          let above =
            match pos with
            | `Above -> true
            | `Below -> false
          in
          [ "virt_lines_above", M.Boolean above ])
      ; bind bypass_sign_and_number_columns ~f:(fun bypass_sign_and_number_columns ->
          [ "virt_lines_leftcol", M.Boolean bypass_sign_and_number_columns ])
      ; bind when_underlying_highlight_conflicts ~f:(fun what_to_do ->
          let hl_mode =
            match what_to_do with
            | `Override -> "replace"
            | `Combine_with_bg -> "combine"
            | `Blend -> "blend"
          in
          [ "hl_mode", M.String hl_mode ])
      ; bind extend_highlight_across_screen ~f:(fun () -> [ "hl_eol", M.Boolean true ])
      ; bind ephemeral ~f:(fun () -> [ "ephemeral", M.Boolean true ])
      ; bind start_gravity ~f:(fun gravity ->
          let right_gravity =
            match gravity with
            | `Right -> true
            | `Left -> false
          in
          [ "right_gravity", M.Boolean right_gravity ])
      ; bind end_gravity ~f:(fun end_gravity ->
          let end_right_gravity =
            match end_gravity with
            | `Right -> true
            | `Left -> false
          in
          [ "end_right_gravity", M.Boolean end_right_gravity ])
      ; bind priority ~f:(fun priority -> [ "priority", M.Integer priority ])
      ; bind strict ~f:(fun strict -> [ "strict", M.Boolean strict ])
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
      ]
      |> List.concat
      |> List.map ~f:(Tuple2.map_fst ~f:(fun key -> M.String key))
    in
    let { Position.row; col } = start_inclusive in
    let set_extmark =
      Nvim_internal.nvim_buf_set_extmark
        ~buffer
        ~ns_id:(Namespace.id namespace)
        ~line:row
        ~col
        ~opts
      |> Api_call.of_api_result
    in
    Api_call.Or_error.both set_extmark (get_buffer buffer)
    |> Api_call.Or_error.map ~f:(fun (id, buffer) -> { Extmark.id; namespace; buffer })
    |> k
  ;;

  let create_extmark t = set_extmark_internal ~k:Fn.id ?id:None ~buffer:t

  let update_extmark { Extmark.id; namespace; buffer } =
    set_extmark_internal ~k:Api_call.Or_error.ignore_m ~id ~namespace ~buffer:(Id buffer)
  ;;

  let delete_extmark extmark =
    Nvim_internal.nvim_buf_del_extmark
      ~buffer:(Id extmark.Extmark.buffer)
      ~ns_id:(Namespace.id extmark.namespace)
      ~id:extmark.id
    |> Api_call.of_api_result
    |> Api_call.map_bind ~f:(function
      | true -> Ok ()
      | false ->
        Or_error.error_s [%message "Invalid extmark" ~_:(extmark : Extmark.t)])
  ;;

  let get_extmark { Extmark.id; namespace; buffer } =
    let opts = [ Msgpack.String "details", Msgpack.Boolean false ] in
    Nvim_internal.nvim_buf_get_extmark_by_id
      ~buffer:(Id buffer)
      ~ns_id:(Namespace.id namespace)
      ~id
      ~opts
    |> Api_call.of_api_result
    |> Api_call.map_bind ~f:(function
      | [] -> Ok None
      | [ Integer row; Integer col ] -> Ok (Some { Position.row; col })
      | _ ->
        Or_error.error_string "malformed result from [nvim_buf_get_extmark_by_id]")
  ;;

  let get_extmark_with_details { Extmark.id; namespace; buffer } =
    let opts = [ Msgpack.String "details", Msgpack.Boolean true ] in
    Nvim_internal.nvim_buf_get_extmark_by_id
      ~buffer:(Id buffer)
      ~ns_id:(Namespace.id namespace)
      ~id
      ~opts
    |> Api_call.of_api_result
    |> Api_call.map_bind ~f:(function
      | [] -> Ok None
      | [ Integer row; Integer col; details ] ->
        let open Or_error.Let_syntax in
        let%map details = Extract.map_of_msgpack_map details in
        Some ({ Position.row; col }, details)
      | _ ->
        Or_error.error_string "malformed result from [nvim_buf_get_extmark_by_id]")
  ;;

  let all_extmarks t ~namespace ?start_inclusive ?end_inclusive () =
    let opts = [ Msgpack.String "details", Msgpack.Boolean false ] in
    let start =
      match start_inclusive with
      | None -> Msgpack.Integer 0
      | Some { Position.row; col } -> Array [ Integer row; Integer col ]
    in
    let end_ =
      match end_inclusive with
      | None -> Msgpack.Integer (-1)
      | Some { Position.row; col } -> Array [ Integer row; Integer col ]
    in
    let get_extmarks =
      Nvim_internal.nvim_buf_get_extmarks
        ~buffer:t
        ~ns_id:(Namespace.id namespace)
        ~start
        ~end_
        ~opts
      |> Api_call.of_api_result
    in
    Api_call.Or_error.both get_extmarks (get_buffer t)
    |> Api_call.map_bind ~f:(fun (extmarks, buffer) ->
      extmarks
      |> List.map ~f:(function
        | Array [ Integer id; Integer row; Integer col ] ->
          let extmark = { Extmark.id; namespace; buffer } in
          let pos = { Position.row; col } in
          Ok (extmark, pos)
        | _ ->
          Or_error.error_string "malformed result from [nvim_buf_get_extmarks]")
      |> Or_error.combine_errors)
  ;;

  let all_extmarks_with_details t ~namespace ?start_inclusive ?end_inclusive () =
    let opts = [ Msgpack.String "details", Msgpack.Boolean true ] in
    let start =
      match start_inclusive with
      | None -> Msgpack.Integer 0
      | Some { Position.row; col } -> Array [ Integer row; Integer col ]
    in
    let end_ =
      match end_inclusive with
      | None -> Msgpack.Integer (-1)
      | Some { Position.row; col } -> Array [ Integer row; Integer col ]
    in
    let get_extmarks =
      Nvim_internal.nvim_buf_get_extmarks
        ~buffer:t
        ~ns_id:(Namespace.id namespace)
        ~start
        ~end_
        ~opts
      |> Api_call.of_api_result
    in
    Api_call.Or_error.both get_extmarks (get_buffer t)
    |> Api_call.map_bind ~f:(fun (extmarks, buffer) ->
      extmarks
      |> List.map ~f:(function
        | Array [ Integer id; Integer row; Integer col; details ] ->
          let open Or_error.Let_syntax in
          let%map details = Extract.map_of_msgpack_map details in
          let extmark = { Extmark.id; namespace; buffer } in
          let pos = { Position.row; col } in
          extmark, pos, details
        | _ ->
          Or_error.error_string "malformed result from [nvim_buf_get_extmarks]")
      |> Or_error.combine_errors)
  ;;

  let open_term ?on_input t =
    let opts =
      [ "on_input", `Luaref on_input ]
      |> List.filter_map ~f:(function
        | _, `Luaref None -> None
        | label, `Luaref (Some callback) ->
          Some (Msgpack.String label, Nvim_internal.Luaref.to_msgpack callback))
    in
    Nvim_internal.nvim_open_term ~buffer:t ~opts
    |> Api_call.of_api_result
    |> Api_call.map ~f:(function
      | Error _ as error -> error
      | Ok channel ->
        (match channel with
         | 0 ->
           Or_error.error_s
             [%message
               "nvim_open_term failed (returned 0)" ~buffer:(t : Or_current.t)]
         | _ -> Ok channel))
  ;;

  module Expert = struct
    let buf_call t ~lua_callback =
      Nvim_internal.nvim_buf_call ~buffer:t ~fun_:lua_callback |> Api_call.of_api_result
    ;;
  end
end
