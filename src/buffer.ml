module Unshadow = struct
  module Command = Command
end

open Core
open Async
open Import
module Command = Unshadow.Command
include Nvim_internal.Buffer

(* We can't use [Vcaml.Client.eval] because of dependency cycles

   This function is pretty fragile, but there isn't really a better way to know what
   buffer we're currently in, so we pretty much have to do this to get good filtering
   from [`Current] event listen requests. *)
let current_buffer =
  let query : Msgpack.t Nvim_internal.Api_result.t =
    { name = "nvim_eval"
    ; params = [ String "bufnr(\"%\")" ]
    ; witness = Nvim_internal.Phantom.Object
    }
  in
  let open Api_call.Let_syntax in
  let%map result = Api_call.of_api_result query in
  Or_error.bind ~f:of_msgpack result
;;

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

  let parse { Msgpack_rpc.method_name; params } =
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

let get_name ~buffer = Nvim_internal.nvim_buf_get_name ~buffer |> Api_call.of_api_result

let set_name ~buffer ~name =
  Nvim_internal.nvim_buf_set_name ~buffer ~name |> Api_call.of_api_result
;;

let get_lines ~buffer ~start ~end_ ~strict_indexing =
  let open Api_call.Let_syntax in
  let%map result =
    Nvim_internal.nvim_buf_get_lines ~buffer ~start ~end_ ~strict_indexing
    |> Api_call.of_api_result
  in
  let open Or_error.Let_syntax in
  let%bind result = result in
  Or_error.try_with (fun () ->
    List.map result ~f:(function
      | String s -> s
      | _ -> failwith "malformed result"))
;;

let set_lines ~buffer ~start ~end_ ~strict_indexing ~replacement =
  let replacement = List.map ~f:(fun v -> Msgpack.String v) replacement in
  Nvim_internal.nvim_buf_set_lines ~buffer ~start ~end_ ~strict_indexing ~replacement
  |> Api_call.of_api_result
;;

let create ~listed ~scratch =
  Nvim_internal.nvim_create_buf ~listed ~scratch |> Api_call.of_api_result
;;

let find_by_name_or_create ~name =
  Nvim_internal.nvim_call_function ~fn:"bufadd" ~args:[ String name ]
  |> Api_call.of_api_result
  |> Api_call.map_bind ~f:(Extract.value Buffer)
;;

module Subscriber = struct
  type t =
    { client : Client.t
    ; on_error : Error.t -> unit
    }

  let create ?on_error (client : Client.t) =
    let T = Client.Private.eq in
    let on_error = Option.value on_error ~default:client.on_error in
    { client; on_error }
  ;;

  let buf_events t here =
    let T = Client.Private.eq in
    let r, w = Pipe.create () in
    let s =
      Bus.subscribe_exn t.client.events here ~f:(fun e ->
        match Event.parse e with
        | Some evt -> Pipe.write_without_pushback_if_open w evt
        | None -> ()
        | exception Event.Parse_error ->
          t.on_error
            (Error.create_s
               [%message "Failed to parse buffer event" ~_:(e : Msgpack_rpc.event)]))
    in
    upon
      (Deferred.any [ Pipe.closed r; Pipe.closed w ])
      (fun () ->
         Pipe.close_read r;
         Pipe.close w;
         Bus.unsubscribe t.client.events s);
    r
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
    let buffer_query =
      match buffer with
      | `Current -> Unsafe.of_int 0
      | `Numbered b -> b
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
          | label, `Boolean (Some flag) -> Some (Msgpack.String label, Boolean flag))
      in
      Nvim_internal.nvim_buf_attach ~buffer:buffer_query ~send_buffer ~opts
      |> Api_call.of_api_result
    in
    let curr_bufnr =
      match buffer with
      | `Current -> current_buffer
      | `Numbered b -> Api_call.return (Ok b)
    in
    let call = Api_call.both attach curr_bufnr in
    let open Deferred.Or_error.Let_syntax in
    (* We always run the actual attach because it's possible that the user has deleted the
       buffer (and so we want to fail with an error). *)
    let%bind success, bufnr =
      Api_call.run [%here] t.client call |> Deferred.map ~f:(tag_callsite here)
    in
    let run_attach () =
      let open Deferred.Or_error.Let_syntax in
      if%bind Deferred.return success
      then (
        let%bind bufnr = Deferred.return bufnr in
        let T = Client.Private.eq in
        Hashtbl.change t.client.buffers_attached bufnr ~f:(function
          | Some x -> Some (x + 1)
          | None -> Some 1);
        let incoming = Pipe.filter (buf_events t here) ~f:(Event.for_buffer bufnr) in
        let r =
          Pipe.create_reader ~close_on_exception:false (fun w ->
            Pipe.iter incoming ~f:(function
              | Event.Detach _ as evt ->
                let open Deferred.Let_syntax in
                (* Write without pushback here because we don't want the scheduler
                   interrupting us *)
                Pipe.write_without_pushback_if_open w evt;
                Pipe.close w;
                Hashtbl.remove t.client.buffers_attached bufnr;
                return ()
              | evt -> Pipe.write_if_open w evt))
        in
        upon (Pipe.closed r) (fun () ->
          upon
            (Api_call.run_join
               [%here]
               t.client
               (Nvim_internal.nvim_buf_detach ~buffer:bufnr |> Api_call.of_api_result))
            (function
              | Ok true -> Hashtbl.remove t.client.buffers_attached bufnr
              | _ ->
                (match on_detach_failure with
                 | `Ignore -> ()
                 | `Raise ->
                   raise_s
                     [%message
                       "Failed to detach from buffer"
                         (buffer : [ `Current | `Numbered of t ])]
                 | `Call f -> f buffer)));
        return r)
      else Deferred.Or_error.error_string "unable to connect to buffer"
    in
    let T = Client.Private.eq in
    Throttle.enqueue t.client.attach_sequencer run_attach
  ;;
end

let set_option ~buffer ~scope ~name ~type_ ~value =
  let vcaml_tmp = "__vcaml_tmp" in
  let value = Extract.inject type_ value in
  let command command = Nvim_internal.nvim_command ~command in
  let set_option = Nvim_internal.nvim_buf_set_option ~buffer ~name ~value in
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

let get_mark ~buffer ~sym =
  let open Api_call.Let_syntax in
  let%map pos =
    Nvim_internal.nvim_buf_get_mark ~buffer ~name:(Char.to_string sym)
    |> Api_call.of_api_result
  in
  let open Or_error.Let_syntax in
  match%bind pos with
  | [ Integer row; Integer col ] -> Ok { Mark.sym; pos = { row; col } }
  | _ -> Or_error.error_string "malformed result from [nvim_buf_get_mark]"
;;

module Untested = struct
  let line_count ~buffer =
    Api_call.of_api_result (Nvim_internal.nvim_buf_line_count ~buffer)
  ;;

  let get_var ~buffer ~name ~type_ =
    Nvim_internal.nvim_buf_get_var ~buffer ~name
    |> Api_call.of_api_result
    |> Api_call.map_bind ~f:(Extract.value type_)
  ;;

  let get_changedtick ~buffer =
    Nvim_internal.nvim_buf_get_changedtick ~buffer |> Api_call.of_api_result
  ;;

  let get_commands ~buffer =
    let open Api_call.Let_syntax in
    let%map result =
      (* [opts] is not used by this version of Neovim, but may be used in the future. If
         we expose it, we should do so in a typeful way rather than asking the user to
         build [Msgpack.t] values. *)
      Nvim_internal.nvim_buf_get_commands ~buffer ~opts:[] |> Api_call.of_api_result
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

  let set_var ~buffer ~name ~type_ ~value =
    let value = Extract.inject type_ value in
    Nvim_internal.nvim_buf_set_var ~buffer ~name ~value |> Api_call.of_api_result
  ;;

  let del_var ~buffer ~name =
    Nvim_internal.nvim_buf_del_var ~buffer ~name |> Api_call.of_api_result
  ;;

  let get_option ~buffer ~name ~type_ =
    Nvim_internal.nvim_buf_get_option ~buffer ~name
    |> Api_call.of_api_result
    |> Api_call.map_bind ~f:(Extract.value type_)
  ;;

  let is_loaded ~buffer =
    Nvim_internal.nvim_buf_is_loaded ~buffer |> Api_call.of_api_result
  ;;

  let is_valid ~buffer = Nvim_internal.nvim_buf_is_valid ~buffer |> Api_call.of_api_result

  (* This function's name is a bit misleading because it actually only ever does :bwipeout
     or :bunload depending on the passed options, but it never does :bdelete. *)
  let nvim_buf_delete ~only_unload ~buffer ~even_if_modified =
    let opts =
      [ Msgpack.String "force", Msgpack.Boolean even_if_modified
      ; Msgpack.String "unload", Msgpack.Boolean only_unload
      ]
    in
    Nvim_internal.nvim_buf_delete ~buffer ~opts |> Api_call.of_api_result
  ;;

  let unload = nvim_buf_delete ~only_unload:true
  let wipeout = nvim_buf_delete ~only_unload:false

  let get_byte_offset_of_line ~buffer ~line:index =
    Nvim_internal.nvim_buf_get_offset ~buffer ~index |> Api_call.of_api_result
  ;;

  let add_highlight ~buffer ~namespace ~hl_group ~line ~col_start ~col_end =
    Nvim_internal.nvim_buf_add_highlight
      ~buffer
      ~ns_id:(Namespace.id namespace)
      ~hl_group
      ~line
      ~col_start
      ~col_end
    |> Api_call.of_api_result
  ;;

  let clear_namespace ~buffer ~namespace ~line_start ~line_end =
    Nvim_internal.nvim_buf_clear_namespace
      ~buffer
      ~ns_id:(Namespace.id namespace)
      ~line_start
      ~line_end
    |> Api_call.of_api_result
  ;;

  let set_text ~buffer ~start_row ~start_col ~end_row ~end_col ~replacement =
    let replacement = List.map ~f:(fun v -> Msgpack.String v) replacement in
    Nvim_internal.nvim_buf_set_text
      ~buffer
      ~start_row
      ~start_col
      ~end_row
      ~end_col
      ~replacement
    |> Api_call.of_api_result
  ;;

  let set_virtual_text ~buffer ~namespace ~line ~text =
    (* [opts] is not used by this version of Neovim, but may be used in the future. If we
       expose it, we should do so in a typeful way rather than asking the user to build
       [Msgpack.t] values. *)
    Nvim_internal.nvim_buf_set_virtual_text
      ~buffer
      ~src_id:(Namespace.id namespace)
      ~line
      ~chunks:(Highlighted_text.to_msgpack text)
      ~opts:[]
    |> Api_call.of_api_result
    |> Api_call.Or_error.ignore_m
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
        ?when_underlying_highlight_conflicts
        ?extend_highlight_across_screen
        ?ephemeral
        ?start_gravity
        ?end_gravity
        ?priority
        ()
    =
    let opts =
      let module M = Msgpack in
      (* [bind] is bind-like but passes from the option monad to the list monad. *)
      let bind value ~f = Option.map value ~f |> Option.value ~default:[] in
      [ bind id ~f:(fun id -> [ "id", M.Integer id ])
      ; bind end_exclusive ~f:(fun { Position.row; col } ->
          [ "end_line", M.Integer row; "end_col", Integer col ])
      ; bind hl_group ~f:(fun hl_group -> [ "hl_group", M.String hl_group ])
      ; bind virtual_text ~f:(fun virtual_text ->
          [ "virt_text", M.Array (Highlighted_text.to_msgpack virtual_text) ])
      ; bind virtual_text_pos ~f:(function
          | `Eol -> [ "virt_text_pos", M.String "eol" ]
          | `Overlay -> [ "virt_text_pos", String "overlay" ]
          | `Right_align -> [ "virt_text_pos", String "right_align" ]
          | `At_column col -> [ "virt_text_win_col", Integer col ])
      ; bind hide_virtual_text_when_overlaying_selection ~f:(fun () ->
          [ "virt_text_hide", M.Boolean true ])
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
      ]
      |> List.concat
      |> List.map ~f:(Tuple2.map_fst ~f:(fun key -> M.String key))
    in
    let { Position.row; col } = start_inclusive in
    Nvim_internal.nvim_buf_set_extmark
      ~buffer
      ~ns_id:(Namespace.id namespace)
      ~line:row
      ~col
      ~opts
    |> Api_call.of_api_result
    |> Api_call.Or_error.map ~f:(fun id -> { Extmark.id; namespace; buffer })
    |> k
  ;;

  let create_extmark = set_extmark_internal ~k:Fn.id ?id:None

  let update_extmark ~extmark:{ Extmark.id; namespace; buffer } =
    set_extmark_internal ~k:Api_call.Or_error.ignore_m ~id ~namespace ~buffer
  ;;

  let delete_extmark ~extmark:{ Extmark.id; namespace; buffer } =
    Nvim_internal.nvim_buf_del_extmark ~buffer ~ns_id:(Namespace.id namespace) ~id
    |> Api_call.of_api_result
  ;;

  let get_extmark ~extmark:{ Extmark.id; namespace; buffer } =
    let opts = [ Msgpack.String "details", Msgpack.Boolean false ] in
    Nvim_internal.nvim_buf_get_extmark_by_id
      ~buffer
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

  let get_extmark_with_details ~extmark:{ Extmark.id; namespace; buffer } =
    let opts = [ Msgpack.String "details", Msgpack.Boolean true ] in
    Nvim_internal.nvim_buf_get_extmark_by_id
      ~buffer
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

  let all_extmarks ~buffer ~namespace ?start_inclusive ?end_inclusive () =
    let opts = [ Msgpack.String "details", Msgpack.Boolean false ] in
    let start =
      match start_inclusive with
      | None -> Msgpack.Integer 0
      | Some { Position.row; col } -> Msgpack.Array [ Integer row; Integer col ]
    in
    let end_ =
      match end_inclusive with
      | None -> Msgpack.Integer (-1)
      | Some { Position.row; col } -> Msgpack.Array [ Integer row; Integer col ]
    in
    Nvim_internal.nvim_buf_get_extmarks
      ~buffer
      ~ns_id:(Namespace.id namespace)
      ~start
      ~end_
      ~opts
    |> Api_call.of_api_result
    |> Api_call.map_bind ~f:(fun extmarks ->
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

  let all_extmarks_with_details ~buffer ~namespace ?start_inclusive ?end_inclusive () =
    let opts = [ Msgpack.String "details", Msgpack.Boolean true ] in
    let start =
      match start_inclusive with
      | None -> Msgpack.Integer 0
      | Some { Position.row; col } -> Msgpack.Array [ Integer row; Integer col ]
    in
    let end_ =
      match end_inclusive with
      | None -> Msgpack.Integer (-1)
      | Some { Position.row; col } -> Msgpack.Array [ Integer row; Integer col ]
    in
    Nvim_internal.nvim_buf_get_extmarks
      ~buffer
      ~ns_id:(Namespace.id namespace)
      ~start
      ~end_
      ~opts
    |> Api_call.of_api_result
    |> Api_call.map_bind ~f:(fun extmarks ->
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

  let open_term ~buffer =
    (* [opts] is not used by this version of Neovim, but may be used in the future. If we
       expose it, we should do so in a typeful way rather than asking the user to build
       [Msgpack.t] values. *)
    Nvim_internal.nvim_open_term ~buffer ~opts:[] |> Api_call.of_api_result
  ;;

  module Expert = struct
    let buf_call ~buffer ~lua_callback =
      Nvim_internal.nvim_buf_call ~buffer ~fun_:lua_callback |> Api_call.of_api_result
    ;;
  end
end
