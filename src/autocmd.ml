open Core
open Async
open Import
module Id = Int

module Event = struct
  module T = struct
    type t =
      | BufAdd
      | BufDelete
      | BufEnter
      | BufFilePost
      | BufFilePre
      | BufHidden
      | BufLeave
      | BufModifiedSet
      | BufNew
      | BufNewFile
      | BufRead
      | BufReadCmd
      | BufReadPost
      | BufReadPre
      | BufUnload
      | BufWinEnter
      | BufWinLeave
      | BufWipeout
      | BufWrite
      | BufWriteCmd
      | BufWritePost
      | BufWritePre
      | ChanInfo
      | ChanOpen
      | CmdUndefined
      | CmdlineChanged
      | CmdlineEnter
      | CmdlineLeave
      | CmdWinEnter
      | CmdWinLeave
      | ColorScheme
      | ColorSchemePre
      | CompleteChanged
      | CompleteDone
      | CompleteDonePre
      | CursorHold
      | CursorHoldI
      | CursorMoved
      | CursorMovedI
      | DiffUpdated
      | DirChanged
      | DirChangedPre
      | ExitPre
      | FileAppendCmd
      | FileAppendPost
      | FileAppendPre
      | FileChangedRO
      | FileChangedShell
      | FileChangedShellPost
      | FileReadCmd
      | FileReadPost
      | FileReadPre
      | FileType
      | FileWriteCmd
      | FileWritePost
      | FileWritePre
      | FilterReadPost
      | FilterReadPre
      | FilterWritePost
      | FilterWritePre
      | FocusGained
      | FocusLost
      | FuncUndefined
      | InsertChange
      | InsertCharPre
      | InsertEnter
      | InsertLeave
      | InsertLeavePre
      | LspAttach
      | LspDetach
      | LspTokenUpdate
      | MenuPopup
      | ModeChanged
      | OptionSet
      | QuickFixCmdPost
      | QuickFixCmdPre
      | QuitPre
      | RecordingEnter
      | RecordingLeave
      | RemoteReply
      | SearchWrapped
      | SessionLoadPost
      | ShellCmdPost
      | ShellFilterPost
      | Signal
      | SourceCmd
      | SourcePost
      | SourcePre
      | SpellFileMissing
      | StdinReadPost
      | StdinReadPre
      | SwapExists
      | Syntax
      | TabClosed
      | TabEnter
      | TabLeave
      | TabNew
      | TabNewEntered
      | TermClose
      | TermEnter
      | TermLeave
      | TermOpen
      | TermResponse
      | TextChanged
      | TextChangedI
      | TextChangedP
      | TextChangedT
      | TextYankPost
      | UIEnter
      | UILeave
      | User
      | VimEnter
      | VimLeave
      | VimLeavePre
      | VimResized
      | VimResume
      | VimSuspend
      | WinClosed
      | WinEnter
      | WinLeave
      | WinNew
      | WinResized
      | WinScrolled
    [@@deriving compare, enumerate, sexp]
  end

  include T
  include Comparable.Make (T)

  let to_string t = Sexp.to_string (sexp_of_t t)

  let of_string =
    let lookup =
      all
      |> List.map ~f:(fun t -> String.lowercase (to_string t), t)
      |> String.Map.of_alist_exn
    in
    fun s -> Core.Map.find_exn lookup (String.lowercase s)
  ;;

  let to_msgpack t = Msgpack.String (to_string t)

  let of_msgpack : Msgpack.t -> t Or_error.t = function
    | String s ->
      (match of_string s with
       | t -> Ok t
       | exception _ -> Or_error.error_s [%message "Unrecognized autocmd event" s])
    | event -> Or_error.error_s [%message "Malformed autocmd event" (event : Msgpack.t)]
  ;;
end

module Group = struct
  include Int

  let create ~(here : [%call_pos]) client ?clear_if_exists name =
    let opts =
      match clear_if_exists with
      | None -> String.Map.empty
      | Some clear -> String.Map.singleton "clear" (Msgpack.Bool clear)
    in
    Nvim_internal.nvim_create_augroup ~name ~opts |> run ~here client
  ;;

  let delete ~(here : [%call_pos]) client t =
    Nvim_internal.nvim_del_augroup_by_id ~id:t |> run ~here client
  ;;
end

module Pattern_or_buffer = struct
  type t =
    | Pattern of string
    | Buffer of Nvim_internal.Buffer.t
  [@@deriving sexp_of]

  let of_msgpack_map map =
    let open Or_error.Let_syntax in
    match%bind find_and_convert map "buffer" (Type.of_msgpack Buffer) with
    | Some buffer -> Ok (Buffer buffer)
    | None ->
      (match%bind find_and_convert map "pattern" (Type.of_msgpack String) with
       | Some pattern -> Ok (Pattern pattern)
       | None ->
         Or_error.error_s
           [%message
             "Expected one of 'buffer' or 'pattern' to be present."
               (map : Msgpack.t String.Map.t)])
  ;;
end

let patterns_to_msgpack patterns =
  patterns |> Nonempty_list.to_list |> Type.to_msgpack (Array String)
;;

let events_to_msgpack events =
  let events = events |> Nonempty_list.to_list |> List.map ~f:Event.to_msgpack in
  Msgpack.Array events
;;

module Patterns_or_buffer = struct
  type t =
    | Patterns of string Nonempty_list.t
    | Buffer of Nvim_internal.Buffer.Or_current.t

  let to_msgpack_map_entry = function
    | Patterns patterns -> "pattern", patterns_to_msgpack patterns
    | Buffer buffer -> "buffer", Nvim_internal.Buffer.Or_current.to_msgpack buffer
  ;;
end

type t =
  { id : Id.t option
  ; group : Group.t option
  ; group_name : string option
  ; description : string option
  ; event : Event.t
  ; pattern_or_buffer : Pattern_or_buffer.t
  ; once : bool
  ; command : string
  }
[@@deriving sexp_of]

let get ~(here : [%call_pos]) client ?group ?events ?patterns_or_buffer () =
  let opts =
    let maybe name var conv = Option.map var ~f:(fun var -> name, conv var) in
    [ maybe "group" group (Type.to_msgpack Int)
    ; maybe "event" events events_to_msgpack
    ; Option.map patterns_or_buffer ~f:Patterns_or_buffer.to_msgpack_map_entry
    ]
    |> List.filter_opt
    |> String.Map.of_alist_exn
  in
  Nvim_internal.nvim_get_autocmds ~opts
  |> map_witness ~f:(fun autocmds ->
    autocmds
    |> List.map ~f:(fun msgpack ->
      let open Or_error.Let_syntax in
      let%bind map = Type.of_msgpack Dict msgpack in
      let%bind id = find_and_convert map "id" (Type.of_msgpack Int) in
      let%bind group = find_and_convert map "group" (Type.of_msgpack Int) in
      let%bind group_name = find_and_convert map "group_name" (Type.of_msgpack String) in
      let%bind description = find_and_convert map "desc" (Type.of_msgpack String) in
      let%bind event = find_or_error_and_convert map "event" Event.of_msgpack in
      let%bind pattern_or_buffer = Pattern_or_buffer.of_msgpack_map map in
      let%bind command =
        find_or_error_and_convert map "command" (Type.of_msgpack String)
      in
      let%bind once = find_or_error_and_convert map "once" (Type.of_msgpack Bool) in
      return
        { id; group; group_name; description; event; pattern_or_buffer; once; command })
    |> Or_error.combine_errors)
  |> run ~here client
;;

let rec create
  ~(here : [%call_pos])
  client
  ?description
  ?once
  ?nested
  ~group
  ~patterns_or_buffer
  ~events
  command
  =
  let open Deferred.Or_error.Let_syntax in
  let%bind patterns_or_buffer =
    match (patterns_or_buffer : Patterns_or_buffer.t) with
    | Patterns _ | Buffer (Id _) -> return patterns_or_buffer
    | Buffer Current ->
      let%map buffer = Nvim.get_current_buf ~here client in
      Patterns_or_buffer.Buffer (Id buffer)
  in
  let viml_or_rpc_name =
    match (command : unit Ocaml_from_nvim.Callback.t) with
    | Viml command -> `Viml command
    | Rpc rpc ->
      `Rpc (Ocaml_from_nvim.Private.register_callback ~here client ~return_type:Nil rpc)
  in
  let channel = Client.channel client in
  let command =
    match viml_or_rpc_name with
    | `Viml command -> command
    | `Rpc name -> [%string {|call rpcrequest(%{channel#Int}, "%{name}")|}]
  in
  let event = events_to_msgpack events in
  let opts =
    let maybe name var conv = Option.map var ~f:(fun var -> name, conv var) in
    [ Some ("group", Msgpack.Int group)
    ; Some (Patterns_or_buffer.to_msgpack_map_entry patterns_or_buffer)
    ; Some ("command", String command)
    ; maybe "desc" description (Type.to_msgpack String)
    ; maybe "once" once (Type.to_msgpack Bool)
    ; maybe "nested" nested (Type.to_msgpack Bool)
    ]
    |> List.filter_opt
    |> String.Map.of_alist_exn
  in
  let%bind id = Nvim_internal.nvim_create_autocmd ~event ~opts |> run ~here client in
  let%bind () =
    match viml_or_rpc_name, patterns_or_buffer with
    | `Viml _, _ | _, Patterns _ -> return ()
    | `Rpc name, Buffer _ ->
      (* It's important that we create the RPC-based autocmd before creating the autocmd
         for unregistering the RPC. If we did this in the other order then an RPC-based
         autocmd that runs on [BufWipeout] would fail to call the RPC since autocmds run
         in the order they are defined. *)
      let group =
        let client = Type_equal.(conv Client.Private.eq) client in
        Set_once.get_exn client.vcaml_internal_group
      in
      create
        ~here
        client
        ~description:[%string "Unregister %{name}"]
        ~once:true
        ~patterns_or_buffer
        ~group
          (* Unlike buffer-local keymaps and buffer-local commands, buffer-local autocmds
             are only removed on wipeout, not on buffer deletion. *)
        ~events:[ BufWipeout ]
        (Viml
           [%string
             {|
if !empty(nvim_get_chan_info(%{channel#Int}))
  call rpcnotify(%{channel#Int}, "%{Client.Private.unregister_blocking_rpc}", "%{name}")
endif |}])
      |> Deferred.ignore_m
      |> Deferred.ok
  in
  return id
;;

let delete ~(here : [%call_pos]) client id =
  Nvim_internal.nvim_del_autocmd ~id |> run ~here client
;;

let clear ~(here : [%call_pos]) client ?patterns_or_buffer () ~group ~events =
  let opts =
    let event = events_to_msgpack events in
    let group =
      match group with
      | `Not_in_any_group -> None
      | `Group group -> Some ("group", Msgpack.Int group)
    in
    [ Some ("event", event)
    ; group
    ; Option.map patterns_or_buffer ~f:Patterns_or_buffer.to_msgpack_map_entry
    ]
    |> List.filter_opt
    |> String.Map.of_alist_exn
  in
  Nvim_internal.nvim_clear_autocmds ~opts |> run ~here client
;;

let exec
  ~(here : [%call_pos])
  client
  ?group
  ?patterns_or_buffer
  ?modeline
  ?data
  ()
  ~events
  =
  let event = events_to_msgpack events in
  let opts =
    let maybe name var conv = Option.map var ~f:(fun var -> name, conv var) in
    [ maybe "group" group (Type.to_msgpack Int)
    ; maybe "modeline" modeline (Type.to_msgpack Bool)
    ; Option.map patterns_or_buffer ~f:Patterns_or_buffer.to_msgpack_map_entry
    ; maybe "data" data (Type.to_msgpack Object)
    ]
    |> List.filter_opt
    |> String.Map.of_alist_exn
  in
  Nvim_internal.nvim_exec_autocmds ~event ~opts |> run ~here client
;;

module Private = struct
  let vcaml_internal_group client =
    let client = Type_equal.(conv Client.Private.eq) client in
    Set_once.get_exn client.vcaml_internal_group
  ;;
end
