open Core
open Async

(** This module deals with Neovim's autocmd (auto-command) mechanism: commands that are
    automatically run on events. For documentation, see `:h autocmd`. *)

(** Events on which auto-commands can be triggered. For details , see `:h events`. *)
module Event : sig
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

  include Stringable.S with type t := t
  include Comparable.S with type t := t
end

(** A group of auto-commands that can be cleared together. See `:h autocmd-groups`. *)
module Group : sig
  type t [@@deriving sexp_of]

  include Comparable.S_plain with type t := t

  val create
    :  here:[%call_pos]
    -> _ Client.t
    -> ?clear_if_exists:bool
    -> string
    -> t Deferred.Or_error.t

  val delete : here:[%call_pos] -> _ Client.t -> t -> unit Deferred.Or_error.t
end

module Pattern_or_buffer : sig
  type t =
    | Pattern of string
    | Buffer of Nvim_internal.Buffer.t
end

module Patterns_or_buffer : sig
  type t =
    | Patterns of string Nonempty_list.t
    | Buffer of Nvim_internal.Buffer.Or_current.t
end

module Id : sig
  type t [@@deriving compare, sexp_of]
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

(** Get all auto-commands satisfying the given parameters. *)
val get
  :  here:[%call_pos]
  -> _ Client.t
  -> ?group:Group.t
  -> ?events:Event.t Nonempty_list.t
  -> ?patterns_or_buffer:Patterns_or_buffer.t
  -> unit
  -> t list Deferred.Or_error.t

(** Create a new auto-command. *)
val create
  :  here:[%call_pos]
  -> _ Client.t
  -> ?description:string
       (** Provide [description] if you implement this autocmd with an RPC so the user
           will understand what the autocmd does when they inspect it with [:autocmd]. *)
  -> ?once:bool
  -> ?nested:bool
  -> group:Group.t
  -> patterns_or_buffer:Patterns_or_buffer.t
  -> events:Event.t Nonempty_list.t
  -> unit Ocaml_from_nvim.Callback.t
  -> Id.t Deferred.Or_error.t

(** Delete an auto-command. *)
val delete : here:[%call_pos] -> _ Client.t -> Id.t -> unit Deferred.Or_error.t

(** Clear all auto-commands satisfying the given parameters. *)
val clear
  :  here:[%call_pos]
  -> _ Client.t
  -> ?patterns_or_buffer:Patterns_or_buffer.t
  -> unit
  -> group:[ `Group of Group.t | `Not_in_any_group ]
  -> events:Event.t Nonempty_list.t
  -> unit Deferred.Or_error.t

(** Run the auto-commands that satisfy the given parameters. Not typically needed. *)
val exec
  :  here:[%call_pos]
  -> _ Client.t
  -> ?group:Group.t
  -> ?patterns_or_buffer:Patterns_or_buffer.t
  -> ?modeline:bool (** Process modeline after the autocommands. *)
  -> ?data:Msgpack.t (** Arbitrary data to send to the callback. *)
  -> unit
  -> events:Event.t Nonempty_list.t
  -> unit Deferred.Or_error.t

module Private : sig
  val vcaml_internal_group : _ Client.t -> Group.t
end
[@@alert vcaml_private "This module is for internal VCaml use."]
