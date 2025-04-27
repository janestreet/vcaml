open Core
open Async
open Import

include module type of struct
  include Nvim_internal.Buffer
end

module Event :
  module type of Subscription_manager.Buffer_event
  with module Private := Subscription_manager.Buffer_event.Private

(** A [changedtick] represents the edit number of the buffer. If you pass it to a function
    that updates the buffer contents, the contents will only be updated if the buffer has
    not changed since that changedtick. See `:h b:changedtick`. *)
type changedtick = Event.changedtick [@@deriving sexp_of]

module With_changedtick : sig
  type 'a t =
    { value : 'a
    ; changedtick : changedtick
    }
  [@@deriving sexp_of]

  val value : 'a t -> 'a
end

val get_changedtick
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> changedtick Deferred.Or_error.t

val get_name
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> string Deferred.Or_error.t

val set_name
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> string
  -> unit Deferred.Or_error.t

(** Read a range of lines from a buffer. Indexing is zero-based, end-exclusive. *)
val get_lines
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> start:int
  -> end_:int
  -> strict_indexing:bool
  -> String.Utf8.t list With_changedtick.t Deferred.Or_error.t

(** This function should only be used for linewise replacement when you don't care about
    preserving marks. If you want to preserve marks and/or want characterwise replacement,
    prefer [set_text]. Indexing is zero-based, end-exclusive. The character encoding
    should be UTF-8 (this function takes a [string list] rather than a
    [String.Utf8.t list] for ergonomic reasons; validity can be tested with
    [String.Utf8.is_valid]). *)
val set_lines
  :  here:[%call_pos]
  -> _ Client.t
  -> ?changedtick:changedtick
  -> Or_current.t
  -> start:int
  -> end_:int
  -> strict_indexing:bool
  -> string list
  -> unit Deferred.Or_error.t

(** Similar to [get_lines], but supports retrieving only portions of a line. If you only
    need full lines, prefer [get_lines]. Indexing is zero-based, end-exclusive. *)
val get_text
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> start_row:int
  -> start_col:int
  -> end_row:int
  -> end_col:int
  -> String.Utf8.t list With_changedtick.t Deferred.Or_error.t

(** Replace text in a buffer. This supports setting portions of a line, which can help
    preserve marks. If you don't care about mark preservation and have an update expressed
    in terms of full lines, prefer [set_lines]. Indexing is zero-based, end-exclusive. The
    character encoding should be UTF-8 (this function takes a [string list] rather than a
    [String.Utf8.t list] for ergonomic reasons; validity can be tested with
    [String.Utf8.is_valid]). *)
val set_text
  :  here:[%call_pos]
  -> _ Client.t
  -> ?changedtick:changedtick
  -> Or_current.t
  -> start_row:int
  -> start_col:int
  -> end_row:int
  -> end_col:int
  -> string list
  -> unit Or_error.t Deferred.t

(** Create a new buffer. See `:h 'buflisted'` and `:h scratch-buffer` for explanations of
    [listed] and [scratch] respectively. *)
val create
  :  here:[%call_pos]
  -> _ Client.t
  -> listed:bool
  -> scratch:bool
  -> t Deferred.Or_error.t

(** Create a new buffer with the given name or if one already exists, return it. A buffer
    created by this function will be unlisted. *)
val find_by_name_or_create
  :  here:[%call_pos]
  -> _ Client.t
  -> string
  -> t Deferred.Or_error.t

(** Unload the buffer from memory. The buffer's marks, settings, etc. are preserved. For
    details, see `:h bunload`.

    In Neovim, buffer "deletion" (see `:h bdelete`) refers to unloading the buffer,
    "unlisting" the buffer, and clearing buffer-local option values, variables, mappings,
    and commands.[^1] The buffer list is roughly the list of "discoverable" buffers.
    Unloading and unlisting are two independent concepts, and there isn't usually a
    situation in which you would want to do both of these together, since whether a buffer
    should be discoverable usually depends more on the purpose of the buffer and whether
    it should be loaded depends more on user activity. You can add/remove the buffer
    to/from the buffer list by setting the [Buflisted] option. There's also a subtle
    corner case: the last buffer cannot be unloaded unless it is unlisted (see `:h E90`).

    Plugin authors who are just casually looking to delete a buffer they created and don't
    want to think hard about these options should reach for [wipeout] below.

    [^1]: However, the [BufDelete] event triggers solely in response to unlisting,
    irrespective of whether the buffer is loaded (indeed, it may even be visible). *)
val unload
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> even_if_modified:bool
  -> unit Deferred.Or_error.t

(** Totally delete the buffer (including some settings that would be preserved on normal
    "deletion," such as marks, channels, and buffer-local autocmds). `:h bwipeout` has a
    cautionary note about doing this, but that note applies more to regular users than to
    plugin authors wiping out buffers created by their plugin. *)
val wipeout
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> even_if_modified:bool
  -> unit Deferred.Or_error.t

(** Will be [false] after calling [unload] on [t]. *)
val loaded : here:[%call_pos] -> _ Client.t -> t -> bool Deferred.Or_error.t

(** Will be [false] after calling [wipeout] on [t]. *)
val exists : here:[%call_pos] -> _ Client.t -> t -> bool Deferred.Or_error.t

(** Subscribe to updates from a buffer. Only one subscription per buffer is allowed at a
    time. To unsubscribe, close the returned pipe. *)
val subscribe
  :  here:[%call_pos]
  -> _ Client.t
  -> ?send_buffer:bool (** default: [true] *)
  -> Or_current.t
  -> Event.t Pipe.Reader.t Deferred.Or_error.t

(** Get a buffer variable (see `:h b:`). *)
val get_var
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> string
  -> type_:'a Type.t
  -> 'a Deferred.Or_error.t

(** Set a buffer variable (see `:h b:`). Before using this, note that users have the
    freedom to change the values of these variables. If that would be undesirable, keep
    your state management inside your plugin. *)
val set_var
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> string
  -> type_:'a Type.t
  -> value:'a
  -> unit Deferred.Or_error.t

(** Delete a buffer variable (see `:h b:`). *)
val delete_var
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> string
  -> unit Deferred.Or_error.t

(** Get a native lowercase mark (see `:h mark-motions`). Use [Nvim.get_mark] to get an
    uppercase mark. *)
val get_mark
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> sym:char
  -> Mark.t With_changedtick.t Deferred.Or_error.t

(** Set a native lowercase mark (see `:h mark-motions`). To create a mark that will only
    be controlled by your plugin, use an [Extmark.t]. *)
val set_mark
  :  here:[%call_pos]
  -> _ Client.t
  -> ?changedtick:changedtick
  -> Or_current.t
  -> Mark.t
  -> unit Deferred.Or_error.t

(** Delete a native lowercase mark (see `:h mark-motions`). *)
val delete_mark
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> char
  -> unit Deferred.Or_error.t

(** Returns the number of lines in the buffer. *)
val line_count
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> int With_changedtick.t Deferred.Or_error.t

(** Return the byte offset of the given line. Note that because Neovim buffers are encoded
    in UTF-8 and files that are not encoded in UTF-8 are converted when reading/writing,
    if a non-UTF-8 file is loaded into a buffer and then this function is called on a line
    in the buffer the offset (which is w.r.t. the UTF-8 encoding) may be different from
    the line's byte offset on disk. *)
val get_byte_offset_of_line
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> line:int
  -> int With_changedtick.t Deferred.Or_error.t

(** Open a terminal not connected to a process in this buffer. Useful for displaying
    ANSI-coded text. Returns the channel for use with [Nvim.send_to_channel]. *)
val open_term : here:[%call_pos] -> _ Client.t -> Or_current.t -> int Deferred.Or_error.t

module Option : sig
  (*$ Vcaml_cinaps.generate_options_intf ~scope:Buffer *)
  (** Buffer-specific Neovim options. The ['global] phantom type represents the notion of
      a "global" value for the option. [`none] means setting the global value has no
      effect. [`global] means there is a global value that can be locally overridden.
      [`copied] means the global value is copied to the local value on buffer creation, so
      setting it will only affect new buffers. *)
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
  (*$*)

  (** Get the effective value of the option for the given buffer. *)
  val get
    :  here:[%call_pos]
    -> _ Client.t
    -> Or_current.t
    -> ('a, _) t
    -> 'a Deferred.Or_error.t

  val set
    :  here:[%call_pos]
    -> _ Client.t
    -> Or_current.t
    -> ('a, _) t
    -> 'a
    -> unit Deferred.Or_error.t

  (** Get the global value of the option used by all buffers without local overrides. *)
  val get_default
    :  here:[%call_pos]
    -> _ Client.t
    -> ('a, [ `global ]) t
    -> 'a Deferred.Or_error.t

  (** Set the option for all buffers without local overrides. *)
  val set_default
    :  here:[%call_pos]
    -> _ Client.t
    -> ('a, [ `global ]) t
    -> 'a
    -> unit Deferred.Or_error.t

  (** Get the value that new buffers will inherit for this option. *)
  val get_for_new_buffers
    :  here:[%call_pos]
    -> _ Client.t
    -> ('a, [ `copied ]) t
    -> 'a Deferred.Or_error.t

  (** Set the option for buffers created in the future. *)
  val set_for_new_buffers
    :  here:[%call_pos]
    -> _ Client.t
    -> ('a, [ `copied ]) t
    -> 'a
    -> unit Deferred.Or_error.t

  val get_dynamic_info
    :  here:[%call_pos]
    -> _ Client.t
    -> ('a, _) t
    -> 'a Dynamic_option_info.t Deferred.Or_error.t
end

module Untested : sig
  val add_highlight
    :  here:[%call_pos]
    -> _ Client.t
    -> ?changedtick:changedtick
    -> Or_current.t
    -> namespace:Namespace.t
    -> hl_group:string
    -> line:int
    -> col_start:int
    -> col_end:int
    -> unit Deferred.Or_error.t

  val clear_namespace
    :  here:[%call_pos]
    -> _ Client.t
    -> Or_current.t
    -> namespace:Namespace.t
    -> line_start:int
    -> line_end:int
    -> unit Deferred.Or_error.t

  module Extmark : sig
    (** An [Extmark.t] represents a logical location in a buffer. It can be used to
        represent a cursor, fold, misspelled word, etc. They are associated with a
        [Namespace.t] so that your plugin can manage only its own [Extmark.t]s and ignore
        those created by other plugins. *)
    type buffer := t

    type t = private
      { id : int
      ; namespace : Namespace.t
      ; buffer : buffer
      }
    [@@deriving sexp_of]

    include Comparable.S_plain with type t := t
    include Hashable.S_plain with type t := t
  end

  val get_extmark
    :  here:[%call_pos]
    -> _ Client.t
    -> Extmark.t
    -> Position.t option With_changedtick.t Deferred.Or_error.t

  val get_extmark_with_details
    :  here:[%call_pos]
    -> _ Client.t
    -> ?hl_groups:[ `Ids | `Names ]
    -> Extmark.t
    -> (Position.t * Msgpack.t String.Map.t) option With_changedtick.t Deferred.Or_error.t

  val all_extmarks
    :  here:[%call_pos]
    -> _ Client.t
    -> ?start_inclusive:Position.t
    -> ?end_inclusive:Position.t
    -> ?limit:int
    -> ?type_:[ `Highlight | `Sign | `Virtual_text | `Virtual_lines ]
    -> Or_current.t
    -> namespace:Namespace.t
    -> (Extmark.t * Position.t) list With_changedtick.t Deferred.Or_error.t

  val all_extmarks_with_details
    :  here:[%call_pos]
    -> _ Client.t
    -> ?start_inclusive:Position.t
    -> ?end_inclusive:Position.t
    -> ?limit:int
    -> ?type_:[ `Highlight | `Sign | `Virtual_text | `Virtual_lines ]
    -> ?hl_groups:[ `Ids | `Names ]
    -> Or_current.t
    -> namespace:Namespace.t
    -> (Extmark.t * Position.t * Msgpack.t String.Map.t) list With_changedtick.t
         Deferred.Or_error.t

  (** See `:h nvim_buf_set_extmark` for differences in treatment of [virt_text] and
      [virt_lines]. *)
  type 'a with_extmark_options :=
    ?end_exclusive:Position.t
    -> ?hl_group:string
    -> ?virtual_text:Highlighted_text.t
    -> ?virtual_text_pos:[ `Eol | `Overlay | `Right_align | `At_column of int ]
    -> ?hide_virtual_text_when_overlaying_selection:bool
    -> ?virtual_lines:Highlighted_text.t list
    -> ?virtual_lines_pos:[ `Above | `Below ]
    -> ?bypass_sign_and_number_columns:bool
    -> ?when_underlying_highlight_conflicts:[ `Override | `Combine_with_bg | `Blend ]
    -> ?extend_highlight_across_screen:bool
    -> ?ephemeral:bool (** For use with [Nvim.Untested.Expert.set_decoration_provider] *)
    -> ?start_gravity:[ `Right | `Left ]
    -> ?end_gravity:[ `Right | `Left ]
    -> ?priority:int (** Higher numbers take precedence. *)
    -> ?strict:bool
    -> ?sign_text:string
    -> ?sign_hl_group:string
    -> ?number_hl_group:string
    -> ?line_hl_group:string
    -> ?cursorline_hl_group:string
    -> ?conceal:[ `With_default | `With of char ]
    -> ?spell:bool
    -> ?ui_watched:bool
    -> unit
    -> 'a

  val create_extmark
    :  here:[%call_pos]
    -> _ Client.t
    -> ?changedtick:changedtick
    -> Or_current.t
    -> namespace:Namespace.t
    -> start_inclusive:Position.t
    -> Extmark.t Deferred.Or_error.t with_extmark_options

  val update_extmark
    :  here:[%call_pos]
    -> _ Client.t
    -> ?changedtick:changedtick
    -> Extmark.t
    -> start_inclusive:Position.t
    -> unit Deferred.Or_error.t with_extmark_options

  val delete_extmark
    :  here:[%call_pos]
    -> _ Client.t
    -> Extmark.t
    -> unit Deferred.Or_error.t
end
