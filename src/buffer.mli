module Unshadow_command := Command
open Core
module Command := Unshadow_command
module Type := Nvim_internal.Phantom

include module type of struct
  include Nvim_internal.Buffer
end

module Event : sig
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
end

val get_name : buffer:t -> string Api_call.Or_error.t
val set_name : buffer:t -> name:string -> unit Api_call.Or_error.t

val get_lines
  :  buffer:t
  -> start:int
  -> end_:int
  -> strict_indexing:bool
  -> string list Api_call.Or_error.t

(** This function should only be used for linewise replacement when you don't want to
    preserve marks. If you want to preserve marks and/or want characterwise replacement,
    prefer [set_text]. *)
val set_lines
  :  buffer:t
  -> start:int
  -> end_:int
  -> strict_indexing:bool
  -> replacement:string list
  -> unit Api_call.Or_error.t

val create : listed:bool -> scratch:bool -> t Api_call.Or_error.t

(** A buffer created by this function will be unlisted. *)
val find_by_name_or_create : name:string -> t Api_call.Or_error.t

module Subscriber : sig
  type buffer :=
    [ `Current
    | `Numbered of t
    ]

  type t

  (** Only one [t] should be created for a given client. [on_error] is invoked when we
      receive buffer event messages that we fail to parse (and therefore cannot be
      attributed to a particular buffer). *)
  val create : ?on_error:(Error.t -> unit) -> Client.t -> t


  (** Attach to an existing buffer and receive a pipe of updates pretaining to this
      buffer.

      This function returns a regular [Deferred.t] instead of an [api_call] because it is
      impossible to execute atomically -- due to potential race conditions between
      multiple attaches and detaches, this function is written as multiple api calls to
      neovim that are sequenced according to [Throttle.Sequencer]. This has the side
      effect of potentially running Async cycles during execution, which may in turn
      invoke other neovim operations, violating the atomicity guarantee of [Api_call].

      Attaching to a buffer is unlikely to race with anything. The only operation that may
      interfere with the buffer attach is if the buffer is deleted or otherwise stops
      broadcasting while the attach is in flight. Depending on the exact ordering, then,
      one of two things may happen:

      - If the delete occurs before the attach completes, then the attach will fail with a
        ``nonexistent buffer'' error.
      - If the attach completes first, then it will receive a detach message immediately
        and close itself.

      Both cases are ones the application should be prepared to handle.

      [nvim_buf_detach] isn't directly exposed because it would detach all attached
      clients. Instead, simply close the returned pipe. *)
  val subscribe
    :  ?on_detach_failure:[ `Ignore (* default *) | `Raise | `Call of buffer -> unit ]
    -> ?on_lines:Nvim_internal.Luaref.t
    -> ?on_bytes:Nvim_internal.Luaref.t
    -> ?on_changed_tick:Nvim_internal.Luaref.t
    -> ?on_detach:Nvim_internal.Luaref.t
    -> ?on_reload:Nvim_internal.Luaref.t
    -> ?utf_sizes:bool
    -> ?preview:bool
    -> t
    -> Source_code_position.t
    -> buffer:buffer
    -> send_buffer:bool
    -> Event.t Async.Pipe.Reader.t Async.Deferred.Or_error.t
end

val set_option
  :  buffer:t
  -> scope:[ `Local | `Global ]
  -> name:string
  -> type_:'a Type.t
  -> value:'a
  -> unit Api_call.Or_error.t

(** Get a native nvim mark. To create a mark that will only be controlled by your
    plugin, use an [Extmark.t]. *)
val get_mark : buffer:t -> sym:char -> Mark.t Api_call.Or_error.t

module Untested : sig
  val line_count : buffer:t -> int Api_call.Or_error.t
  val get_var : buffer:t -> name:string -> type_:'a Type.t -> 'a Api_call.Or_error.t
  val get_changedtick : buffer:t -> int Api_call.Or_error.t

  (** Gets a map of buffer-local user-commands. *)
  val get_commands : buffer:t -> Command.t String.Map.t Api_call.Or_error.t

  val set_var
    :  buffer:t
    -> name:string
    -> type_:'a Type.t
    -> value:'a
    -> unit Api_call.Or_error.t

  val del_var : buffer:t -> name:string -> unit Api_call.Or_error.t
  val get_option : buffer:t -> name:string -> type_:'a Type.t -> 'a Api_call.Or_error.t
  val is_loaded : buffer:t -> bool Api_call.Or_error.t
  val is_valid : buffer:t -> bool Api_call.Or_error.t
  val unload : buffer:t -> even_if_modified:bool -> unit Api_call.Or_error.t
  val wipeout : buffer:t -> even_if_modified:bool -> unit Api_call.Or_error.t
  val get_byte_offset_of_line : buffer:t -> line:int -> int Api_call.Or_error.t

  val add_highlight
    :  buffer:t
    -> namespace:Namespace.t
    -> hl_group:string
    -> line:int
    -> col_start:int
    -> col_end:int
    -> int Api_call.Or_error.t

  val clear_namespace
    :  buffer:t
    -> namespace:Namespace.t
    -> line_start:int
    -> line_end:int
    -> unit Api_call.Or_error.t

  val set_text
    :  buffer:t
    -> start_row:int
    -> start_col:int
    -> end_row:int
    -> end_col:int
    -> replacement:string list
    -> unit Or_error.t Api_call.t

  (** You may want to use an [Extmark.t] with virtual text instead. *)
  val set_virtual_text
    :  buffer:t
    -> namespace:Namespace.t
    -> line:int
    -> text:Highlighted_text.t
    -> unit Api_call.Or_error.t

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
    [@@deriving fields, sexp_of]

    include Comparable.S_plain with type t := t
    include Hashable.S_plain with type t := t
  end

  val get_extmark : extmark:Extmark.t -> Position.t option Api_call.Or_error.t

  val get_extmark_with_details
    :  extmark:Extmark.t
    -> (Position.t * Msgpack.t String.Map.t) option Api_call.Or_error.t

  val all_extmarks
    :  buffer:t
    -> namespace:Namespace.t
    -> ?start_inclusive:Position.t
    -> ?end_inclusive:Position.t
    -> unit
    -> (Extmark.t * Position.t) list Api_call.Or_error.t

  val all_extmarks_with_details
    :  buffer:t
    -> namespace:Namespace.t
    -> ?start_inclusive:Position.t
    -> ?end_inclusive:Position.t
    -> unit
    -> (Extmark.t * Position.t * Msgpack.t String.Map.t) list Api_call.Or_error.t

  type 'a with_extmark_options :=
    ?end_exclusive:Position.t
    -> ?hl_group:string
    -> ?virtual_text:Highlighted_text.t
    -> ?virtual_text_pos:[ `Eol | `Overlay | `Right_align | `At_column of int ]
    -> ?hide_virtual_text_when_overlaying_selection:unit
    -> ?when_underlying_highlight_conflicts:[ `Override | `Combine_with_bg | `Blend ]
    -> ?extend_highlight_across_screen:unit
    -> ?ephemeral:unit (* For use with [Nvim.Untested.Expert.set_decoration_provider] *)
    -> ?start_gravity:[ `Right | `Left ]
    -> ?end_gravity:[ `Right | `Left ]
    -> ?priority:int (* Higher numbers take precedence. *)
    -> unit
    -> 'a

  val create_extmark
    :  buffer:t
    -> namespace:Namespace.t
    -> start_inclusive:Position.t
    -> Extmark.t Api_call.Or_error.t with_extmark_options

  val update_extmark
    :  extmark:Extmark.t
    -> start_inclusive:Position.t
    -> unit Api_call.Or_error.t with_extmark_options

  (** Returns [true] if the extmark was found, [false] otherwise. *)
  val delete_extmark : extmark:Extmark.t -> bool Api_call.Or_error.t

  (** Open a terminal not connected to a process in this buffer. Useful for displaying
      ANSI-coded text. Returns the channel id for use with [Nvim.Untested.chan_send].

      NOTE: The buffer is expected to be empty and unmodified, but this is not enforced.
      As far as I can tell there is no way to recover modifications once the buffer is
      converted. The file cannot be reopened until the terminal buffer is closed. *)
  val open_term : buffer:t -> int Api_call.Or_error.t

  module Expert : sig

    (** Call a lua function from the given buffer, and if the buffer isn't in a window in
        the current tabpage a new "autocmd window" will be created temporarily. If this is
        actually useful we can create a wrapper similar to [wrap_viml_function] and expose
        it in a more type-safe way. *)
    val buf_call
      :  buffer:t
      -> lua_callback:Nvim_internal.Luaref.t
      -> Msgpack.t Api_call.Or_error.t
  end
end
