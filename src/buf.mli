open Async
open Core

type t = Types.Buf.t [@@deriving sexp_of]

module Table : Hashtbl.S with type key = t

type mark =
  { row : int
  ; col : int
  }

type which_buffer =
  [ `Current
  | `Numbered of t
  ]

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

val of_msgpack : Msgpack.t -> t Or_error.t
val to_msgpack : t -> Msgpack.t
val get_name : buffer:t -> string Or_error.t Api_call.t

val get_lines
  :  buffer:t
  -> start:int
  -> end_:int
  -> strict_indexing:bool
  -> string list Or_error.t Api_call.t

val set_lines
  :  buffer:t
  -> start:int
  -> end_:int
  -> strict_indexing:bool
  -> replacement:string list
  -> unit Or_error.t Api_call.t

val find_by_name_or_create : name:string -> t Or_error.t Api_call.t

(** Attach to an existing buffer and receive a pipe of updates pretaining to this buffer.

    The [opts] parameter is currently unused but may become used by neovim in the future.

    This function returns a regular [Deferred.t] instead of an [api_call] because it is
    impossible to execute atomically -- due to potential race conditions between multiple
    attaches and detaches, this function is written as multiple api calls to neovim that
    are sequenced according to [Throttle.Sequencer]. This has the side effect of potentially
    running Async cycles during execution, which may in turn invoke other neovim
    operations, violating the atomicity guarantee of [Api_call].

    Attaching to a buffer is unlikely to race with anything. The only operation that may
    interfere with the buffer attach is if the buffer is deleted or otherwise stops
    broadcasting while the attach is in flight. Depending on the exact ordering, then, one
    of two things may happen:

    - If the delete occurs before the attach completes, then the attach will fail with a
      ``nonexistent buffer'' error.
    - If the attach completes first, then it will receive a detach message immediately
      and close itself.

    Both cases are ones the application should be prepared to handle.

    [nvim_buf_detach] isn't directly exposed because it would detach all attached clients.
    Instead, simply close the returned pipe.
*)
val attach
  :  ?opts:(Msgpack.t * Msgpack.t) list
  -> Types.client
  -> buffer:which_buffer
  -> send_buffer:bool
  -> Event.t Pipe.Reader.t Deferred.Or_error.t

val set_option : buffer:t -> name:string -> value:Msgpack.t -> unit Or_error.t Api_call.t

module Untested : sig
  val line_count : buffer:t -> int Or_error.t Api_call.t
  val get_var : buffer:t -> name:string -> Msgpack.t Or_error.t Api_call.t
  val get_changedtick : buffer:t -> int Or_error.t Api_call.t

  (** Gets a map of buffer-local user-commands.

      The [opts] parameter is not used by the current version of neovim, but may be used in
      the future.
  *)
  val get_commands
    :  ?opts:(Msgpack.t * Msgpack.t) list
    -> buffer:t
    -> Nvim_command.t String.Map.t Or_error.t Api_call.t

  val set_var : buffer:t -> name:string -> value:Msgpack.t -> unit Or_error.t Api_call.t
  val del_var : buffer:t -> name:string -> unit Or_error.t Api_call.t
  val get_option : buffer:t -> name:string -> Msgpack.t Or_error.t Api_call.t
  val set_name : buffer:t -> name:string -> unit Or_error.t Api_call.t
  val is_valid : buffer:t -> bool Or_error.t Api_call.t
  val get_mark : buffer:t -> name:string -> mark Or_error.t Api_call.t
  val set_scratch : buffer:t -> unit Api_call.t

  val add_highlight
    :  buffer:t
    -> ns_id:int
    -> hl_group:string
    -> line:int
    -> col_start:int
    -> col_end:int
    -> int Or_error.t Api_call.t

  val clear_highlight
    :  buffer:t
    -> ns_id:int
    -> line_start:int
    -> line_end:int
    -> unit Or_error.t Api_call.t
end
