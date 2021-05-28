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

val get_lines
  :  buffer:t
  -> start:int
  -> end_:int
  -> strict_indexing:bool
  -> string list Api_call.Or_error.t

val set_lines
  :  buffer:t
  -> start:int
  -> end_:int
  -> strict_indexing:bool
  -> replacement:string list
  -> unit Api_call.Or_error.t

val find_by_name_or_create : name:string -> t Api_call.Or_error.t


(** Attach to an existing buffer and receive a pipe of updates pretaining to this buffer.

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

  val subscribe
    :  ?on_detach_failure:[ `Ignore (* default *) | `Raise | `Call of buffer -> unit ]
    -> t
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
  val set_name : buffer:t -> name:string -> unit Api_call.Or_error.t
  val is_valid : buffer:t -> bool Api_call.Or_error.t
  val get_mark : buffer:t -> sym:char -> Mark.t Api_call.Or_error.t
  val set_scratch : buffer:t -> unit Api_call.Or_error.t

  val add_highlight
    :  buffer:t
    -> ns_id:int
    -> hl_group:string
    -> line:int
    -> col_start:int
    -> col_end:int
    -> int Api_call.Or_error.t

  val clear_highlight
    :  buffer:t
    -> ns_id:int
    -> line_start:int
    -> line_end:int
    -> unit Api_call.Or_error.t
end
