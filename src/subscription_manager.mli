open Core
open Async
module Buffer := Nvim_internal.Buffer
module Ui_event := Nvim_internal.Ui_event

(** This module is internal to VCaml. *)

module Buffer_event : sig
  type changedtick : immediate [@@deriving sexp_of]

  type t =
    | Changedtick of changedtick
    | Lines of
        { changedtick : changedtick option
        ; firstline : int
        ; lastline : int
        ; linedata : String.Utf8.t list
        ; more : bool
        }
  [@@deriving sexp_of]

  module Private : sig
    val changedtick_eq : (changedtick, int) Type_equal.t
  end
end

(** [Subscription_manager] manages subscriptions to events from Neovim and provides the
    events in a pipe. It is not responsible for making the actual API calls that cause
    events to start and stop being delivered. The usage pattern is:
    {v
      1. Call [subscribe] to get an event pipe.
      2. Make an API call for Neovim to start sending events.
         If the call fails, call [cleanup_failed_subscription].
         If the call succeeds,
           3. Set up a callback for when the pipe is closed to send an API call to Neovim
              to stop sending events and give the pipe t.
           4. Give the event pipe to the user.
    v} *)
type t

val create : Msgpack_rpc.t -> on_error:(Vcaml_error.t -> unit) -> t

val subscribe_to_buffer
  :  t
  -> buffer:Buffer.t
  -> Buffer_event.t Pipe.Reader.t Deferred.Or_error.t

val subscribe_to_ui_events : t -> Ui_event.t Pipe.Reader.t Deferred.Or_error.t
val cleanup_failed_buffer_subscription : t -> buffer:Buffer.t -> unit

(** List of events to which this module subscribes. *)
val events : string list
