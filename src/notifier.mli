open Async
open Import

(** [Notifier] supports sending API calls as asynchronous notifications. In most cases you
    don't need this; you can just send a regular request and not wait for the result.
    Notifications can be helpful when you need to send a second request before the first
    request completes; e.g., when the first request opens a prompt and the second request
    responds to it (a pattern more likely to arise in tests). Requests are sequenced, so
    this pattern requires the first request be sent as a notification. There are also some
    API calls that are documented as being more performant when sent as notifications. *)

(** This module is analogous to [Nvim.Func] but is used for wrapping VimL / Lua functions
    to invoke as notifications rather than as requests. *)
module Func : sig
  type 'fn t

  (** N.B. If you are wrapping a function that takes no arguments, just use [unit]. Do not
      use [Nil @-> unit]. *)
  val unit : unit Deferred.Or_error.t t

  val ( @-> ) : 'a Type.t -> 'b t -> ('a -> 'b) t
end

(** This function is analogous to [Nvim.call_function] but is used for calling functions
    as notifications. *)
val notify
  :  here:[%call_pos]
  -> _ Client.t
  -> name:[ `Viml of string | `Lua of string ]
  -> type_:'fn Func.t
  -> 'fn

module Untested : sig
  (** From `:h api-highlights`: If an external highlighter plugin wants to add many
      highlights in a batch, performance can be improved by calling
      [nvim_buf_add_highlight] as an asynchronous notification, after first
      (synchronously) requesting a source id. *)
  val nvim_buf_add_highlight
    :  here:[%call_pos]
    -> _ Client.t
    -> Nvim_internal.Buffer.Or_current.t
    -> namespace:Namespace.t
    -> hl_group:string
    -> line:int
    -> col_start:int
    -> col_end:int
    -> unit Deferred.Or_error.t
end
