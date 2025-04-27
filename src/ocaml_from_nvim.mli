open Core
open Async
open Import

(** The functions in [Ocaml_from_nvim] handle RPC calls from Neovim to VCaml. *)

module Blocking : sig
  (** [t] represents the type of a blocking RPC that Neovim can call with [rpcrequest]. *)
  type 'fn t

  val return : 'a Type.t -> 'a Deferred.Or_error.t t
  val ( @-> ) : 'a Type.t -> 'b t -> ('a -> 'b) t

  module Expert : sig
    val varargs
      :  args_type:'a Type.t
      -> return_type:'b Type.t
      -> ('a list -> 'b Deferred.Or_error.t) t
  end
end

module Async : sig
  (** [t] represents the type of an async RPC that Neovim can call with [rpcnotify]. *)
  type 'fn t

  val unit : unit Deferred.Or_error.t t
  val ( @-> ) : 'a Type.t -> 'b t -> ('a -> 'b) t

  module Expert : sig
    val varargs : 'a Type.t -> ('a list -> unit Deferred.Or_error.t) t
  end
end

(** [register_request_blocking] registers a blocking RPC that can be called from Neovim
    via [rpcrequest]. Neovim will be blocked from processing user input or communicating
    with other channels until a response is returned. Neovim will continue to process
    calls sent over the same channel while a blocking request is in flight, which means
    nested calls are supported.

    When the user presses Ctrl-C to interrupt a blocking call, the call will be aborted.
    [on_keyboard_interrupt] can be passed to handle any necessary cleanup.

    After a blocking request completes or is interrupted by Ctrl-C, any further uses of
    the client will result in an error (e.g., do not store the client in the state). Use
    [run_in_background] to launch a background job that may persist beyond the callback.
    Background jobs will not start until after the blocking request completes or is
    interrupted. *)
val register_request_blocking
  :  ?on_keyboard_interrupt:(unit -> unit)
  -> here:[%call_pos]
  -> _ Client.Maybe_connected.t
  -> name:string
  -> type_:'fn Blocking.t
  -> f:
       (run_in_background:
          (here:[%call_pos]
           -> ([ `asynchronous ] Client.t -> unit Deferred.Or_error.t)
           -> unit)
        -> client:[ `blocking ] Client.t
        -> 'fn)
  -> unit

(** [register_request_async] registers an asynchronous RPC that can be called from Neovim
    via [rpcnotify]. When [f] runs, Neovim may be in a different state than it was at the
    time when the request was made. *)
val register_request_async
  :  here:[%call_pos]
  -> _ Client.Maybe_connected.t
  -> name:string
  -> type_:'fn Async.t
  -> f:(client:[ `asynchronous ] Client.t -> 'fn)
  -> unit

(** Neovim supports a broadcast mode for [rpcnotify] (when the channel argument is 0).
    When the RPC [name] is called from an [rpcnotify] broadcast, the RPC will only be sent
    to channels that have subscribed to broadcasts for [name]. *)
val subscribe_to_broadcast
  :  here:[%call_pos]
  -> _ Client.t
  -> name:string
  -> unit Deferred.Or_error.t

val unsubscribe_from_broadcast
  :  here:[%call_pos]
  -> _ Client.t
  -> name:string
  -> unit Deferred.Or_error.t

module Callback : sig
  type 'a anon_rpc

  (** This type is used by certain API functions that accept a VimL expression or command
      as a body, e.g., when defining a command or keymap. Instead of providing VimL, you
      can provide an OCaml-based implementation via an anonymous RPC. *)
  type 'a t =
    | Viml of string
    | Rpc of 'a anon_rpc

  (** An anonymous blocking RPC that is provided as a callback to an API function. It will
      be registered before the API function is invoked (i.e., before the command or keymap
      is defined). If the API function is scoped to a buffer, the RPC will be freed when
      the buffer is deleted; if it is globally scoped, the RPC will never be freed.
      Returns a ['a t] instead of ['a anon_rpc] for ergonomics. *)
  val anon_rpc
    :  ?on_keyboard_interrupt:(unit -> unit)
    -> (run_in_background:
          (here:[%call_pos]
           -> ([ `asynchronous ] Client.t -> unit Deferred.Or_error.t)
           -> unit)
        -> client:[ `blocking ] Client.t
        -> 'a Deferred.Or_error.t)
    -> 'a t
end

module Private : sig
  val register_request_blocking
    :  ?on_keyboard_interrupt:(unit -> unit)
    -> here:[%call_pos]
    -> _ Client.Maybe_connected.t
    -> name:string
    -> type_:'fn Blocking.t
    -> f:
         ('b
          -> run_in_background:
               (here:[%call_pos]
                -> ([ `asynchronous ] Client.t -> unit Deferred.Or_error.t)
                -> unit)
          -> client:[ `blocking ] Client.t
          -> 'fn)
    -> wrap_f:(('b -> Msgpack.t Deferred.Or_error.t) -> Msgpack.t Deferred.Or_error.t)
    -> unit

  val register_request_async
    :  here:[%call_pos]
    -> _ Client.Maybe_connected.t
    -> name:string
    -> type_:'fn Async.t
    -> f:('b -> client:[ `asynchronous ] Client.t -> 'fn)
    -> wrap_f:(('b -> unit Deferred.Or_error.t) -> (unit, Error.t) result Deferred.t)
    -> unit

  val register_callback
    :  here:[%call_pos]
    -> _ Client.t
    -> return_type:'a Type.t
    -> 'a Callback.anon_rpc
    -> string
end
[@@alert vcaml_private "This module is for internal VCaml use."]
