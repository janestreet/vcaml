open Core
open Async
open Vcaml

(** This library provides scaffolding for creating Neovim plugins in OCaml.

    [Oneshot] plugins are launched synchronously from Neovim and shut down after
    performing their task, allowing Neovim to resume. Because they use stdout to
    communicate with Neovim, any logging must be done via stderr (or to a file). See
    ../templates/oneshot.vim for a template for invoking a Oneshot plugin from Neovim.

    [Persistent] plugins are launched asynchronously from Neovim and serve both
    synchronous and asynchronous requests. See ../templates/persistent.vim for a template
    for invoking a Persistent plugin from Neovim.

    If you have a use case that doesn't fit one of these models (e.g., you are starting an
    OCaml app from outside Neovim that needs to talk to a running Neovim instance), use
    the VCaml library directly to create and attach a client. *)

module Oneshot : sig
  module Rpc : sig
    type t

    val create
      :  here:[%call_pos]
      -> string
      -> type_:'fn Ocaml_from_nvim.Blocking.t
      -> f:(client:[ `blocking ] Client.t -> 'fn)
      -> t
  end

  (** Create a command to invoke from Neovim to start this oneshot plugin. The plugin will
      shut down after any one of its RPCs is called. The [on_crash] handler can be used to
      do something with an unexpected error just before the plugin crashes - you may want
      to use this to send the error to some logging service. *)
  val create
    :  ?on_crash:(Error.t -> unit Deferred.t)
    -> name:string
    -> description:string
    -> Rpc.t list
    -> Core.Command.t
end

module Persistent : sig
  module Rpc : sig
    type 'state t

    (** This is a wrapper around [Ocaml_from_nvim.register_request_blocking]. See its
        documentation to understand the arguments here. *)
    val create_blocking
      :  ?on_keyboard_interrupt:(unit -> unit)
      -> here:[%call_pos]
      -> string
      -> type_:'fn Ocaml_from_nvim.Blocking.t
      -> f:
           ('state
            -> run_in_background:
                 (here:[%call_pos]
                  -> ([ `asynchronous ] Client.t -> unit Deferred.Or_error.t)
                  -> unit)
            -> client:[ `blocking ] Client.t
            -> 'fn)
      -> 'state t

    (** This is a wrapper around [Ocaml_from_nvim.register_request_async]. See its
        documentation to understand the arguments here. *)
    val create_async
      :  here:[%call_pos]
      -> string
      -> type_:'fn Ocaml_from_nvim.Async.t
      -> f:('state -> client:[ `asynchronous ] Client.t -> 'fn)
      -> 'state t

    (** Given an RPC that takes an ['a] state and a way to produce a ['a] from a ['b],
        return an RPC that takes a ['b] state that when called will convert the state to
        ['a] and invoke the original RPC. *)
    val contra_map : 'a t -> f:('b -> 'a) -> 'b t
  end

  (** Create a command to invoke from Neovim to start this persistent plugin. The plugin
      will not shut down on its own - you must call [exit] or let Neovim SIGTERM the
      plugin when it exits. The [on_crash] handler can be used to do something with an
      unexpected error just before the plugin crashes - you may want to use this to send
      the error to some logging service.

      [notify_fn] specifies a function that will be called after [on_startup]. It is used
      to communicate to Neovim that the plugin is ready. The function is passed the
      channel ID as an argument, which is needed to send RPC requests to the plugin.

      Any RPCs that are invoked before [on_startup] has returned will return an error.
      This can happen if there is logic inside [on_startup] that enables Neovim to invoke
      RPCs (a common example is setting up autocmds that invoke RPCs on certain events).
      Any such logic should be moved to [after_startup] to avoid racing with state
      initialization. *)
  val create
    :  ?on_crash:(Error.t -> unit Deferred.t)
    -> ?after_startup:
         ('state -> client:[ `asynchronous ] Client.t -> unit Deferred.Or_error.t)
    -> name:string
    -> description:string
    -> on_startup:([ `asynchronous ] Client.t -> 'state Deferred.Or_error.t)
    -> notify_fn:[ `Lua of string | `Viml of string ]
    -> 'state Rpc.t list
    -> Core.Command.t

  (** Same as [create] but allows you to specify parameters for the generated command. *)
  val create'
    :  ?on_crash:(Error.t -> unit Deferred.t)
    -> ?after_startup:
         ('state -> client:[ `asynchronous ] Client.t -> unit Deferred.Or_error.t)
    -> name:string
    -> description:string
    -> param:'param Core.Command.Param.t
    -> on_startup:
         ('param -> client:[ `asynchronous ] Client.t -> 'state Deferred.Or_error.t)
    -> notify_fn:[ `Lua of string | `Viml of string ]
    -> 'state Rpc.t list
    -> Core.Command.t
end
