open! Core
open! Async
open Vcaml

(** This library provides helpful abstractions for common patterns for interfacing with
    Neovim.

    [Oneshot] plugins are launched synchronously from Neovim and shut down after
    performing their task, allowing Neovim to resume. Because they use stdout to
    communicate with Neovim, any logging must be done via stderr (or to a file).

    [Persistent] plugins are launched asynchronously from Neovim and serve both
    synchronous and asynchronous requests.

    If you have a use case that doesn't fit one of these models (e.g., you are starting an
    OCaml app from outside Neovim that needs to talk to a running Neovim instance), use
    the VCaml library directly. *)

module Oneshot = struct
  module Rpc = struct
    type t =
      | Sync_rpc :
          { name : string
          ; type_ : ('fn, 'leftmost) Defun.Ocaml.Sync.t
          ; f :
              keyboard_interrupted:unit Deferred.t
              -> client:[ `connected ] Client.t
              -> 'fn
          }
          -> t

    let create ~name ~type_ ~f = Sync_rpc { name; type_; f }
  end

  module type Arg = sig
    val name : string

    (** [on_error] is invoked when VCaml fails to parse a response from Neovim and when
        Neovim sends us an asynchronous error event to inform us that it encountered a
        problem with a message we sent. *)
    val on_error : [ `Raise | `Call of Vcaml_error.t -> unit ]

    val rpc_handlers : Rpc.t list
  end

  module type S = sig
    (** A function like this should be defined for your plugin:

        {v
           function! s:rpcrequest(...) abort
             let l:job_id = jobstart(['/path/to/exe'], { 'rpc': 1 })
             return call('rpcrequest', [l:job_id] + a:000)
           endfunction
        v}

        Then you can invoke [s:rpcrequest] the same way you would call [rpcrequest] on a
        synchronous callback of a persistent plugin. *)
    val command : summary:string -> Core.Command.t

    (* There is no special [run_for_testing] function exposed for [Oneshot] plugins
       because the interesting "oneshot" aspect comes from launching them from Neovim
       and using an [Stdio] client. Tests using [Embed] clients already have channels to
       Neovim established can just [rpcrequest] the RPCs normally. *)
  end
end

module Persistent = struct
  module Rpc = struct
    type 'state t =
      | Sync_rpc :
          { name : string
          ; type_ : ('fn, 'leftmost) Defun.Ocaml.Sync.t
          ; f :
              'state
              -> shutdown:(unit -> unit)
              -> keyboard_interrupted:unit Deferred.t
              -> client:[ `connected ] Client.t
              -> 'fn
          }
          -> 'state t
      | Async_rpc :
          { name : string
          ; type_ : 'fn Defun.Ocaml.Async.t
          ; f : 'state -> shutdown:(unit -> unit) -> client:[ `connected ] Client.t -> 'fn
          }
          -> 'state t

    let create_sync ~name ~type_ ~f = Sync_rpc { name; type_; f }
    let create_async ~name ~type_ ~f = Async_rpc { name; type_; f }
  end

  module type Arg = sig
    type state [@@deriving sexp_of]

    val name : string

    (** Used as the [summary] argument for [command]. *)
    val description : string

    (** [on_error] is invoked when VCaml fails to parse a response from Neovim and when
        Neovim sends us an asynchronous error event to inform us that it encountered a
        problem with a message we sent. *)
    val on_error : [ `Raise | `Call of Vcaml_error.t -> unit ]

    val rpc_handlers : state Rpc.t list
    val init_state : unit -> state

    val on_startup
      :  [ `connected ] Client.t
      -> state
      -> shutdown:(unit -> unit)
      -> unit Deferred.Or_error.t

    (** If specified, this Vimscript function will be called after [on_startup] finishes.
        It should be used as the indication to Neovim that the plugin is now ready to
        start serving RPCs - before this it's possible to be in a bad state (the RPCs may
        not yet be registered or [on_startup] may not have finished running). It should
        take a single integer argument, which will be the channel ID. *)
    val vimscript_notify_fn : string option

    val on_shutdown : [ `connected ] Client.t -> state -> unit Deferred.Or_error.t
  end

  module For_testing = struct
    module type S = sig
      type plugin_state [@@deriving sexp_of]


      module State : sig
        type t =
          { plugin_state : plugin_state
          ; shutdown : unit -> unit
          ; wait_for_shutdown : unit Or_error.t Deferred.t
          }
      end

      val start : client:[ `connected ] Client.t -> State.t Deferred.Or_error.t
    end
  end

  module type S = sig
    type state

    val command : Core.Command.t

    module For_testing : For_testing.S with type plugin_state = state
  end
end

module type Vcaml_plugin = sig
  module Oneshot : sig
    include module type of struct
      include Oneshot
    end

    module Make (O : Arg) : S
  end

  module Persistent : sig
    include module type of struct
      include Persistent
    end

    module Make (P : Arg) : S with type state := P.state
  end
end
