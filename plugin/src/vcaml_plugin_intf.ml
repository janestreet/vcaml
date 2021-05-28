open! Core
open! Async
open Vcaml

type shutdown_handler = unit -> unit

module Rpc_handler = struct
  type 'state t =
    | Sync_handler :
        { name : string
        ; type_ : ('fn, 'leftmost) Defun.Ocaml.Sync.t
        ; f : Client.t * 'state * shutdown_handler -> 'fn
        }
        -> 'state t
    | Async_handler :
        { name : string
        ; type_ : 'fn Defun.Ocaml.Async.t
        ; f : Client.t * 'state * shutdown_handler -> 'fn
        }
        -> 'state t

  let create_sync ~name ~type_ ~f = Sync_handler { name; type_; f }
  let create_async ~name ~type_ ~f = Async_handler { name; type_; f }
end

module type Oneshot_arg = sig
  (** Default error handler for asynchronous Msgpack RPC errors. *)
  val on_async_msgpack_error : Error.t -> unit

  val execute : Client.t -> unit Deferred.Or_error.t
end

module type Oneshot_s = sig
  val run : unit -> unit Deferred.Or_error.t
  val run_for_testing : Client.t -> unit Deferred.Or_error.t
  val command : summary:string -> unit -> Core.Command.t
end

module type Persistent_arg = sig
  type state

  (** Default error handler for asynchronous Msgpack RPC errors. *)
  val on_async_msgpack_error : Error.t -> unit

  val rpc_handlers : state Rpc_handler.t list
  val startup : Client.t * shutdown_handler -> state Deferred.Or_error.t

  (** This VimL function should take a single integer argument, which will be the channel
      ID. It should return 0 on success and a non-zero value on failure (if [return] is
      omitted 0 will be returned by default). *)
  val vimscript_notify_fn : string option

  val on_shutdown : Client.t * state -> unit Deferred.Or_error.t
end

module type Persistent_s = sig
  type state

  val run : unit -> unit Deferred.Or_error.t
  val command : summary:string -> unit -> Core.Command.t

  val run_for_testing
    :  ?during_plugin:(chan_id:int -> state:state -> unit Deferred.Or_error.t)
    -> Client.t
    -> state Deferred.Or_error.t
end

module type Intf = sig
  module Oneshot : sig
    module type Arg = Oneshot_arg
    module type S = Oneshot_s

    module Make (O : Arg) : S
  end

  module Persistent : sig
    module type Arg = Persistent_arg
    module type S = Persistent_s

    module Make (P : Arg) : S with type state = P.state
  end

  module Rpc_handler = Rpc_handler

  module For_testing : sig
    val with_client
      :  ?on_error:(Error.t -> unit)
      -> (Client.t -> 'a Deferred.Or_error.t)
      -> 'a Deferred.t
  end
end
