open! Core
open! Async
open Vcaml

module Rpc_handler = struct
  type 'state t =
    | Sync_handler :
        { name : string
        ; type_ : ('fn, 'leftmost) Defun.Ocaml.Sync.t
        ; f :
            Client.t
            -> 'state
            -> shutdown:(unit -> unit)
            -> keyboard_interrupted:unit Deferred.t
            -> 'fn
        }
        -> 'state t
    | Async_handler :
        { name : string
        ; here : Source_code_position.t
        ; type_ : 'fn Defun.Ocaml.Async.t
        ; f : Client.t -> 'state -> shutdown:(unit -> unit) -> 'fn
        ; on_error : (Error.t -> unit) option
        }
        -> 'state t

  let create_sync ~name ~type_ ~f = Sync_handler { name; type_; f }

  let create_async ?on_error here ~name ~type_ ~f =
    Async_handler { name; here; type_; f; on_error }
  ;;
end

module type Error_handlers = sig
  (** [on_error] is invoked when VCaml encounters a problem, e.g., failing to parse a
      response from Neovim. It is also the default error handler in cases where an
      optional error handler is not provided for handling errors for specific kinds of
      messages. *)
  val on_error : Error.t -> unit

  (** [on_error_event] is invoked when Neovim sends us an asynchronous error event to
      inform us that it encountered a problem with a message we sent. *)
  val on_error_event : Error_type.t -> message:string -> unit
end

(** For apps that are happy to crash when encountering a VCaml / Neovim error. *)
module Raise_on_any_error : Error_handlers = struct
  let on_error = Error.raise

  let on_error_event error_type ~message =
    raise_s [%message message (error_type : Error_type.t)]
  ;;
end

module type Oneshot_arg = sig
  include Error_handlers

  val execute : Client.t -> unit Deferred.Or_error.t
end

module type Oneshot_s = sig
  val run : unit -> unit Deferred.Or_error.t
  val run_for_testing : Client.t -> unit Deferred.Or_error.t
  val command : summary:string -> unit -> Core.Command.t
end

module type Persistent_arg = sig
  include Error_handlers

  type state

  val rpc_handlers : state Rpc_handler.t list
  val startup : Client.t -> shutdown:(unit -> unit) -> state Deferred.Or_error.t

  (** This VimL function should take a single integer argument, which will be the channel
      ID. It should return 0 on success and a non-zero value on failure (if [return] is
      omitted 0 will be returned by default). *)
  val vimscript_notify_fn : string option

  val on_shutdown : Client.t -> state -> unit Deferred.Or_error.t
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

module type Vcaml_plugin = sig
  module Raise_on_any_error = Raise_on_any_error

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
end
