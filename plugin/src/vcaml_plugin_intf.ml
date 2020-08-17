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

module type S = sig
  type state

  val run : unit -> unit Deferred.Or_error.t
  val command : summary:string -> unit -> Command.t

  val test
    :  ?before_plugin:(client:Client.t -> unit Deferred.Or_error.t)
    -> ?during_plugin:
         (client:Client.t -> chan_id:int -> state:state -> unit Deferred.Or_error.t)
    -> ?after_plugin:(client:Client.t -> state:state -> unit Deferred.Or_error.t)
    -> unit
    -> unit Deferred.t
end

module type Oneshot_arg = sig
  val execute : Client.t -> unit Deferred.Or_error.t
end

module type Persistent_arg = sig
  type state

  val rpc_handlers : state Rpc_handler.t list
  val startup : Client.t * shutdown_handler -> state Deferred.Or_error.t
  val vimscript_notify_fn : string option
  val on_shutdown : Client.t * state -> unit Deferred.Or_error.t
end

module type Intf = sig
  module type S = S
  module type Oneshot_arg = Oneshot_arg
  module type Persistent_arg = Persistent_arg

  val setup_buffer_events
    :  client:Client.t
    -> buffer:Buf.t
    -> state:'state
    -> on_buffer_event:('state -> Client.t -> Buf.Event.t -> unit Deferred.Or_error.t)
    -> on_buffer_close:('state -> Client.t -> unit Deferred.Or_error.t)
    -> unit Deferred.Or_error.t

  module Make_oneshot (O : Oneshot_arg) : S with type state = unit
  module Make_persistent (P : Persistent_arg) : S with type state = P.state
  module Rpc_handler = Rpc_handler
  module Testing = Testing
end
