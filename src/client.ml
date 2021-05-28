open! Core
open Async

module Private = struct
  module Message_type = struct
    type ('in_, 'out) t =
      | Request : ('a, 'a Deferred.Or_error.t) t
      | Notification : ('a, unit) t
  end

  type t =
    { events : (Msgpack_rpc.event -> unit) Bus.Read_only.t
    ; call_nvim_api_fn :
        'a 'b. 'a Nvim_internal.Api_result.t -> ('a, 'b) Message_type.t -> 'b
    ; register_request :
        name:string
        -> f:(Msgpack.t list -> Msgpack.t Deferred.Or_error.t)
        -> unit Or_error.t
    ; buffers_attached : int Nvim_internal.Buffer.Table.t
    ; attach_sequencer : unit Sequencer.t
    ; on_error : Error.t -> unit
    }

  let eq = Type_equal.T
end

include Private
