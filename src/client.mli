open Core
open Async

type t

module Private : sig
  type public := t

  module Message_type : sig
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

  val eq : (t, public) Type_equal.t
end
