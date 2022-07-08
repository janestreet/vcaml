open Core
open Async

type 'state t

val create : on_error:(Vcaml_error.t -> unit) -> [ `not_connected ] t

val connect
  :  [ `not_connected ] t
  -> Async.Reader.t
  -> Async.Writer.t
  -> close_reader_and_writer_on_disconnect:bool
  -> time_source:Time_source.t
  -> [ `connected ] t Deferred.Or_error.t

module Private : sig
  type 'state public := 'state t

  module Message_type : sig
    type ('in_, 'out) t =
      | Request : ('a, 'a Deferred.Or_error.t) t
      | Notification : ('a, unit) t
  end

  module State : sig
    module Connected : sig
      type t =
        { channel : int Set_once.t (* Guaranteed to be set. *)
        ; call_nvim_api_fn :
            'a 'b. 'a Nvim_internal.Api_result.t -> ('a, 'b) Message_type.t -> 'b
        ; buffers_attached : int Nvim_internal.Buffer.Table.t
        ; attach_sequencer : unit Sequencer.t
        ; close : unit -> unit Deferred.t
        }
    end

    module Not_connected : T

    type 'state t =
      | Connected : Connected.t -> [ `connected ] t
      | Not_connected : Not_connected.t -> [ `not_connected ] t
  end

  type 'state t =
    { keyboard_interrupts : (unit, read_write) Bvar.t
    ; events : (Msgpack_rpc.Event.t -> unit) Bus.Read_only.t
    ; register_request_async :
        name:string -> f:([ `connected ] public -> Msgpack.t list -> unit) -> unit
    ; register_request_blocking :
        name:string
        -> f:
             (keyboard_interrupted:unit Deferred.t
              -> [ `connected ] public
              -> Msgpack.t list
              -> Msgpack.t Deferred.Or_error.t)
        -> unit
    ; on_error : Vcaml_error.t -> unit
    ; state : 'state State.t
    }

  val eq : ('state public, 'state t) Type_equal.t
end
