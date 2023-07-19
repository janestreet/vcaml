open Core
open Async
open Import0

(** This module is internal to VCaml. See [Vcaml.Client] for the public interface. *)

type 'kind t

module Not_connected : sig
  type t

  val name : t -> string
end

val create : name:string -> on_error:(Vcaml_error.t -> unit) -> Not_connected.t

val connect
  :  ?time_source:Time_source.t
  -> Not_connected.t
  -> Reader.t
  -> Writer.t
  -> [ `asynchronous ] t Deferred.Or_error.t

val block_nvim
  :  Source_code_position.t
  -> [ `asynchronous ] t
  -> f:([ `blocking ] t -> 'a Deferred.Or_error.t)
  -> 'a Deferred.Or_error.t

val block_nvim'
  :  Source_code_position.t
  -> [ `asynchronous ] t
  -> f:([ `blocking ] t -> 'a Deferred.Or_error.t)
  -> [ `Ok of 'a | `Keyboard_interrupted | `Error of Error.t ] Deferred.t

module Private : sig
  type 'kind public := 'kind t

  module Callbacks : sig
    type async = [ `asynchronous ] t -> Msgpack.t list -> unit Deferred.Or_error.t

    type blocking =
      run_in_background:
        (Source_code_position.t
         -> f:([ `asynchronous ] public -> unit Deferred.Or_error.t)
         -> unit)
      -> [ `blocking ] t
      -> Msgpack.t list
      -> Msgpack.t Deferred.Or_error.t
  end

  module Not_connected : sig
    type t = Not_connected.t

    val name : t -> string

    val register_request_async
      :  Source_code_position.t
      -> t
      -> name:string
      -> f:Callbacks.async
      -> unit

    val register_request_blocking
      :  Source_code_position.t
      -> t
      -> name:string
      -> f:Callbacks.blocking
      -> on_keyboard_interrupt:(unit -> unit)
      -> unit
  end

  module Message_type : sig
    type ('in_, 'out) t =
      | Request : ('a, 'a) t
      | Notification : ('a, unit) t
  end

  module Method_info : T

  type 'kind t =
    { channel : int Set_once.t (** Guaranteed to be set. *)
    ; name : string
    ; register_request_async :
        Source_code_position.t -> name:string -> f:Callbacks.async -> unit
    ; register_request_blocking :
        Source_code_position.t
        -> name:string
        -> f:Callbacks.blocking
        -> on_keyboard_interrupt:(unit -> unit)
        -> unit
    ; unregister_request_blocking : name:string -> unit
    ; registered_methods : unit -> Method_info.t String.Map.t
    ; call_nvim_api_fn :
        'a 'b.
          Source_code_position.t
        -> ('a, 'b) Message_type.t
        -> 'a Api_result.t
        -> 'b Deferred.Or_error.t
    ; keyboard_interrupts : (unit, read_write) Bvar.t
    ; on_error : Vcaml_error.t -> unit
    ; notify_nvim_of_error : Source_code_position.t -> Error.t -> unit Deferred.t
    ; subscription_manager : Subscription_manager.t
    ; close : unit -> unit Deferred.t
    }

  val eq : ('kind public, 'kind t) Type_equal.t

  val nvim_set_client_info
    :  Source_code_position.t
    -> 'a t
    -> ?version:Client_info.Version.t
    -> ?attributes:string String.Map.t
    -> ?client_type:Client_info.Client_type.t
    -> unit
    -> unit Deferred.Or_error.t

  val nvim_list_chans
    :  Source_code_position.t
    -> _ t
    -> Channel_info.t list Deferred.Or_error.t

  val before_sending_response_hook_for_tests : (unit -> unit Deferred.t) option ref
end
