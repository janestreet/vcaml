open Core
open Async
module Options = Nvim_internal.Ui_options
module Event = Nvim_internal.Ui_event

module Description : sig
  type t =
    { channel_id : [ `Tui | `Id of int ]
    ; height : int
    ; width : int
    ; options : Options.t
    }
  [@@deriving sexp_of]
end

val describe_attached_uis : Description.t list Api_call.Or_error.t

type t

val attach
  :  ?on_error:(Error.t -> unit)
  -> Client.t
  -> width:int
  -> height:int
  -> options:Nvim_internal.Ui_options.t
  -> on_event:(Event.t -> unit)
  -> t Deferred.Or_error.t

val detach : t -> unit Deferred.Or_error.t
