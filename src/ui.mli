open Core
open Async
module Event = Nvim_internal.Ui_event

module Options : sig
  type t = Nvim_internal.Ui_options.t =
    { ext_cmdline : bool
    ; ext_hlstate : bool
    ; ext_linegrid : bool
    ; ext_messages : bool
    ; ext_multigrid : bool
    ; ext_popupmenu : bool
    ; ext_tabline : bool
    ; ext_termcolors : bool
    ; ext_wildmenu : bool
    ; rgb : bool
    }
  [@@deriving fields, sexp_of]

  val default : t
end

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

(** Note that setting certain UI options implicitly sets other UI options despite the
    value passed here. See `:h ui-option` for details. *)
val attach
  :  ?on_error:(Error.t -> unit)
  -> Source_code_position.t
  -> Client.t
  -> width:int
  -> height:int
  -> options:Nvim_internal.Ui_options.t
  -> on_event:(Event.t -> unit)
  -> t Deferred.Or_error.t

val detach : t -> Source_code_position.t -> unit Deferred.Or_error.t

module Untested : sig
  val try_resizing_grid : grid:int -> width:int -> height:int -> unit Api_call.Or_error.t

  (** Set the number of visible items in the menu. *)
  val popup_menu_set_height : height:int -> unit Api_call.Or_error.t

  (** Set the bounding box of the menu, including borders and sliders. *)
  val popup_menu_set_bounds
    :  width:float
    -> height:float
    -> row:float
    -> col:float
    -> unit Api_call.Or_error.t
end
