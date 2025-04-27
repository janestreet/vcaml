open! Core
open Async
module Event = Nvim_internal.Ui_event

(** This module supports attaching a UI to Neovim. See `:h ui.txt` for details. *)

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
  [@@deriving fields ~iterators:(make_creator, to_list), sexp_of]

  val default : t
end

module Description : sig
  type t =
    { channel : [ `Tui | `Id of int ]
    ; height : int
    ; width : int
    ; options : Options.t
    }
  [@@deriving sexp_of]
end

val describe_attached_uis
  :  here:[%call_pos]
  -> _ Client.t
  -> Description.t list Deferred.Or_error.t

(** Attach a UI to Neovim. Note that setting certain UI options implicitly sets other UI
    options despite the value passed here. See `:h ui-option` for details. *)
val attach
  :  here:[%call_pos]
  -> _ Client.t
  -> width:int
  -> height:int
  -> options:Nvim_internal.Ui_options.t
  -> only_enable_options_supported_by_other_attached_uis:bool
  -> Event.t Pipe.Reader.t Deferred.Or_error.t

module Untested : sig
  val set_focus
    :  here:[%call_pos]
    -> _ Client.t
    -> [ `Gained | `Lost ]
    -> unit Deferred.Or_error.t

  val try_resizing_grid
    :  here:[%call_pos]
    -> _ Client.t
    -> grid:int
    -> width:int
    -> height:int
    -> unit Deferred.Or_error.t

  (** Set the number of visible items in the menu. *)
  val popup_menu_set_height
    :  here:[%call_pos]
    -> _ Client.t
    -> height:int
    -> unit Deferred.Or_error.t

  (** Set the bounding box of the menu, including borders and sliders. *)
  val popup_menu_set_bounds
    :  here:[%call_pos]
    -> _ Client.t
    -> width:float
    -> height:float
    -> row:float
    -> col:float
    -> unit Deferred.Or_error.t
end
