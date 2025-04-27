open Async
open Import

include module type of struct
  include Nvim_internal.Tabpage
end

(** List the windows in the tabpage. *)
val list_wins
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> Nvim_internal.Window.t list Deferred.Or_error.t

(** Get a tabpage variable (see `:h t:`). *)
val get_var
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> string
  -> type_:'a Type.t
  -> 'a Deferred.Or_error.t

(** Set a tabpage variable (see `:h t:`). Before using this, note that users have the
    freedom to change the values of these variables. If that would be undesirable, keep
    your state management inside your plugin. *)
val set_var
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> string
  -> type_:'a Type.t
  -> value:'a
  -> unit Deferred.Or_error.t

(** Delete a tabpage variable (see `:h t:`). *)
val delete_var
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> string
  -> unit Deferred.Or_error.t

(** Get the current window in the tabpage. This is the window that will be the current
    (active) window when the tabpage is the current (active) tabpage. *)
val get_win
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> Nvim_internal.Window.t Deferred.Or_error.t

(** Tabpages are both identified by unique ids (represented by [t]) as well as by relative
    numbers based on the currently available tabs, indexed from 1. This function returns
    the number of the given tab, and will return an error if the tab no longer exists. *)
val get_number : here:[%call_pos] -> _ Client.t -> Or_current.t -> int Deferred.Or_error.t

val exists : here:[%call_pos] -> _ Client.t -> t -> bool Deferred.Or_error.t

module Option : sig
  (*$ Vcaml_cinaps.generate_options_intf ~scope:Tabpage *)
  type 'a t = Cmdheight : int t [@@deriving sexp_of]
  (*$*)

  val get : here:[%call_pos] -> _ Client.t -> 'a t -> 'a Deferred.Or_error.t
  val set : here:[%call_pos] -> _ Client.t -> 'a t -> 'a -> unit Deferred.Or_error.t

  val get_dynamic_info
    :  here:[%call_pos]
    -> _ Client.t
    -> 'a t
    -> 'a Dynamic_option_info.t Deferred.Or_error.t
end
