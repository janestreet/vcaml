module Type := Nvim_internal.Phantom

include module type of struct
  include Nvim_internal.Tabpage
end

module Untested : sig
  val list_wins : tabpage:t -> Nvim_internal.Window.t list Api_call.Or_error.t
  val get_var : tabpage:t -> name:string -> type_:'a Type.t -> 'a Api_call.Or_error.t

  val set_var
    :  tabpage:t
    -> name:string
    -> type_:'a Type.t
    -> value:'a
    -> unit Api_call.Or_error.t

  val del_var : tabpage:t -> name:string -> unit Api_call.Or_error.t
  val get_win : tabpage:t -> Nvim_internal.Window.t Api_call.Or_error.t
  val get_number : tabpage:t -> int Api_call.Or_error.t
  val is_valid : tabpage:t -> bool Api_call.Or_error.t
end
