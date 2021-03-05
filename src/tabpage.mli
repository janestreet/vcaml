include module type of struct
  include Types.Tabpage
end

module Untested : sig
  val list_wins : tabpage:t -> Types.Window.t list Api_call.Or_error.t
  val get_var : tabpage:t -> name:string -> Msgpack.t Api_call.Or_error.t
  val set_var : tabpage:t -> name:string -> value:Msgpack.t -> unit Api_call.Or_error.t
  val del_var : tabpage:t -> name:string -> unit Api_call.Or_error.t
  val get_win : tabpage:t -> Types.Window.t Api_call.Or_error.t
  val get_number : tabpage:t -> int Api_call.Or_error.t
  val is_valid : tabpage:t -> bool Api_call.Or_error.t
end
