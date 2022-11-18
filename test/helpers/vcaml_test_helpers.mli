open Core
open Async
open Vcaml

val neovim_path : string

val with_client
  :  ?args:string list
  -> ?env:([> `Tmpdir of string ] -> Core_unix.env)
  -> ?links:(string * [ `In_path_as | `In_temp_as ] * string) list
  -> ?time_source:Time_source.t
  -> ?on_error:[ `Raise | `Call of Vcaml_error.t -> unit ]
  -> ?before_connecting:([ `not_connected ] Client.t -> unit)
  -> ([ `connected ] Client.t -> 'a Deferred.Or_error.t)
  -> 'a Deferred.t

val simple
  :  Source_code_position.t
  -> 'a Api_call.Or_error.t
  -> ('a -> Sexp.t)
  -> unit Deferred.t

val print_s : ?mach:unit -> Sexp.t -> unit

module Test_ui : sig
  type t

  val attach
    :  ?width:int
    -> ?height:int
    -> Source_code_position.t
    -> [ `connected ] Client.t
    -> t Deferred.Or_error.t

  val detach : t -> Source_code_position.t -> unit Deferred.Or_error.t

  val with_ui
    :  ?width:int
    -> ?height:int
    -> Source_code_position.t
    -> [ `connected ] Client.t
    -> (t -> 'a Deferred.Or_error.t)
    -> 'a Deferred.Or_error.t
end

val get_screen_contents
  :  Source_code_position.t
  -> Test_ui.t
  -> string Deferred.Or_error.t

val wait_until_text
  :  ?timeout:Time_ns.Span.t
  -> Source_code_position.t
  -> Test_ui.t
  -> f:(string -> bool)
  -> string Deferred.Or_error.t

val with_ui_client
  :  ?width:int
  -> ?height:int
  -> ?time_source:Time_source.t
  -> ?on_error:[ `Raise | `Call of Vcaml_error.t -> unit ]
  -> ?before_connecting:([ `not_connected ] Client.t -> unit)
  -> ([ `connected ] Client.t -> Test_ui.t -> 'a Deferred.Or_error.t)
  -> 'a Deferred.t

(* For advanced tests that require a unix domain socket. Be sure to close the client when
   finished with the test. If you are reaching for this to visually debug a failing test,
   use [For_debugging.with_ui_client] instead. *)
val socket_client
  :  ?time_source:Time_source.t
  -> ?on_error:[ `Raise | `Call of Vcaml_error.t -> unit ]
  -> ?before_connecting:([ `not_connected ] Client.t -> unit)
  -> string
  -> [ `connected ] Client.t Deferred.Or_error.t

(** If a test is behaving in a way that is especially surprising, use this module to
    attach it to an Neovim instance you are running so you can observe the effect. Note
    that the client detaching at the end of the test may be too rapid for you to view the
    effects - you may want to add a delay before returning. *)
module For_debugging : sig
  val with_ui_client
    :  ?time_source:Time_source.t
    -> ?on_error:[ `Raise | `Call of Vcaml_error.t -> unit ]
    -> ?before_connecting:([ `not_connected ] Client.t -> unit)
    -> socket:string
    -> ([ `connected ] Client.t -> Test_ui.t -> 'a Deferred.Or_error.t)
    -> 'a Deferred.t
end
