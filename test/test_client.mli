open! Core
open! Async

val neovim_path : string

module Default : sig
  val on_error : Error.t -> unit
  val on_error_event : Vcaml.Error_type.t -> message:string -> unit
  val time_source : Time_source.t
end

val with_client
  :  ?args:string list
  -> ?env:([> `Tmpdir of string ] -> Core_unix.env)
  -> ?links:(string * [ `In_path_as | `In_temp_as ] * string) list
  -> ?on_error:(Error.t -> unit)
  -> ?on_error_event:(Vcaml.Error_type.t -> message:string -> unit)
  -> ?time_source:Time_source.t
  -> (Vcaml.Client.t -> 'a Deferred.Or_error.t)
  -> 'a Deferred.t

val simple
  :  Source_code_position.t
  -> 'a Vcaml.Api_call.Or_error.t
  -> ('a -> Sexp.t)
  -> unit Deferred.t

val print_s : ?mach:unit -> Sexp.t -> unit

module Test_ui : sig
  type t

  val attach
    :  ?width:int
    -> ?height:int
    -> Source_code_position.t
    -> Vcaml.Client.t
    -> t Deferred.Or_error.t

  val detach : t -> Source_code_position.t -> unit Deferred.Or_error.t

  val with_ui
    :  ?width:int
    -> ?height:int
    -> Source_code_position.t
    -> Vcaml.Client.t
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
  :  ?on_error:(Error.t -> unit)
  -> ?on_error_event:(Vcaml.Error_type.t -> message:string -> unit)
  -> ?width:int
  -> ?height:int
  -> ?time_source:Time_source.t
  -> (Vcaml.Client.t -> Test_ui.t -> 'a Deferred.Or_error.t)
  -> 'a Deferred.t

(* For advanced tests that require a unix domain socket. Be sure to close the client when
   finished with the test. If you are reaching for this to visually debug a failing test,
   use [For_debugging.with_ui_client] instead. *)
val socket_client
  :  ?on_error:(Error.t -> unit)
  -> ?on_error_event:(Vcaml.Error_type.t -> message:string -> unit)
  -> ?time_source:Time_source.t
  -> string
  -> Vcaml.Client.t Deferred.Or_error.t

(** If a test is behaving in a way that is especially surprising, use this module to
    attach it to an Neovim instance you are running so you can observe the effect. Note
    that the client detaching at the end of the test may be too rapid for you to view the
    effects - you may want to add a delay before returning. *)
module For_debugging : sig
  val with_ui_client
    :  ?on_error:(Error.t -> unit)
    -> ?on_error_event:(Vcaml.Error_type.t -> message:string -> unit)
    -> ?time_source:Time_source.t
    -> socket:string
    -> (Vcaml.Client.t -> Test_ui.t -> 'a Deferred.Or_error.t)
    -> 'a Deferred.t
end
