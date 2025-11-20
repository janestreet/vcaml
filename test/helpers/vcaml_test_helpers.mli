open Core
open Async
open Vcaml

(*_ General note about testing Neovim: if you have a test that gets stuck on the hit-enter
    prompt (see `:h hit-enter`), send "<CR>" to Neovim via [Nvim.Fast.input]. *)

(** Launch an embedded Neovim instance and persist the connection for the duration of the
    callback. If [verbose] is set, a dump of the traffic between VCaml and Neovim will be
    printed, which is often helpful for debugging problems. You may see repeated API calls
    for [nvim_get_current_buf] and [nvim_eval "0"] - these are used by VCaml for flushing
    the Neovim event loop and heartbeating Neovim respectively. [verbose] works even with
    integration tests in which Neovim launches the plugin - both traffic between the test
    client and Neovim and traffic between Neovim and the plugin are displayed. *)
val with_client
  :  ?args:string list
  -> ?env:([> `Tmpdir of string ] -> Core_unix.env)
  -> ?links:(string * [ `In_path_as | `In_temp_as ] * string) list
  -> ?time_source:Time_source.t (** Informs when to send heartbeats. *)
  -> ?on_error:[ `Raise | `Call of Vcaml_error.t -> unit ]
  -> ?before_connecting:(Client.Not_connected.t -> unit Deferred.t)
  -> ?verbose:bool (** default: [false] *)
  -> ?warn_if_neovim_exits_early:bool (** default: [true] *)
  -> ([ `asynchronous ] Client.t -> 'a Deferred.Or_error.t)
  -> 'a Deferred.t

(** Shadow [print_s] to elide the test's temporary directory in the output. This is useful
    when the current working directory would appear in test output, e.g., in tests that
    display the name of the current buffer. *)
val print_s : ?mach:unit -> Sexp.t -> unit

module Test_ui : T

(** Same as [with_client], but also attaches a UI to receive UI events from Neovim. This
    lets you embed a visual representation of Neovim in your expect test. *)
val with_ui_client
  :  ?width:int
  -> ?height:int
  -> ?args:string list
  -> ?env:([> `Tmpdir of string ] -> Core_unix.env)
  -> ?links:(string * [ `In_path_as | `In_temp_as ] * string) list
  -> ?time_source:Time_source.t
  -> ?on_error:[ `Raise | `Call of Vcaml_error.t -> unit ]
  -> ?before_connecting:(Client.Not_connected.t -> unit Deferred.t)
  -> ?verbose:bool
  -> ?warn_if_neovim_exits_early:bool
  -> ([ `asynchronous ] Client.t -> Test_ui.t -> 'a Deferred.Or_error.t)
  -> 'a Deferred.t

(** Get the current screen contents. If you want to confirm that desired text has appeared
    on the screen before proceeding, use [wait_until_text] instead. *)
val get_screen_contents : Test_ui.t -> string Deferred.Or_error.t

(** Wait until text satisfying [f] is rendered in the Neovim UI, then return the screen
    contents. *)
val wait_until_text
  :  ?timeout:Time_ns.Span.t
  -> here:[%call_pos]
  -> Test_ui.t
  -> f:(string -> bool)
  -> string Deferred.Or_error.t

(** This function is for advanced tests that require a unix domain socket. Be sure to
    close the client when finished with the test. If you are reaching for this to visually
    debug a failing test, use [For_debugging.with_ui_client] instead. *)
val socket_client
  :  ?time_source:Time_source.t
  -> ?on_error:[ `Raise | `Call of Vcaml_error.t -> unit ]
  -> ?before_connecting:(Client.Not_connected.t -> unit Deferred.t)
  -> ?verbose:bool
  -> string
  -> [ `asynchronous ] Client.t Deferred.Or_error.t

(** If a test is behaving in a way that is especially surprising, use this module to
    attach it to an Neovim instance you are running so you can observe the effect. Note
    that the client detaching at the end of the test may be too rapid for you to view the
    effects - you may want to add a delay before returning. *)
module For_debugging : sig
  val with_ui_client
    :  ?time_source:Time_source.t
    -> ?on_error:[ `Raise | `Call of Vcaml_error.t -> unit ]
    -> ?before_connecting:(Client.Not_connected.t -> unit Deferred.t)
    -> ?verbose:bool
    -> socket:string
    -> ([ `asynchronous ] Client.t -> Test_ui.t -> 'a Deferred.Or_error.t)
    -> 'a Deferred.t
end

module Private : sig
  include module type of Private

  val neovim_path : string

  val attach_client
    :  ?stdio_override:Reader.t * Writer.t
    -> ?time_source:Time_source.t
    -> Client.Not_connected.t
    -> 'a Client.Connection_type.t
    -> 'a Deferred.Or_error.t
end
[@@alert vcaml_private "This module is for internal VCaml use."]
