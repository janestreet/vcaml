open! Core
open Vcaml

module State : sig
  type t =
    { buffer : Buffer.t
    ; window : Window.t
    }
end

val main : Core.Command.t

module For_testing : sig
  val create_plugin
    :  sequencer:unit Async.Sequencer.t
    -> (module Vcaml_plugin.Persistent.S with type state = State.t)
end
