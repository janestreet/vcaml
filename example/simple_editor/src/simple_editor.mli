open! Core
open Async
open Vcaml

module State : sig
  type t =
    { buffer : Buffer.t Set_once.t
    ; window : Window.t Set_once.t
    }
end

val main : Core.Command.t

module For_testing : sig
  module type S = Vcaml_plugin.Persistent.For_testing.S with type plugin_state := State.t

  val create_plugin : sequencer:unit Sequencer.t -> (module S)
end
