open! Core
open Vcaml

module State : sig
  type t =
    { buffer : Buf.t
    ; window : Window.t
    }
end

val main : Command.t

module For_testing : sig
  val create_plugin
    :  sequencer:unit Async.Sequencer.t
    -> (module Vcaml_plugin.S with type state = State.t)
end
