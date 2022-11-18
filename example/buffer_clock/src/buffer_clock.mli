open Core
open Async
open Vcaml

module State : sig
  type t =
    { window : Window.t Set_once.t
    ; buffer : Buffer.t Set_once.t
    }
end

val main : Core.Command.t

module For_testing : sig
  module type S = Vcaml_plugin.Persistent.For_testing.S with type plugin_state := State.t

  val create_plugin : time_source:Time_source.t -> (module S)
end
