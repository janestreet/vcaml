open! Core
open Vcaml

module State : sig
  type t =
    { buffer : Buffer.t Set_once.t
    ; window : Window.t Set_once.t
    }
end

include Vcaml_plugin.Persistent.S with type state := State.t
