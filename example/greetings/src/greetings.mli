open! Core
open! Async

val main : Core.Command.t

module For_testing : Vcaml_plugin.Persistent.For_testing.S with type plugin_state := unit
