open! Core
open! Async

val main : Command.t

module Next_file_pattern : Vcaml_plugin.Oneshot.S
module Prev_file_pattern : Vcaml_plugin.Oneshot.S
module Echo_file_patterns : Vcaml_plugin.Oneshot.S
module List_file_patterns_in_fzf : Vcaml_plugin.Oneshot.S
