open! Core
open! Async

val main : Command.t

module For_testing : sig
  type command := [ `connected ] Vcaml.Client.t -> unit Deferred.Or_error.t

  val next_file_pattern : command
  val prev_file_pattern : command
  val echo_file_patterns : command
  val list_file_patterns_in_fzf : command
end
