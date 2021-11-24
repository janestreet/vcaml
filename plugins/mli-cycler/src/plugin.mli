open! Core
open! Async

val main : Command.t

module For_testing : sig
  val next_file_pattern : Vcaml.Client.t -> unit Deferred.Or_error.t
  val prev_file_pattern : Vcaml.Client.t -> unit Deferred.Or_error.t
  val echo_file_patterns : Vcaml.Client.t -> unit Deferred.Or_error.t
  val list_file_patterns_in_fzf : Vcaml.Client.t -> unit Deferred.Or_error.t
end
