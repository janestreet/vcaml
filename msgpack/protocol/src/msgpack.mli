open Core

(** Parser and Serializer for Msgpack data.
    https://github.com/msgpack/msgpack/blob/master/spec.md *)

include module type of Message

module Internal : sig
  module Parser : module type of Parser
  module Serializer : module type of Serializer
end

module type Msgpackable = sig
  type t

  val of_msgpack : Message.t -> t Or_error.t
  val to_msgpack : t -> Message.t
end

val t_of_string : string -> t Or_error.t
val t_of_string_exn : string -> t
val string_of_t_exn : ?bufsize:int -> t -> string

(** [pp_ext] can be used to customize pretty-printing of extensions for your application.
    If omitted, a default printer will be used that shows the type id and raw bytes. *)
val pp : ?pp_ext:(Formatter.t -> Custom.t -> unit) -> Formatter.t -> t -> unit
