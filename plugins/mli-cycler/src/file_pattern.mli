open! Core
open! Async

(** A File_pattern represents a file whose name has been parsed into the structured data
    useful for cycling. For instance "my_foo.mli" gets represented as `Mli "my_foo"`. *)

type t [@@deriving compare]

val of_filename : Filename.t -> t option
val to_filename : t -> Filename.t
val dirname : t -> Filename.t
val to_short_filename : t -> string
val to_intf_name : t -> string

(** High-level functions that deal with cycling logic *)

val list : Filename.t -> t list Deferred.t
val next : current_file_pattern:t option -> file_patterns:t list -> t option
val prev : current_file_pattern:t option -> file_patterns:t list -> t option
