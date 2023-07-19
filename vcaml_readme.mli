(** Helper library for README.mdx *)

type t

val setup : unit -> t
val socket : t -> string
val teardown : t -> unit
