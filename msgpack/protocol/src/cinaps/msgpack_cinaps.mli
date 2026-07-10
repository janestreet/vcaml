(** Cinaps helpers for the msgpack protocol parser. *)

(** Prints a [match c with | ... -> ...] expression that dispatches on the tag byte [c] to
    the correct msgpack sub-parser.

    The hex patterns are derived from [Msgpack.Internal.Constants], so those constants in
    the match arm are kept in sync with the constants in the serializer. *)
val print_parser_match : unit -> unit
