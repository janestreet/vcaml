open! Core
open! Async

module Man_in_the_middle_debugger :
  Man_in_the_middle_debugger.S with type message := Msgpack.t

(** Given the names of two Msgpack peers and a channel to which traffic will be logged,
    return two functions that log [Msgpack.t]s sent between [peer1] and [peer2]. The
    messages have "[peer1] -> [peer2]" or "[peer2] -> [peer1]" headings to indicate the
    message direction. Useful for debugging communication between two peers. *)
val create_debug_printers
  :  ?pp:(Format.formatter -> Msgpack.t -> unit) (** default: [Msgpack.pp ?pp_ext:None] *)
  -> ?color:bool (** default: [false] *)
  -> peer1:string
  -> peer2:string
  -> Out_channel.t
  -> [ `Peer_1_to_2 of Msgpack.t -> unit ] * [ `Peer_2_to_1 of Msgpack.t -> unit ]

val command
  :  ?pp:(Format.formatter -> Msgpack.t -> unit) (** default: [Msgpack.pp ?pp_ext:None] *)
  -> unit
  -> Command.t
