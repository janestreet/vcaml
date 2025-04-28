open Core

type t

(** This is a barebones implementation of Msgpack RPC for Neovim to facilitate debugging
    problems in the VCaml library by seeing if the problem is in VCaml or Neovim. *)

(** Open a connection to a running Neovim instance. You can find the name by checking
    [v:servername] or $NVIM. *)
val open_ : string -> t

(** Get the channel for this connection. Once you have this you can issue [rpcrequest] and
    [rpcnotify] calls from your running Neovim instance to OCaml. *)
val channel : t -> int

(** Send a synchronous request. *)
val request : t -> string -> Msgpack.t list -> unit

(** Send an asynchronous notification. *)
val notify : t -> string -> Msgpack.t list -> unit

(** Respond to an [rpcrequest]. The [msgid] must be the same as the one Neovim sent. *)
val respond : t -> msgid:int -> (Msgpack.t, Msgpack.t) Result.t -> unit

(** Receive a message from Neovim. *)
val receive : t -> [ `Connection_closed | `Waiting_for_neovim | `Message of Msgpack.t ]

(** Read all available messages from Neovim. Useful for cases where you expect many
    messages, e.g., for event subscriptions. *)
val receive_all_available : t -> Msgpack.t list

(** Close the connection. *)
val close : t -> unit

(** Enable / disable printing messages as they are sent and received. *)
val verbose : t -> bool -> unit

val pp : Formatter.t -> Msgpack.t -> unit
