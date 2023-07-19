open! Core
open Async

(** This module is internal to VCaml. It is used by [Client.t] to track permission to
    communicate with Neovim. On each blocking RPC request we receive, we call [take] and
    then create a new [Client.t] with a [call_nvim_api_fn] that closes over the new
    [Permission_to_run.t]. In the event of nested blocking RPC requests, the most recent
    has exclusive permission to run until it returns, then the previous, etc. *)

type t

module Permission_to_run : sig
  (** Permission to communicate with Neovim. Implemented as an [Mvar.t] but only "view"
      permissions are exposed (no putting or taking). *)
  type t

  val value_available : t -> unit Deferred.t
  val taken : t -> unit Deferred.t
  val peek : t -> [ `Ok | `Expired ] option
end

val create : unit -> t

(** [t] manages a stack of [Permission_to_run.t]s. The node at the top of the stack has
    permission and the other nodes await permission or are expired. Once the top node's
    permission is expired, expired nodes are popped from the stack until the first
    non-expired node is encountered, which will then receive permission. *)
val take : t -> Permission_to_run.t

(** Expire this [Permission_to_run.t]. If it is the top of the stack (see [take] above),
    which means it actively held permission, permission will be returned to the first
    non-expired [Permission_to_run.t] in the stack. *)
val expire : t -> Permission_to_run.t -> unit

(** Swap out this [Permission_to_run.t] for a fresh one. The old [Permission_to_run.t] is
    expired and the new one will have the old one's state and will take its place in [t].

    This allows us to expire the client given to the callback as soon as the callback
    returns, but only allow the permission we took for this RPC to be reclaimable after we
    actually send the response to Neovim. *)
val expire_other_users : t -> Permission_to_run.t -> Permission_to_run.t
