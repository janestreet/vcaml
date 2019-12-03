open Core
open Async

(** ['a Api_call.t] represents an un-sent API call that returns a value 
    of type ['a].  [run] and [run_join] are used to execute these api calls. *)

type 'a t

val map_bind : 'a Or_error.t t -> f:('a -> 'b Or_error.t) -> 'b Or_error.t t
val both : 'a t -> 'b t -> ('a * 'b) t
val return : 'a -> 'a t
val of_api_result : 'a Nvim_internal.Types.api_result -> 'a Or_error.t t
val run : Types.client -> 'a t -> 'a Or_error.t Deferred.t
val run_join : Types.client -> 'a Or_error.t t -> 'a Or_error.t Deferred.t

module Let_syntax : sig
  module Let_syntax : sig
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t
    val return : 'a -> 'a t
  end
end
