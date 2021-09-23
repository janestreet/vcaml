open Core
open Async

(** ['a Api_call.t] represents an un-sent API call that returns a value of type ['a].
    [run] and [run_join] are used to execute these api calls. *)

type 'a t

val map_bind : 'a Or_error.t t -> f:('a -> 'b Or_error.t) -> 'b Or_error.t t
val of_api_result : 'a Nvim_internal.Api_result.t -> 'a Or_error.t t
val run : Source_code_position.t -> Client.t -> 'a t -> 'a Deferred.Or_error.t

val run_join
  :  Source_code_position.t
  -> Client.t
  -> 'a Or_error.t t
  -> 'a Deferred.Or_error.t

include Applicative.S with type 'a t := 'a t
include Applicative.Let_syntax with type 'a t := 'a t

module Or_error : sig
  include Applicative.S with type 'a t = 'a Or_error.t t
  include Applicative.Let_syntax with type 'a t := 'a t

  val error_s : Sexp.t -> 'a t
  val ignore_m : 'a t -> unit t
end
