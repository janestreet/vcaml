open Async

module For_fuzz_testing : sig
  module Scenario : sig
    type t =
      | Log
      | Call of
          { await : bool
          ; spec : t list
          }
      | Parallel of t list
  end

  val invariant : verbose:bool -> Scenario.t -> unit Deferred.t
end
