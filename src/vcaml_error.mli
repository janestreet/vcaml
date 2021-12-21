open Core

module Nvim_error_event : sig
  module Error_type = Nvim_internal.Error_type

  type t =
    | Error of
        { error_type : Error_type.t
        ; message : string
        }
    | Parse_failure of Msgpack_rpc.Event.t
  [@@deriving sexp_of]

  val to_error : t -> Error.t
end

type t =
  | Msgpack_rpc_error of Msgpack_rpc.Error.t
  | Nvim_error_event of Nvim_error_event.t
[@@deriving sexp_of]

val to_error : t -> Error.t
