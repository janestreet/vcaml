open Core

(** An [Nvim_error_event] is an asynchronous notification that Neovim sends us of an
    error, typically in response to an asynchronous request we made. [Parse_failure] means
    we know this is an error event but failed to parse the rest of message. *)
module Nvim_error_event = struct
  module Error_type = Nvim_internal.Error_type

  type t =
    | Error of
        { error_type : Error_type.t
        ; message : string
        }
    | Parse_failure of Msgpack_rpc.Event.t
  [@@deriving sexp_of]

  let to_error t = Error.create_s [%sexp (t : t)]
end

type t =
  | Msgpack_rpc_error of Msgpack_rpc.Error.t
  | Nvim_error_event of Nvim_error_event.t
[@@deriving sexp_of]

let to_error t = Error.create_s [%sexp (t : t)]
