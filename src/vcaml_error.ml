open Core
open Import0

module Notification = struct
  type t =
    { method_name : string
    ; params : Msgpack.t list
    }
  [@@deriving sexp_of]
end

module Nvim_error_event = struct
  module Error_type = Error_type

  type t =
    { error_type : Error_type.t
    ; message : string
    }
  [@@deriving sexp_of]

  let to_error t = Error.create_s [%sexp (t : t)]
end

type t =
  | Msgpack_rpc_error of Msgpack_rpc.Error.t
  | Nvim_error_event of Nvim_error_event.t
  | Nvim_error_event_parse_failure of Notification.t
  | Nvim_buffer_event_parse_failure of exn * Notification.t
  | Nvim_ui_event_parse_failure of exn * Notification.t
[@@deriving sexp_of]

let to_error t = Error.create_s [%sexp (t : t)]
