open Base
module Message = Message

module Internal = struct
  module Parser = Parser
  module Serializer = Serializer
end

module Custom = struct
  type t = Message.custom =
    { type_id : int
    ; data : Bytes.t
    }
  [@@deriving compare, sexp]
end

module T = struct
  type t = Message.t =
    | Nil
    | Integer of int
    | Int64 of Int64.t
    | UInt64 of Int64.t
    | Boolean of bool
    | Floating of float
    | Array of t list
    | Map of (t * t) list
    | String of string
    | Binary of Bytes.t
    | Extension of Custom.t
  [@@deriving compare, sexp]
end

include T
include Comparable.Make (T)

let t_of_string = Parser.parse
let t_of_string_exn s = Or_error.ok_exn (Parser.parse s)
let string_of_t_exn = Serializer.message_to_string_exn
