open! Base

module type Nvim_ext = sig
  type t [@@deriving compare, hash, sexp]

  val equal : t -> t -> bool
  val of_msgpack : Msgpack.t -> t Or_error.t
  val to_msgpack : t -> Msgpack.t
end

module Buffer = struct
  type t = int [@@deriving equal, hash, compare, sexp]

  let of_msgpack = function
    | Msgpack.Integer i -> Ok i
    | Int64 _ | UInt64 _ -> Or_error.error_string "too many buffers!"
    | _ -> Or_error.error_string "Expected int when deserializing buffer"
  ;;

  let to_msgpack i = Msgpack.Integer i
end

module Window = struct
  type t = int [@@deriving equal, hash, compare, sexp]

  let of_msgpack = function
    | Msgpack.Integer i -> Ok i
    | Int64 _ | UInt64 _ -> Or_error.error_string "too many windows!"
    | _ -> Or_error.error_string "Expected int when deserializing window"
  ;;

  let to_msgpack i = Msgpack.Integer i
end

module Tabpage = struct
  type t = Int64.t [@@deriving equal, hash, compare, sexp]

  let of_msgpack = function
    | Msgpack.Integer i -> Ok (Int64.of_int i)
    | Int64 i | UInt64 i -> Ok i
    | _ -> Or_error.error_string "Expected int when deserializing tabpage"
  ;;

  let to_msgpack i = Msgpack.Int64 i
end

module Phantom = struct
  type _ t =
    | Nil : unit t
    | Integer : int t
    | Boolean : bool t
    | Array : 'a t -> 'a list t
    | Tuple : 'a t * int -> 'a list t
    | Dict : (Msgpack.t * Msgpack.t) list t
    | String : string t
    | Buffer : Buffer.t t
    | Tabpage : Tabpage.t t
    | Window : Window.t t
    | Object : Msgpack.t t
    | Custom :
        { of_msgpack : Msgpack.t -> 'a Or_error.t
        ; to_msgpack : 'a -> Msgpack.t
        }
        -> 'a t
end

type 'result api_result =
  { name : string
  ; params : Msgpack.t
  ; witness : 'result Phantom.t
  }
