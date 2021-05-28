open Core_kernel

module type Nvim_id = sig
  type t = private int [@@deriving sexp_of]

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t
  include Msgpack.Msgpackable with type t := t

  module Unsafe : sig
    val of_int : int -> t
  end
end

let make_nvim_id ~type_id ~name =
  (module struct
    include Int

    module Unsafe = struct
      let of_int = of_int
    end

    let rec of_msgpack =
      let expected_type_id = type_id in
      function
      | Msgpack.Extension { type_id; data } when type_id = expected_type_id ->
        let open Or_error in
        Msgpack.t_of_string (Bytes.to_string data) >>= of_msgpack
      | Integer i -> Ok i
      | Int64 i | UInt64 i ->
        (match Int64.to_int i with
         | Some i -> Ok i
         | None ->
           Or_error.error_s
             [%message (Printf.sprintf "too many %ss!" name) ~_:(i : Int64.t)])
      | msg ->
        Or_error.error_s
          [%message (Printf.sprintf "not a %s message!" name) (msg : Msgpack.t)]
    ;;

    let to_msgpack t = Msgpack.Integer t
  end : Nvim_id)
;;
