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

module Buffer = (val make_nvim_id ~name:"buffer" ~type_id:0)
module Window = (val make_nvim_id ~name:"window" ~type_id:1)
module Tabpage = (val make_nvim_id ~name:"tabpage" ~type_id:2)

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
    | Custom : (module Msgpack.Msgpackable with type t = 'a) -> 'a t

  let rec sexp_of_t : type a. (a -> Sexp.t) -> a t -> Sexp.t =
    fun _ t ->
    let ignore _ : Sexp.t = List [] in
    match t with
    | Nil -> Sexp.Atom "Nil"
    | Integer -> Atom "Integer"
    | Boolean -> Atom "Boolean"
    | Array arr -> List [ sexp_of_t ignore arr; Atom "ArrayN" ]
    | Tuple (arr, n) -> List [ sexp_of_t ignore arr; Atom (sprintf "Array%d" n) ]
    | Dict -> Atom "Dict"
    | String -> Atom "String"
    | Buffer -> Atom "Buffer"
    | Tabpage -> Atom "Tabpage"
    | Window -> Atom "Window"
    | Object -> Atom "Object"
    | Custom _ -> Atom "Custom"
  ;;
end

module Api_result = struct
  type 'result t =
    { name : string
    ; params : Msgpack.t
    ; witness : 'result Phantom.t
    }
  [@@deriving sexp_of]
end
