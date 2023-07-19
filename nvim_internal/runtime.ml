open Core

module type Nvim_id = sig
  type 'a phantom
  type t = private int [@@deriving sexp_of]

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t
  include Msgpack.Msgpackable with type t := t

  val t : t phantom

  module Or_current : sig
    type nvim_id := t

    type nonrec t =
      | Current
      | Id of t
    [@@deriving sexp_of]

    include Msgpack.Msgpackable with type t := t

    val to_id : t -> nvim_id option
    val t : t phantom
  end
end

let make_nvim_id ~type_id ~name =
  (module struct
    include Int

    (* This definition is overridden later with destructive substitution. *)
    type 'a phantom = 'a option

    (* This value is overridden later. *)
    let t = None

    let rec of_msgpack =
      let expected_type_id = type_id in
      function
      | Msgpack.Ext { type_id; data } when type_id = expected_type_id ->
        let open Or_error in
        Msgpack.t_of_string (Bytes.to_string data) >>= of_msgpack
      | Int i -> Ok i
      | Int64 i | Uint64 i ->
        (match Int64.to_int i with
         | Some i -> Ok i
         | None ->
           Or_error.error_s
             [%message (Printf.sprintf "too many %ss!" name) ~_:(i : Int64.t)])
      | msg ->
        Or_error.error_s
          [%message (Printf.sprintf "not a %s message!" name) (msg : Msgpack.t)]
    ;;

    let to_msgpack t = Msgpack.Int t

    module Or_current = struct
      type nonrec t =
        | Current
        | Id of t
      [@@deriving sexp_of]

      (* This value is overridden later. *)
      let t = None

      let to_id = function
        | Current -> None
        | Id t -> Some t
      ;;

      let of_msgpack msgpack =
        match%map.Or_error of_msgpack msgpack with
        | 0 -> Current
        | id -> Id id
      ;;

      let to_msgpack t =
        match t with
        | Current -> to_msgpack 0
        | Id id -> to_msgpack id
      ;;
    end
  end : Nvim_id)
;;
