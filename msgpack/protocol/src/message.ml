open Core

module Custom = struct
  type t =
    { type_id : int
    ; data : Bytes.t
    }
  [@@deriving compare, sexp]
end

module T = struct
  type t =
    | Nil
    | Int of int
    | Int64 of Int64.t
    | Uint64 of Int64.t
    | Bool of bool
    | Float of float
    | Array of t list
    (* The specification doesn't say what to do in the case of duplicate keys. Also, these
       objects are currently mutable, so [Map.t] might not be the best idea. *)
    | Map of (t * t) list
    | String of string
    | Binary of Bytes.t
    | Ext of Custom.t
  [@@deriving compare, sexp]
end

include T
include Comparable.Make (T)

(* This generator is intended to generate ``well-behaved'' objects, ie those satisfying
   [parse . serialize == id]. Because we need to be able to handle 64-bit integers, any
   [Int] variant containing a value greater than [0xFFFF_FFFF] get deserialized into
   [Int64] or [Uint64] variants instead. *)
let quickcheck_generator ~only_string_keys ~only_finite_floats =
  let open Quickcheck in
  let open Generator in
  let open Generator.Let_syntax in
  let int_gen =
    let open Int.O in
    (* We should properly test all integers from -2**N to 2**N-1, where N is
       [system bitsize] minus 2, but it's difficult to do so without conditional
       compilation. It also isn't immediately obvious why this is the number of bits to
       choose, and so we instead choose 16 bits, which falls well within the allowable
       range for both 32- and 64-bit systems. *)
    let min_tested_int = 0 - (2 ** 16) in
    let max_tested_int = (2 ** 16) - 1 in
    Int.gen_uniform_incl min_tested_int max_tested_int
  in
  let int64_gen = Int64.gen_uniform_incl Int64.min_value Int64.max_value in
  let int64_pos_gen = Int64.gen_uniform_incl Int64.min_value Int64.minus_one in
  let float =
    if only_finite_floats then Float.gen_finite else Float.quickcheck_generator
  in
  recursive_union
    [ return Nil
    ; (let%map i = int_gen in
       Int i)
    ; (let%map i = int64_gen in
       Uint64 i)
    ; (let%map i = int64_pos_gen in
       Int64 i)
    ; (let%map b = bool in
       Bool b)
    ; (let%map f = float in
       Float f)
    ; (let%map s = String.gen' char_print in
       String s)
    ; (let%map b = Bytes.gen' char in
       Binary b)
    ; (let%bind type_id = Int.gen_uniform_incl (-128) 127 in
       let%map data = Bytes.gen' char in
       Ext { type_id; data })
    ]
    ~f:(fun self ->
      [ (let%map vs = list self in
         Array vs)
      ; (let%bind ks =
           list
             (match only_string_keys with
              | false -> self
              | true ->
                let%map s = String.gen' char_print in
                String s)
         in
         let ks_length = List.length ks in
         let%map vs = list_with_length ks_length self in
         Map (List.zip_exn ks vs))
      ])
;;
