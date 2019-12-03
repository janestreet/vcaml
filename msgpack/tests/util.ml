open Core
open Quickcheck
open Msgpack

(* This function is intended to generate ``well-behaved'' objects, ie those satisfying
   [parse . serialize == id]. Because we need to be able to handle 64-bit integers, any
   [Integer] variant containing a value greater than [0xFFFF_FFFF] get deserialized
   into [Int64] or [UInt64] variants instead.
*)
let generate () =
  let open Generator in
  let open Generator.Let_syntax in
  let int_gen =
    let open Int.O in
    (* We should properly test all integers from -2**N to 2**N-1, where N is [system bitsize]
       minus 2, but it's difficult to do so without conditional compilation. It also isn't
       immediately obvious why this is the number of bits to choose, and so we instead choose
       16 bits, which falls well within the allowable range for both 32- and 64-bit systems.
    *)
    let min_tested_int = 0 - (2 ** 16) in
    let max_tested_int = (2 ** 16) - 1 in
    Int.gen_uniform_incl min_tested_int max_tested_int
  in
  let int64_gen = Int64.gen_uniform_incl Int64.min_value Int64.max_value in
  let int64_pos_gen = Int64.gen_uniform_incl Int64.min_value Int64.minus_one in
  recursive_union
    [ return Nil
    ; (let%map i = int_gen in
       Integer i)
    ; (let%map i = int64_gen in
       UInt64 i)
    ; (let%map i = int64_pos_gen in
       Int64 i)
    ; (let%map b = bool in
       Boolean b)
    ; (let%map f = Float.gen_infinite in
       Floating f)
    ; (let%map s = String.gen' char_print in
       String s)
    ; (let%map b = Bytes.gen' char in
       Binary b)
    ; (let%bind type_id = Int.gen_uniform_incl (-128) 127 in
       let%map data = Bytes.gen' char in
       Extension { type_id; data })
    ]
    ~f:(fun self ->
      [ (let%map vs = list self in
         Array vs)
      ; (let%bind ks = list self in
         let ks_length = List.length ks in
         let%map vs = list_with_length ks_length self in
         Map (List.zip_exn ks vs))
      ])
;;
