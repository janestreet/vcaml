open Core
open Async

(* Api_call.t is an applicative which means that it can be defined either as
   the tripple [map, both, return] or via [map, apply, return].  Because
   [Api_call.t] never contain functions in practice (though it is possible via
   [return]), it is easier to think about as an implementation based off both.
   We define the operation [both : 'a api_call -> 'b api_call -> ('a * 'b)
   api_call], which atomically receives its values from neovim before combining
   them. *)

type _ t =
  | Single : 'a Nvim_internal.Types.api_result -> 'a Or_error.t t
  | Map : ('a -> 'b) * 'a t -> 'b t
  | Map_bind : ('a -> 'b Or_error.t) * 'a Or_error.t t -> 'b Or_error.t t
  | Pair : 'a t * 'b t -> ('a * 'b) t
  | Const : 'a -> 'a t

let of_api_result x = Single x

let call_atomic { Types.call_nvim_api_fn; _ } ~calls =
  Nvim_internal.Wrappers.nvim_call_atomic ~calls |> call_nvim_api_fn
;;

let rec collect_calls : type a. a t -> Msgpack.t list =
  let open Nvim_internal.Types in
  function
  | Single { name; params; _ } -> [ Msgpack.Array [ String name; params ] ]
  | Map (_, c) -> collect_calls c
  | Map_bind (_, c) -> collect_calls c
  | Pair (c1, c2) -> collect_calls c1 @ collect_calls c2
  | Const _ -> []
;;

let rec extract_results
  : type a. Msgpack.t list -> a t -> Types.client -> a * Msgpack.t list
  =
  fun l shape cli ->
  let open Nvim_internal.Types in
  match l, shape with
  | l, Const x -> x, l
  | [], Single _ ->
    Or_error.error_string "got bad response from vim: wrong number of responses", []
  | [], Map _ -> failwith "got bad response from vim: wrong number of responses"
  | [], Pair _ -> failwith "got bad response from vim: wrong number of responses"
  | obj :: rest, Single { witness; _ } -> Extract.value witness obj, rest
  | l, Map (f, c) ->
    let obj, rest = extract_results l c cli in
    f obj, rest
  | l, Map_bind (f, c) ->
    let obj, rest = extract_results l c cli in
    Or_error.bind ~f obj, rest
  | l, Pair (a, b) ->
    let left, remaining = extract_results l a cli in
    let right, rest = extract_results remaining b cli in
    (left, right), rest
;;

let rec run : type a. Types.client -> a t -> a Or_error.t Deferred.t =
  fun ({ Types.call_nvim_api_fn; _ } as client) res ->
  match res with
  | Const x -> return (Ok x)
  | Single api ->
    let%map result = call_nvim_api_fn api in
    Ok result
  | Map (f, c) -> run client c |> Deferred.Or_error.map ~f
  | Map_bind (f, c) ->
    let%map result = run client c in
    Or_error.map ~f:(Or_error.bind ~f) result
  | Pair _ ->
    let calls = collect_calls res in
    (match%map call_atomic client ~calls with
     | Error _ as e -> e
     | Ok [ Msgpack.Array results; _err ] ->
       let r = Or_error.try_with (fun () -> extract_results results res client) in
       Or_error.map ~f:Tuple2.get1 r
     | _ -> Or_error.error_string "got bad response from vim: bad format")
;;

let run_join cli t = run cli t >>| Or_error.join
let map_bind x ~f = Map_bind (f, x)
let both x y = Pair (x, y)

module T = Applicative.Make_using_map2 (struct
    type nonrec 'a t = 'a t

    let map x ~f = Map (f, x)
    let map2 a b ~f = both a b |> map ~f:(fun (a, b) -> f a b)
    let return x = Const x
    let map = `Custom map
  end)

include T

module Open_on_rhs_intf = struct
  module type S = sig end
end

include
  Applicative.Make_let_syntax
    (struct
      type nonrec 'a t = 'a t

      include T
    end)
    (Open_on_rhs_intf)
    ()

module Or_error = struct
  module Open_on_rhs_intf = Open_on_rhs_intf

  module T = Applicative.Make_using_map2 (struct
      type nonrec 'a t = 'a Or_error.t t

      let map x ~f = map x ~f:(Or_error.map ~f)
      let map2 a b ~f = map2 a b ~f:(Or_error.map2 ~f)
      let return x = Const (Or_error.return x)
      let map = `Custom map
    end)

  include T

  include
    Applicative.Make_let_syntax
      (struct
        type nonrec 'a t = 'a Or_error.t t

        include T
      end)
      (Open_on_rhs_intf)
      ()
end
