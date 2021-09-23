open Core
open Async
open Import
open Nvim_internal

(* Api_call.t is an applicative which means that it can be defined either as
   the triple [map, both, return] or via [map, apply, return].  Because
   [Api_call.t] never contain functions in practice (though it is possible via
   [return]), it is easier to think about as an implementation based off both.
   We define the operation [both : 'a api_call -> 'b api_call -> ('a * 'b)
   api_call], which atomically receives its values from neovim before combining
   them. *)

type _ t =
  | Single : 'a Api_result.t -> 'a Or_error.t t
  | Map : ('a -> 'b) * 'a t -> 'b t
  | Map_bind : ('a -> 'b Or_error.t) * 'a Or_error.t t -> 'b Or_error.t t
  | Pair : 'a t * 'b t -> ('a * 'b) t
  | Const : 'a -> 'a t

let of_api_result x = Single x

let call_atomic (client : Client.t) ~calls =
  let T = Client.Private.eq in
  let api_call = Nvim_internal.nvim_call_atomic ~calls in
  client.call_nvim_api_fn api_call Request
;;

let rec collect_calls : type a. a t -> Msgpack.t list = function
  | Single { name; params; _ } -> [ Msgpack.Array [ String name; Array params ] ]
  | Map (_, c) -> collect_calls c
  | Map_bind (_, c) -> collect_calls c
  | Pair (c1, c2) -> collect_calls c1 @ collect_calls c2
  | Const _ -> []
;;

let rec extract_results : type a. Msgpack.t list -> a t -> a * Msgpack.t list =
  fun l shape ->
  match l, shape with
  | l, Const x -> x, l
  | [], (Single _ | Map _ | Pair _) ->
    failwith "got bad response from vim: wrong number of responses"
  | obj :: rest, Single { witness; _ } -> Extract.value witness obj, rest
  | l, Map (f, c) ->
    let obj, rest = extract_results l c in
    f obj, rest
  | l, Map_bind (f, c) ->
    let obj, rest = extract_results l c in
    Or_error.bind ~f obj, rest
  | l, Pair (a, b) ->
    let left, remaining = extract_results l a in
    let right, rest = extract_results remaining b in
    (left, right), rest
;;

let rec run : type a. Client.t -> a t -> a Deferred.Or_error.t =
  fun client res ->
  let T = Client.Private.eq in
  match res with
  | Const x -> return (Ok x)
  | Single api -> client.call_nvim_api_fn api Request |> Deferred.ok
  | Map (f, c) -> run client c |> Deferred.Or_error.map ~f
  | Map_bind (f, c) ->
    let%map result = run client c in
    Or_error.map ~f:(Or_error.bind ~f) result
  | Pair _ ->
    let calls = collect_calls res in
    (match%bind call_atomic client ~calls with
     | Error _ as e -> return e
     | Ok [ Msgpack.Array results; Nil ] ->
       let r = Or_error.try_with (fun () -> extract_results results res) in
       return (Or_error.map ~f:Tuple2.get1 r)
     | Ok [ Msgpack.Array _; Array [ Integer index; Integer error_type; String msg ] ] ->
       Extract.convert_msgpack_error
         (Error (Array [ Integer error_type; String msg ]))
         ~on_keyboard_interrupt:(Bvar.broadcast client.keyboard_interrupts)
       |> Result.map_error ~f:(fun error ->
         Error.create_s [%message "" ~_:(error : Error.t) (index : int)])
       |> return
     | _ -> Deferred.Or_error.error_string "got bad response from vim: bad format")
;;

let run_join here client t = run client t >>| Or_error.join >>| tag_callsite here
let run here client t = run client t >>| tag_callsite here
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
  type nonrec 'a t = 'a Or_error.t t

  module Open_on_rhs_intf = Open_on_rhs_intf

  module T = Applicative.Make_using_map2 (struct
      type nonrec 'a t = 'a t

      let map x ~f = map x ~f:(Or_error.map ~f)
      let map2 a b ~f = map2 a b ~f:(Or_error.map2 ~f)
      let return x = Const (Or_error.return x)
      let map = `Custom map
    end)

  include T

  include
    Applicative.Make_let_syntax
      (struct
        type nonrec 'a t = 'a t

        include T
      end)
      (Open_on_rhs_intf)
      ()

  let error_s sexp = Const (Or_error.error_s sexp)
  let ignore_m t = map t ~f:ignore
end
