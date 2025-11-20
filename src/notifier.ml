open Core
open Async
open Import

module Func = struct
  type 'fn t =
    | Unit : unit Deferred.Or_error.t t
    | Cons : 'a Type.t * 'b t -> ('a -> 'b) t

  let unit = Unit
  let ( @-> ) typ t = Cons (typ, t)

  let rec apply_fn : type fn. fn t -> (Msgpack.t list -> unit Deferred.Or_error.t) -> fn =
    fun t f ->
    (* Due to the fact that OCaml does not (easily) support higher-ranked polymorphism, we
       need to construct the function [to_msgpack] *after* we unpack this GADT, so it can
       have the type [i -> Msgpack.t] (which is fixed by [t] in this function). Otherwise,
       it needs the type [forall 'a . 'a witness -> 'a -> Msgpack.t], which is not that
       easily expressible. *)
    match t with
    | Unit -> f []
    | Cons (typ, rest) ->
      fun arg -> apply_fn rest (fun args -> f (Type.to_msgpack typ arg :: args))
  ;;
end

(* Changes here should probably have analogous changes in [Nvim.call_function]. *)
let notify ~(here : [%call_pos]) client ~name ~type_ =
  let client = Type_equal.conv Client.Private.eq client in
  Func.apply_fn type_ (fun args ->
    (match name with
     | `Viml name -> Nvim_internal.nvim_call_function ~fn:name ~args
     | `Lua name ->
       Nvim_internal.nvim_exec_lua
       (* We surround [name] with parentheses to support anonymous functions. We assign it
          to [result] before returning it to ensure [name] appears in the stack trace if
          it raises an error. *)
         ~code:[%string {| local result = (%{name})(...); return result |}]
         ~args)
    |> client.call_nvim_api_fn ~here Notification)
;;

module Untested = struct
  let nvim_buf_add_highlight
    ~(here : [%call_pos])
    client
    buffer
    ~namespace
    ~hl_group
    ~line
    ~col_start
    ~col_end
    =
    let client = Type_equal.conv Client.Private.eq client in
    Nvim_internal.nvim_buf_add_highlight
      ~buffer
      ~ns_id:(Namespace.id namespace)
      ~hl_group
      ~line
      ~col_start
      ~col_end
    |> client.call_nvim_api_fn ~here Notification
  ;;
end
