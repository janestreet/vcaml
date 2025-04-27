open Core
open Async
open Import

exception Failed_to_parse of Error.t

module Blocking = struct
  type 'fn t =
    | Nullary : 'output Type.t -> 'output Deferred.Or_error.t t
    | Varargs :
        'leftmost Type.t * 'output Type.t
        -> ('leftmost list -> 'output Deferred.Or_error.t) t
    | Cons : 'a Type.t * 'b t -> ('a -> 'b) t

  let valid_number_of_args : 'fn t -> int -> bool =
    let rec f : type fn. fn t -> required:int -> int -> bool =
      fun t ~required ->
      match t with
      | Nullary _ -> ( = ) required
      | Varargs _ -> Int.( <= ) required
      | Cons (_, t) -> f t ~required:(required + 1)
    in
    f ~required:0
  ;;

  let rec apply_fn
    : type fn. fn t -> fn -> Msgpack.t list -> Msgpack.t Deferred.Or_error.t
    =
    fun t f args ->
    let open Deferred.Or_error.Let_syntax in
    match t, args with
    | Nullary return_type, [] ->
      let%map result = f in
      Type.to_msgpack return_type result
    | Varargs (expected_type, return_type), args ->
      (match
         List.mapi args ~f:(fun index arg ->
           Type.of_msgpack expected_type arg
           |> Or_error.tag_s ~tag:[%message (index : int)])
         |> Or_error.combine_errors
       with
       | Ok args ->
         let%map result = f args in
         Type.to_msgpack return_type result
       | Error error ->
         Exn.raise_without_backtrace
           (Failed_to_parse (Error.tag error ~tag:"Wrong argument type(s) in varargs")))
    | Cons (expected_type, t), arg :: args ->
      (match Type.of_msgpack expected_type arg with
       | Ok arg -> apply_fn t (f arg) args
       | Error error ->
         Exn.raise_without_backtrace
           (Failed_to_parse (Error.tag error ~tag:"Wrong argument type")))
    | _, _ ->
      (* This should be caught by the [valid_number_of_args] check. *)
      Exn.raise_without_backtrace
        (Failed_to_parse (Error.of_string "[BUG] Wrong number of arguments"))
  ;;

  let return typ = Nullary typ
  let ( @-> ) typ t = Cons (typ, t)

  module Expert = struct
    let varargs ~args_type ~return_type = Varargs (args_type, return_type)
  end
end

module Async = struct
  type 'fn t =
    | Unit : unit Deferred.Or_error.t t
    | Varargs : 'a Type.t -> ('a list -> unit Deferred.Or_error.t) t
    | Cons : 'a Type.t * 'b t -> ('a -> 'b) t

  let valid_number_of_args : 'fn t -> int -> bool =
    let rec f : type fn. fn t -> required:int -> int -> bool =
      fun t ~required ->
      match t with
      | Unit -> ( = ) required
      | Varargs _ -> Int.( <= ) required
      | Cons (_, t) -> f t ~required:(required + 1)
    in
    f ~required:0
  ;;

  let rec apply_fn : type fn. fn t -> fn -> Msgpack.t list -> unit Deferred.Or_error.t =
    fun t f args ->
    match t, args with
    | Unit, [] -> f
    | Varargs expected_type, args ->
      (match
         List.mapi args ~f:(fun index arg ->
           Type.of_msgpack expected_type arg
           |> Or_error.tag_s ~tag:[%message (index : int)])
         |> Or_error.combine_errors
       with
       | Ok args -> f args
       | Error error ->
         Exn.raise_without_backtrace
           (Failed_to_parse (Error.tag error ~tag:"Wrong argument type(s) in varargs")))
    | Cons (expected_type, t), arg :: args ->
      (match Type.of_msgpack expected_type arg with
       | Ok arg -> apply_fn t (f arg) args
       | Error error ->
         Exn.raise_without_backtrace
           (Failed_to_parse (Error.tag error ~tag:"Wrong argument type")))
    | _ ->
      (* This should be caught by the [valid_number_of_args] check. *)
      Exn.raise_without_backtrace
        (Failed_to_parse (Error.of_string "[BUG] Wrong number of arguments"))
  ;;

  let unit = Unit
  let ( @-> ) typ t = Cons (typ, t)

  module Expert = struct
    let varargs typ = Varargs typ
  end
end

let register_request_async_internal ~(here : [%call_pos]) client ~name ~type_ ~f ~wrap_f =
  let f client params =
    match Async.valid_number_of_args type_ (List.length params) with
    | false ->
      Deferred.Or_error.error_s
        [%message "Wrong number of arguments" ~method_name:name (params : Msgpack.t list)]
    | true ->
      wrap_f (fun x ->
        try Async.apply_fn type_ (f x ~client) params with
        | Failed_to_parse error ->
          error
          |> Error.tag_s
               ~tag:
                 [%message "Failed to parse" ~method_name:name (params : Msgpack.t list)]
          |> Deferred.Or_error.fail)
  in
  match (client : _ Client.Maybe_connected.t) with
  | Not_connected client ->
    Client.Private.Not_connected.register_request_async ~here client ~name ~f
  | Connected client ->
    let client = Type_equal.conv Client.Private.eq client in
    client.register_request_async ~here name ~f
;;

let register_request_blocking_internal
  ?(on_keyboard_interrupt = ignore)
  ~(here : [%call_pos])
  client
  ~name
  ~type_
  ~f
  ~wrap_f
  =
  let f ~run_in_background client params =
    match Blocking.valid_number_of_args type_ (List.length params) with
    | false ->
      Deferred.Or_error.error_s
        [%message "Wrong number of arguments" (params : Msgpack.t list)]
    | true ->
      wrap_f (fun x ->
        try Blocking.apply_fn type_ (f x ~run_in_background ~client) params with
        | Failed_to_parse error ->
          error
          |> Error.tag_s ~tag:[%message "Failed to parse" (params : Msgpack.t list)]
          |> Deferred.Or_error.fail)
  in
  match (client : _ Client.Maybe_connected.t) with
  | Not_connected client ->
    Client.Private.Not_connected.register_request_blocking
      ~here
      client
      ~name
      ~f
      ~on_keyboard_interrupt
  | Connected client ->
    let client = Type_equal.conv Client.Private.eq client in
    client.register_request_blocking ~here name ~f ~on_keyboard_interrupt
;;

let register_request_async ~(here : [%call_pos]) client ~name ~type_ ~f =
  register_request_async_internal
    ~here
    client
    ~name
    ~type_
    ~f:(fun () -> f)
    ~wrap_f:(fun f -> f ())
;;

let register_request_blocking
  ?on_keyboard_interrupt
  ~(here : [%call_pos])
  client
  ~name
  ~type_
  ~f
  =
  register_request_blocking_internal
    ?on_keyboard_interrupt
    ~here
    client
    ~name
    ~type_
    ~f:(fun () -> f)
    ~wrap_f:(fun f -> f ())
;;

let subscribe_to_broadcast ~(here : [%call_pos]) client ~name =
  Nvim_internal.nvim_subscribe ~event:name |> run ~here client
;;

let unsubscribe_from_broadcast ~(here : [%call_pos]) client ~name =
  Nvim_internal.nvim_unsubscribe ~event:name |> run ~here client
;;

module Callback = struct
  type 'a anon_rpc =
    { on_keyboard_interrupt : (unit -> unit) option
    ; f :
        run_in_background:
          (here:[%call_pos]
           -> ([ `asynchronous ] Client.t -> unit Deferred.Or_error.t)
           -> unit)
        -> client:[ `blocking ] Client.t
        -> 'a Deferred.Or_error.t
    }

  type 'a t =
    | Viml of string
    | Rpc of 'a anon_rpc

  let anon_rpc ?on_keyboard_interrupt f = Rpc { on_keyboard_interrupt; f }
end

module Private = struct
  let register_callback
    ~(here : [%call_pos])
    client
    ~return_type
    { Callback.on_keyboard_interrupt; f }
    =
    let name =
      (Type_equal.conv Client.Private.eq client).name_anonymous_blocking_request ()
    in
    register_request_blocking
      ?on_keyboard_interrupt
      ~here
      (Connected client)
      ~name
      ~type_:(Nullary return_type)
      ~f;
    name
  ;;

  let register_request_async = register_request_async_internal
  let register_request_blocking = register_request_blocking_internal
end
