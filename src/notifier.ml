open Core
module Error_type = Nvim_internal.Error_type

type t =
  { client : Client.t
  ; on_error_event : Error_type.t -> message:string -> unit
  }

module Notification = struct
  type t = T : _ Nvim_internal.Api_result.t -> t [@@unboxed]

  module Defun = struct
    module Vim = struct
      type notification = t

      type ('fn, 'leftmost_input) t =
        | Unit : (notification, notification) t
        | Cons : 'a Nvim_internal.Phantom.t * ('b, _) t -> ('a -> 'b, 'a) t

      let unit = Unit
      let ( @-> ) a t = Cons (a, t)
    end
  end

  let custom ~type_ ~function_name =
    let rec custom
      : type fn i. (fn, i) Defun.Vim.t -> (Msgpack.t list -> Msgpack.t list) -> fn
      =
      fun arity f ->
        (* Due to the fact that OCaml does not (easily) support higher-ranked
           polymorphism, we need to construct the function [to_msgpack] *after* we unpack
           this GADT, so it can have the type [i -> Msgpack.t] (which is fixed by [arity]
           in this function). Otherwise, it needs the type [forall 'a . 'a witness -> 'a
           -> Msgpack.t], which is not that easily expressible. *)
        let T = Client.Private.eq in
        match arity with
        | Unit -> T (Nvim_internal.nvim_call_function ~fn:function_name ~args:(f []))
        | Cons (typ, rest) ->
          fun i ->
            let to_msgpack = Extract.inject typ in
            custom rest (fun args -> f (to_msgpack i :: args))
    in
    custom type_ Fn.id
  ;;

  module Untested = struct
    let nvim_buf_add_highlight ~buffer ~ns_id ~hl_group ~line ~col_start ~col_end =
      Nvim_internal.nvim_buf_add_highlight
        ~buffer
        ~ns_id
        ~hl_group
        ~line
        ~col_start
        ~col_end
      |> T
    ;;
  end
end

let create (client : Client.t) ~on_error_event =
  let T = Client.Private.eq in
  Bus.iter_exn client.events [%here] ~f:(fun ({ method_name; params } as event) ->
    match method_name with
    | "nvim_error_event" ->
      (match params with
       | [ Integer error_type; String message ] ->
         (match Error_type.of_int error_type with
          | Ok error_type -> on_error_event error_type ~message
          | Error error ->
            client.on_error
              (Error.create_s
                 [%message
                   "Unknown error event type"
                     (event : Msgpack_rpc.event)
                     (error : Error.t)]))
       | _ ->
         client.on_error
           (Error.create_s [%message "Argument mismatch" (event : Msgpack_rpc.event)]))
    | _ -> ());
  { client; on_error_event }
;;

let notify (t : t) (Notification.T notification) =
  let T = Client.Private.eq in
  t.client.call_nvim_api_fn notification Notification
;;

module For_testing = struct
  let send_raw (t : t) ~function_name:name ~params =
    let T = Client.Private.eq in
    let notification = { Nvim_internal.Api_result.name; params; witness = Nil } in
    t.client.call_nvim_api_fn notification Notification
  ;;
end
