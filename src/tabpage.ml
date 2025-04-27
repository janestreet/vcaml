open Core
open Async
open Import
include Nvim_internal.Tabpage

let list_wins ~(here : [%call_pos]) client t =
  Nvim_internal.nvim_tabpage_list_wins ~tabpage:t |> run ~here client
;;

let get_var ~(here : [%call_pos]) client t name ~type_ =
  Nvim_internal.nvim_tabpage_get_var ~tabpage:t ~name
  |> map_witness ~f:(Type.of_msgpack type_)
  |> run ~here client
;;

let set_var ~(here : [%call_pos]) client t name ~type_ ~value =
  let value = Type.to_msgpack type_ value in
  Nvim_internal.nvim_tabpage_set_var ~tabpage:t ~name ~value |> run ~here client
;;

let delete_var ~(here : [%call_pos]) client t name =
  Nvim_internal.nvim_tabpage_del_var ~tabpage:t ~name |> run ~here client
;;

let get_win ~(here : [%call_pos]) client t =
  Nvim_internal.nvim_tabpage_get_win ~tabpage:t |> run ~here client
;;

let get_number ~(here : [%call_pos]) client t =
  Nvim_internal.nvim_tabpage_get_number ~tabpage:t |> run ~here client
;;

let exists ~(here : [%call_pos]) client t =
  Nvim_internal.nvim_tabpage_is_valid ~tabpage:t |> run ~here client
;;

module Option = struct
  (*$ Vcaml_cinaps.generate_options_impl ~scope:Tabpage *)
  type 'a t = Cmdheight : int t [@@deriving sexp_of]

  let to_string (type a) : a t -> string = function
    | Cmdheight -> "cmdheight"
  ;;

  let[@warning "-33"] of_msgpack (type a) (t : a t) msgpack : a Or_error.t =
    let open Option_helpers in
    match t with
    | Cmdheight -> Type.of_msgpack Int msgpack
  ;;

  let[@warning "-33"] to_msgpack (type a) (t : a t) (value : a) =
    let open Option_helpers in
    match t with
    | Cmdheight -> Type.to_msgpack Int value
  ;;

  (*$*)

  let get (type a) ~(here : [%call_pos]) client (t : a t) : a Deferred.Or_error.t =
    Nvim_internal.nvim_get_option_value ~name:(to_string t) ~opts:String.Map.empty
    |> map_witness ~f:(of_msgpack t)
    |> run ~here client
  ;;

  let set (type a) ~(here : [%call_pos]) client (t : a t) (value : a)
    : unit Deferred.Or_error.t
    =
    Nvim_internal.nvim_set_option_value
      ~name:(to_string t)
      ~value:(to_msgpack t value)
      ~opts:String.Map.empty
    |> run ~here client
  ;;

  let get_dynamic_info (type a) ~(here : [%call_pos]) client (t : a t) =
    Nvim_internal.nvim_get_option_info ~name:(to_string t)
    |> map_witness
         ~f:(Dynamic_option_info.of_msgpack_map ~default_of_msgpack:(of_msgpack t))
    |> run ~here client
  ;;
end
