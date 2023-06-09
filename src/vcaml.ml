module Unshadow = struct
  module Buffer = Buffer
  module Command = Command
end

open Core
open Async
module Api_call = Api_call
module Api_version = Nvim_internal.Api_version
module Buffer = Unshadow.Buffer
module Channel_info = Channel_info
module Client_info = Client_info
module Color = Color
module Command = Unshadow.Command
module Error_type = Nvim_internal.Error_type
module Highlighted_text = Highlighted_text
module Keymap = Keymap
module Mark = Mark
module Mode = Mode
module Namespace = Namespace
module Nvim = Nvim
module Position = Position
module Tabpage = Tabpage
module Type = Nvim_internal.Phantom
module Ui = Ui
module Vcaml_error = Vcaml_error
module Window = Window

let api_version = Nvim_internal.api_version
let pp = Nvim_internal.pp

module Nvim_version = struct
  include Semantic_version.Make ()

  let vcaml = of_string "0.7.0"
end

module Client = struct
  include Client

  module Connection_type = struct
    type 'state client = 'state t

    type _ t =
      | Unix : [ `Child | `Socket of string ] -> [ `connected ] client t
      | Stdio : [ `connected ] client t
      | Embed :
          { prog : string
          ; args : string list
          ; working_dir : string
          ; env : Core_unix.env
          }
          -> ([ `connected ] client * Async.Process.t) t
    [@@deriving sexp_of]
  end

  let create ~on_error =
    let on_error =
      match on_error with
      | `Call f -> f
      | `Raise -> Fn.compose Error.raise Vcaml_error.to_error
    in
    create ~on_error
  ;;

  let attach
        (type a)
        ?(close_reader_and_writer_on_disconnect = true)
        client
        (connection_type : a Connection_type.t)
        ~time_source
    : a Deferred.Or_error.t
    =
    let connect = connect client ~close_reader_and_writer_on_disconnect ~time_source in
    match connection_type with
    | Unix socket ->
      let socket =
        match socket with
        | `Socket socket -> socket
        | `Child ->
          Sys.getenv_exn "NVIM_LISTEN_ADDRESS"
      in
      let socket = Tcp.Where_to_connect.of_file socket in
      let%bind _addr, reader, writer = Tcp.connect socket in
      connect reader writer
    | Stdio -> connect (force Reader.stdin) (force Writer.stdout)
    | Embed { prog; args; working_dir; env } ->
      (match List.exists args ~f:(String.equal "--embed") with
       | false ->
         Deferred.Or_error.error_s
           [%message
             "Tried to create a VCaml client for an embedded Neovim process, but --embed \
              flag was not passed"
               ~_:(connection_type : _ Connection_type.t)]
       | true ->
         let open Deferred.Or_error.Let_syntax in
         let%bind nvim = Process.create ~prog ~args ~working_dir ~env () in
         let%bind client = connect (Process.stdout nvim) (Process.stdin nvim) in
         return (client, nvim))
  ;;

  let close client =
    let client = Type_equal.conv Private.eq client in
    let (Connected state) = client.state in
    state.close ()
  ;;

  let channel client =
    let client = Type_equal.conv Private.eq client in
    let (Connected state) = client.state in
    Set_once.get_exn state.channel [%here]
  ;;
end

let run = Api_call.run
let run_join = Api_call.run_join

module Defun = struct
  module Vim = struct
    type ('f, 'leftmost_input, 'out) t =
      | Nullary : 'output Type.t -> ('output Api_call.Or_error.t, unit, 'output) t
      | Cons : 'a Type.t * ('b, _, 'output) t -> ('a -> 'b, 'a, 'output) t

    let return t = Nullary t
    let ( @-> ) a t = Cons (a, t)

    let rec make_fn
      : type fn i out.
        string -> (fn, i, out) t -> (Msgpack.t list -> Msgpack.t list) -> fn
      =
      fun function_name arity f ->
      (* Due to the fact that OCaml does not (easily) support higher-ranked polymorphism,
         we need to construct the function [to_msgpack] *after* we unpack this GADT, so it
         can have the type [i -> Msgpack.t] (which is fixed by [arity] in this function).
         Otherwise, it needs the type [forall 'a . 'a witness -> 'a -> Msgpack.t], which
         is not that easily expressible. *)
      match arity with
      | Nullary return_type ->
        let args = f [] in
        let open Api_call.Let_syntax in
        let%map result =
          Nvim_internal.nvim_call_function ~fn:function_name ~args
          |> Api_call.of_api_result
        in
        let open Or_error.Let_syntax in
        let%bind result = result in
        Extract.value return_type result
        |> Or_error.tag ~tag:"return type given to [wrap_viml_function] is incorrect"
      | Cons (t, rest) ->
        fun i ->
          let to_msgpack = Extract.inject t in
          make_fn function_name rest (fun args -> f (to_msgpack i :: args))
    ;;
  end

  module Ocaml = struct
    module Sync = struct
      type ('f, 'leftmost_input) t =
        | Nullary :
            ('output Type.t
             * ('output_deferred_or_error, 'output Deferred.Or_error.t) Type_equal.t)
            -> ('output_deferred_or_error, unit) t
        | Varargs :
            ('leftmost Type.t
             * 'output Type.t
             * ('output_deferred_or_error, 'output Deferred.Or_error.t) Type_equal.t)
            -> ('leftmost list -> 'output_deferred_or_error, 'leftmost list) t
        | Cons : 'a Type.t * ('b, _) t -> ('a -> 'b, 'a) t

      let valid_number_of_args : ('fn, 'i) t -> int -> bool =
        let rec f : type fn i. (fn, i) t -> required:int -> int -> bool =
          fun t ~required ->
          match t with
          | Nullary _ -> ( = ) required
          | Varargs _ -> Int.( <= ) required
          | Cons (_, t) -> f t ~required:(required + 1)
        in
        f ~required:0
      ;;

      let rec make_fn
        : type fn i. (fn, i) t -> fn -> Msgpack.t list -> Msgpack.t Deferred.Or_error.t
        =
        fun arity f l ->
        let open Deferred.Or_error.Let_syntax in
        match arity, l with
        | Nullary (return_type, T), [] ->
          let%map v = f in
          Extract.inject return_type v
        | Varargs (leftmost, output, T), l ->
          (match List.map l ~f:(Extract.value leftmost) |> Or_error.combine_errors with
           | Error error ->
             Deferred.Or_error.error_s
               [%message
                 "Wrong argument type"
                   ~expected_type:(leftmost : _ Type.t)
                   (error : Error.t)]
           | Ok l ->
             let%map v = f l in
             Extract.inject output v)
        | Cons (leftmost, rest), x :: xs ->
          let%bind v = Extract.value leftmost x |> Deferred.return in
          make_fn rest (f v) xs
        | _, _ ->
          (* This should be caught by the [valid_number_of_args] check. *)
          Deferred.Or_error.error_s [%message "[BUG] Wrong number of arguments"]
      ;;

      let return t = Nullary (t, T)
      let ( @-> ) a b = Cons (a, b)

      module Expert = struct
        let varargs ~args_type ~return_type = Varargs (args_type, return_type, T)
      end
    end

    module Async = struct
      exception Failed_to_parse of Error.t

      type 'f t =
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

      let rec make_fn : type fn. fn t -> fn -> Msgpack.t list -> unit Deferred.Or_error.t =
        fun arity f l ->
        match arity, l with
        | Varargs typ, l ->
          (match List.map l ~f:(Extract.value typ) |> Or_error.combine_errors with
           | Error error ->
             raise (Failed_to_parse (Error.tag error ~tag:"Wrong argument type"))
           | Ok l -> f l)
        | Unit, [] -> f
        | Cons (leftmost, rest), x :: xs ->
          (match%bind Extract.value leftmost x |> return with
           | Ok v ->
             let f' = f v in
             make_fn rest f' xs
           | Error error ->
             raise (Failed_to_parse (Error.tag error ~tag:"Wrong argument type")))
        | _ ->
          (* This should be caught by the [valid_number_of_args] check. *)
          raise (Failed_to_parse (Error.of_string "[BUG] Wrong number of arguments"))
      ;;

      let unit = Unit
      let ( @-> ) a b = Cons (a, b)

      module Expert = struct
        let varargs typ = Varargs typ
      end
    end
  end
end

let wrap_viml_function ~type_ ~function_name = Defun.Vim.make_fn function_name type_ Fn.id

let register_request_blocking_internal client ~name ~type_ ~f ~wrap_f =
  let client = Type_equal.conv Client.Private.eq client in
  let valid_number_of_args = Defun.Ocaml.Sync.valid_number_of_args type_ in
  let f ~keyboard_interrupted client params =
    match valid_number_of_args (List.length params) with
    | false -> Deferred.Or_error.error_string "Wrong number of arguments"
    | true ->
      wrap_f (fun () ->
        Defun.Ocaml.Sync.make_fn type_ (f ~keyboard_interrupted ~client) params)
  in
  client.register_request_blocking ~name ~f
;;

let register_request_async_internal client ~name ~type_ ~f ~wrap_f =
  let client = Type_equal.conv Client.Private.eq client in
  let valid_number_of_args = Defun.Ocaml.Async.valid_number_of_args type_ in
  let f client params =
    let event = { Msgpack_rpc.Event.method_name = name; params } in
    match valid_number_of_args (List.length params) with
    | false ->
      Error.create_s [%message "Wrong number of arguments" (event : Msgpack_rpc.Event.t)]
      |> Notifier.error client
    | true ->
      don't_wait_for
        (match%map
           Monitor.try_with ~extract_exn:true (fun () ->
             wrap_f (fun () -> Defun.Ocaml.Async.make_fn type_ (f ~client) params))
         with
         | Ok (Ok ()) -> ()
         | Ok (Error error) -> Notifier.error client error
         | Error (Defun.Ocaml.Async.Failed_to_parse error) ->
           Notifier.error
             client
             (Error.tag_s error ~tag:[%sexp (event : Msgpack_rpc.Event.t)])
         | Error exn -> raise exn)
  in
  client.register_request_async ~name ~f
;;

let register_request_async = register_request_async_internal ~wrap_f:(fun f -> f ())
let register_request_blocking = register_request_blocking_internal ~wrap_f:(fun f -> f ())

module Expert = struct
  module Notifier = Notifier
end

module Private = struct
  let register_request_async = register_request_async_internal
  let register_request_blocking = register_request_blocking_internal
end
