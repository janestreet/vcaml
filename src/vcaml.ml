module Unshadow = struct
  module Buffer = Buffer
  module Command = Command
end

open Core
open Async
module Api_call = Api_call
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
module Version = Nvim_internal.Version
module Window = Window

let version = Nvim_internal.version

module Client = struct
  include Client

  module Connection_type = struct
    type _ t =
      | Unix : [ `Child | `Socket of string ] -> Client.t t
      | Embed :
          { prog : string
          ; args : string list
          ; working_dir : string
          ; env : Core_unix.env
          }
          -> (Client.t * Async.Process.t) t
      | Child : Client.t t
    [@@deriving sexp_of]
  end

  let attach
        (type a)
        ?(close_reader_and_writer_on_disconnect = true)
        ?(on_error = Error.raise)
        ?(on_error_event =
          fun error_type ~message -> raise_s [%message message (error_type : Error_type.t)])
        (connection_type : a Connection_type.t)
        ~time_source
    : a Deferred.Or_error.t
    =
    let open Deferred.Or_error.Let_syntax in
    let make_client connect =
      let%bind.Deferred connection =
        connect ~on_error:(fun ~message msgpack ->
          on_error (Error.create_s [%message message ~_:(msgpack : Msgpack.t)]))
      in
      Transport.attach connection ~on_error ~on_error_event ~time_source
    in
    match connection_type with
    | Unix socket ->
      let socket =
        match socket with
        | `Socket socket -> socket
        | `Child -> Sys.getenv_exn "NVIM_LISTEN_ADDRESS"
      in
      make_client
        (Msgpack_unix.open_from_filename socket ~close_reader_and_writer_on_disconnect)
    | Embed { prog; args; working_dir; env } ->
      (match List.exists args ~f:(String.equal "--embed") with
       | false ->
         Deferred.Or_error.error_s
           [%message
             "Tried to create a VCaml client for an embedded Neovim process, but --embed \
              flag was not passed"
               ~_:(connection_type : _ Connection_type.t)]
       | true ->
         let%bind nvim_process = Process.create ~prog ~args ~working_dir ~env () in
         let%bind client =
           make_client (fun ~on_error ->
             Msgpack_rpc.connect
               (Process.stdout nvim_process)
               (Process.stdin nvim_process)
               ~on_error
               ~close_reader_and_writer_on_disconnect
             |> Deferred.return)
         in
         return (client, nvim_process))
    | Child ->
      make_client (fun ~on_error ->
        Msgpack_rpc.connect
          (force Reader.stdin)
          (force Writer.stdout)
          ~on_error
          ~close_reader_and_writer_on_disconnect
        |> Deferred.return)
  ;;

  let close (client : Client.t) =
    let T = Client.Private.eq in
    client.close ()
  ;;

  let rpc_channel_id (client : Client.t) =
    let T = Client.Private.eq in
    client.rpc_channel_id
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
    let unary t_in t_out = Cons (t_in, return t_out)
    let ( @-> ) a t = Cons (a, t)

    let rec make_fn
      : type fn i out.
        string -> (fn, i, out) t -> (Msgpack.t list -> Msgpack.t list) -> fn
      =
      fun function_name arity f ->
      (* Due to the fact that OCaml does not (easily) support higher-ranked polymorphism,
         we need to construct the function [to_msgpack] *after* we unpack this GADT, so it
         can have the type [i -> Msgpack.t] (which is fixed by [arity] in this function).
         Otherwise, it needs the type [forall 'a . 'a witness -> 'a -> Msgpack.t], which is
         not that easily expressible. *)
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

      let rec make_fn
        : type fn i. (fn, i) t -> fn -> Msgpack.t list -> Msgpack.t Deferred.Or_error.t
        =
        fun arity f l ->
        let open Deferred.Or_error.Let_syntax in
        match arity, l with
        | Nullary (return_type, T), ([] | [ Nil ]) ->
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
        | _, _ -> Deferred.Or_error.error_s [%message "Wrong number of arguments"]
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
        | Unit : unit Deferred.t t
        | Varargs : 'a Type.t -> ('a list -> unit Deferred.t) t
        | Cons : 'a Type.t * 'b t -> ('a -> 'b) t

      let rec make_fn : type fn. fn t -> fn -> Msgpack.t list -> unit Deferred.t =
        fun arity f l ->
        match arity, l with
        | Varargs typ, l ->
          (match List.map l ~f:(Extract.value typ) |> Or_error.combine_errors with
           | Error error ->
             raise (Failed_to_parse (Error.tag error ~tag:"Wrong argument type"))
           | Ok l -> f l)
        | Unit, [] -> return ()
        | Unit, [ Msgpack.Nil ] -> return ()
        | Cons (leftmost, rest), x :: xs ->
          (match%bind Extract.value leftmost x |> return with
           | Ok v ->
             let f' = f v in
             make_fn rest f' xs
           | Error error ->
             raise (Failed_to_parse (Error.tag error ~tag:"Wrong argument type")))
        | _ -> raise (Failed_to_parse (Error.of_string "Wrong number of arguments"))
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

let register_request_blocking (client : Client.t) ~name ~type_ ~f =
  let T = Client.Private.eq in
  client.register_request ~name ~f:(fun request ->
    let T = Client.Private.eq in
    let keyboard_interrupted = Bvar.wait client.keyboard_interrupts in
    let response = Defun.Ocaml.Sync.make_fn type_ (f ~keyboard_interrupted) request in
    (* In the case of a keyboard interrupt we want to return an [Ok] result instead of an
       [Error] because we don't want to display an error message to the user. We also send
       ":<BS>" command because Neovim doesn't recognize that it's in a blocked context so
       it displays a message about how to exit, and we want to hide this message. We use
       [nvim_feedkeys] instead of [nvim_input] because the latter doesn't reliably clear
       the command line. *)
    choose
      [ choice response Fn.id
      ; choice keyboard_interrupted (fun () ->
          client.call_nvim_api_fn
            (Nvim_internal.nvim_feedkeys ~keys:":\x08" ~mode:"n" ~escape_csi:true)
            Notification;
          Ok Msgpack.Nil)
      ])
;;

let register_request_async ?on_error here (client : Client.t) ~name ~type_ ~f =
  let T = Client.Private.eq in
  let on_error = Option.value on_error ~default:client.on_error in
  Bus.iter_exn client.events here ~f:(fun ({ method_name; params } as event) ->
    let T = Client.Private.eq in
    match String.equal method_name name with
    | false -> ()
    | true ->
      don't_wait_for
        (match%map
           Monitor.try_with ~extract_exn:true (fun () ->
             Defun.Ocaml.Async.make_fn type_ f params)
         with
         | Ok () -> ()
         | Error (Defun.Ocaml.Async.Failed_to_parse error) ->
           on_error (Error.tag_s error ~tag:[%sexp (event : Msgpack_rpc.event)])
         | Error exn -> raise exn))
;;

module Expert = struct
  module Notifier = Notifier
end
