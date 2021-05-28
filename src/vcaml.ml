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
module Keymap = Keymap
module Mark = Mark
module Mode = Mode
module Nvim = Nvim
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
      | Unix : string -> Client.t t
      | Embed :
          { prog : string
          ; args : string list
          ; working_dir : string
          ; env : (string * string) list
          }
          -> (Client.t * Async.Process.t) t
      | Child : Client.t t
  end

  open Connection_type

  module Make_child () = struct
    module T = struct
      type t =
        { reader : Reader.t
        ; writer : Writer.t
        }
      [@@deriving fields]
    end

    include Msgpack_rpc.Make (T) ()

    let self =
      connect { T.reader = Lazy.force Reader.stdin; writer = Lazy.force Writer.stdout }
    ;;
  end

  module Make_embedded () = struct
    open Async

    module T = struct
      type t =
        { reader : Reader.t
        ; writer : Writer.t
        }
      [@@deriving fields]
    end

    include Msgpack_rpc.Make (T) ()

    let spawn ~prog ~args ~working_dir ~env ~on_error =
      let open Deferred.Or_error.Let_syntax in
      let%bind underlying =
        Process.create ~prog ~args ~working_dir ~env:(`Replace env) ()
      in
      let conn =
        { T.reader = Process.stdout underlying; writer = Process.stdin underlying }
      in
      return (connect conn ~on_error, underlying)
    ;;
  end

  let attach (type a) (connection_type : a Connection_type.t) ~on_error
    : a Deferred.Or_error.t
    =
    let on_msgpack_error ~message msgpack =
      on_error (Error.create_s [%message message ~_:(msgpack : Msgpack.t)])
    in
    match connection_type with
    | Unix sock_name ->
      let module T = Transport.Make (Msgpack_unix) in
      let%bind connection = Msgpack_unix.Unix_socket.open_from_filename sock_name in
      let unix = Msgpack_unix.connect connection ~on_error:on_msgpack_error in
      let%bind client = T.attach unix ~on_error in
      Deferred.Or_error.return client
    | Embed { prog; args; working_dir; env } ->
      let module Embedded = Make_embedded () in
      let module T = Transport.Make (Embedded) in
      let%bind.Deferred.Or_error embedded, process =
        Embedded.spawn ~prog ~args ~working_dir ~env ~on_error:on_msgpack_error
      in
      let%bind client = T.attach embedded ~on_error in
      Deferred.Or_error.return (client, process)
    | Child ->
      let module Child = Make_child () in
      let module T = Transport.Make (Child) in
      let%bind client = T.attach (Child.self ~on_error:on_msgpack_error) ~on_error in
      Deferred.Or_error.return client
  ;;

  let is_channel_with_name ~channel_info ~name =
    let open Option.Let_syntax in
    let channel_has_same_name =
      let%bind { name = name_opt; _ } = channel_info.Channel_info.client in
      let%map chan_name = name_opt in
      String.equal chan_name name
    in
    Option.value channel_has_same_name ~default:false
  ;;

  let find_rpc_channel_with_name ~client ~name =
    let open Deferred.Or_error.Let_syntax in
    let%bind channel_list = Api_call.run_join client Nvim.list_chans in
    let matching_channel_opt =
      List.find channel_list ~f:(fun channel_info ->
        is_channel_with_name ~channel_info ~name)
    in
    match matching_channel_opt with
    | Some { id; _ } -> return id
    | None ->
      Deferred.Or_error.error_string "Cannot find rpc with the correct name for plugin"
  ;;

  (* Returns neovim's id for the channel over which neovim and the client communicate.
     This can be useful when you want to register an RPC event to fire upon a certain
     event happening in vim (e.g. keypress or autocmd), since registering events require
     the channel id. *)
  let get_rpc_channel_id client =
    let name = Uuid.to_string (Uuid_unix.create ()) in
    let%bind.Deferred.Or_error () =
      Api_call.run_join client (Nvim.set_client_info ~name ~type_:`Plugin ())
    in
    find_rpc_channel_with_name ~client ~name
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
        Extract.value
          ~err_msg:"return type given to [wrap_viml_function] is incorrect"
          return_type
          result
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
        : type fn i.
          Client.t -> (fn, i) t -> fn -> Msgpack.t list -> Msgpack.t Deferred.Or_error.t
        =
        fun client arity f l ->
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
          make_fn client rest (f v) xs
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
  client.register_request
    ~name
    ~f:
      (let T = Client.Private.eq in
       Defun.Ocaml.Sync.make_fn client type_ f)
;;

let register_request_async ?on_error (client : Client.t) ~name ~type_ ~f =
  let T = Client.Private.eq in
  let on_error = Option.value on_error ~default:client.on_error in
  Bus.iter_exn client.events [%here] ~f:(fun ({ method_name; params } as event) ->
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
