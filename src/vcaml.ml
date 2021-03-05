module Unshadow = struct
  module Buffer = Buffer
end

open Core
open Async
module Api_call = Api_call
module Buffer = Unshadow.Buffer
module Channel_info = Channel_info
module Client_info = Client_info
module Internal = Nvim_internal
module Keymap = Keymap
module Nvim_command = Nvim_command
module Tabpage = Tabpage
module Type = Types.Phantom
module Window = Window

module Client = struct
  include Client

  module Connection_type = struct
    type t =
      | Unix of string
      | Embed of
          { prog : string
          ; args : string list
          ; working_dir : string
          ; env : (string * string) list
          }
      | Child
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

    let self () =
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

    let spawn ~prog ~args ~working_dir ~env =
      let open Deferred.Or_error.Let_syntax in
      let%bind underlying =
        Process.create ~prog ~args ~working_dir ~env:(`Replace env) ()
      in
      let conn =
        { T.reader = Process.stdout underlying; writer = Process.stdin underlying }
      in
      return (connect conn, underlying)
    ;;
  end

  let attach = function
    | Unix sock_name ->
      let module T = Transport.Make (Msgpack_unix) in
      let%bind s = Msgpack_unix.Unix_socket.open_from_filename sock_name in
      let cli = Msgpack_unix.connect s in
      let%bind client = T.attach cli in
      Deferred.Or_error.return (client, None)
    | Embed { prog; args; working_dir; env } ->
      let module Embedded = Make_embedded () in
      let module T = Transport.Make (Embedded) in
      let%bind.Deferred.Or_error client, process =
        Embedded.spawn ~prog ~args ~working_dir ~env
      in
      let%bind client = T.attach client in
      Deferred.Or_error.return (client, Some process)
    | Child ->
      let module Child = Make_child () in
      let module T = Transport.Make (Child) in
      let%bind client = T.attach (Child.self ()) in
      Deferred.Or_error.return (client, None)
  ;;

  let embed ~prog ~args ~working_dir ~env =
    attach (Embed { prog; args; working_dir; env })
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
    let%bind channel_list = Api_call.run_join client Client.list_chans in
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
      Api_call.run_join client (Client.set_client_info ~name ~type_:`Plugin ())
    in
    find_rpc_channel_with_name ~client ~name
  ;;
end

module Property = struct
  type 'a t =
    { get : 'a Api_call.Or_error.t
    ; set : 'a -> unit Api_call.Or_error.t
    }
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
         not that easily expressible.
      *)
      match arity with
      | Nullary return_type ->
        let args = f [] in
        let open Api_call.Let_syntax in
        let%map result = Client.call_function ~fn:function_name ~args in
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
        | Cons : 'a Type.t * ('b, _) t -> ('a -> 'b, 'a) t

      let return t = Nullary (t, T)

      let rec make_fn
        : type fn i.
          Types.Client.t
          -> (fn, i) t
          -> fn
          -> Msgpack.t list
          -> Msgpack.t Deferred.Or_error.t
        =
        fun client arity f l ->
        let open Deferred.Or_error.Let_syntax in
        match arity, l with
        | Nullary (return_type, T), ([] | [ Nil ]) ->
          let%map v = f in
          Extract.inject return_type v
        | Cons (leftmost, rest), x :: xs ->
          let%bind v = Extract.value leftmost x |> Deferred.return in
          make_fn client rest (f v) xs
        | _, _ -> Deferred.Or_error.error_s [%message "Wrong number of arguments"]
      ;;

      let ( @-> ) a b = Cons (a, b)
    end

    module Async = struct
      type 'f t =
        | Unit : unit Deferred.t t
        | Rest : (Msgpack.t list -> unit Deferred.t) t
        | Cons : 'a Type.t * 'b t -> ('a -> 'b) t

      let rec make_fn
        : type fn.
          Types.Client.t -> string -> fn t -> fn -> Msgpack.t list -> unit Deferred.t
        =
        fun client name arity f l ->
        match arity, l with
        | Rest, l -> f l
        | Unit, [] -> return ()
        | Unit, [ Msgpack.Nil ] -> return ()
        | Cons (leftmost, rest), x :: xs ->
          (match%bind Extract.value leftmost x |> Deferred.return with
           | Ok v ->
             let f' = f v in
             make_fn client name rest f' xs
           | Error e ->
             Log.Global.error !"got wrong argument type for %s: %{sexp: Error.t}" name e;
             return ())
        | _ ->
          Log.Global.error "got wrong number of args for async request %s" name;
          return ()
      ;;

      let unit = Unit
      let rest = Rest
      let ( @-> ) a b = Cons (a, b)
    end
  end
end

let wrap_viml_function ~type_ ~function_name = Defun.Vim.make_fn function_name type_ Fn.id

let construct_getset ~name ~type_ ~remote_get ~remote_set =
  let get =
    let open Api_call.Let_syntax in
    let%map result = remote_get ~name in
    let open Or_error.Let_syntax in
    let%bind result = result in
    Extract.value
      ~err_msg:(sprintf "return type given to wrapper for %s is incorrect" name)
      type_
      result
  in
  let set v =
    let value = Extract.inject type_ v in
    remote_set ~name ~value
  in
  { Property.get; set }
;;

let wrap_var =
  construct_getset ~remote_get:Client.Untested.get_var ~remote_set:Client.Untested.set_var
;;

let wrap_get_vvar ~name ~type_ =
  let open Api_call.Let_syntax in
  let%map result = Client.Untested.get_vvar ~name in
  let open Or_error.Let_syntax in
  let%bind result = result in
  Extract.value ~err_msg:"return type given to [wrap_get_vvar] is incorrect" type_ result
;;

let wrap_option =
  construct_getset
    ~remote_get:Client.Untested.get_option
    ~remote_set:Client.Untested.set_option
;;

let register_request_blocking client ~name ~type_ ~f =
  (Types.Client.Private.of_public client).register_request
    ~name
    ~f:(Defun.Ocaml.Sync.make_fn client type_ f)
;;

let register_request_async client ~name ~type_ ~f =
  Bus.iter_exn
    (Types.Client.Private.of_public client).events
    [%here]
    ~f:(fun { Msgpack_rpc.method_name; params } ->
      if not (String.equal method_name name)
      then ()
      else Defun.Ocaml.Async.make_fn client name type_ f params |> don't_wait_for)
;;

let convert_msgpack_response type_ call =
  Api_call.map_bind
    call
    ~f:
      (Extract.value
         ~err_msg:"return type given to [convert_msgpack_response] is incorrect"
         type_)
;;
