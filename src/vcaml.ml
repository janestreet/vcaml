open Core
open Async
module Internal = Nvim_internal
module Client_info = Client_info
module Channel_info = Channel_info
module Nvim_command = Nvim_command
module Keymap = Keymap
module Type = Types.Phantom
module Buf = Buf
module Window = Window
module Tabpage = Tabpage

module Client = struct
  include Client

  type t = Types.client =
    { events : (Msgpack_rpc.event -> unit) Bus.Read_only.t
    ; call_nvim_api_fn : 'a. 'a Internal.Types.api_result -> 'a Deferred.Or_error.t
    ; register_request :
        name:string -> f:(Msgpack.t list -> Msgpack.t Or_error.t) -> unit Or_error.t
    ; buffers_attached : int Buf.Table.t
    ; attach_sequencer : unit Sequencer.t
    }

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

  module Child = struct
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

  module Embedded = struct
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
      let%bind.Deferred.Or_error client = T.attach cli in
      Deferred.Or_error.return (client, None)
    | Embed { prog; args; working_dir; env } ->
      let module T = Transport.Make (Embedded) in
      let%bind.Deferred.Or_error client, process =
        Embedded.spawn ~prog ~args ~working_dir ~env
      in
      let%bind.Deferred.Or_error client = T.attach client in
      Deferred.Or_error.return (client, Some process)
    | Child ->
      let module T = Transport.Make (Child) in
      let%bind.Deferred.Or_error client = T.attach (Child.self ()) in
      Deferred.Or_error.return (client, None)
  ;;

  let embed ~prog ~args ~working_dir ~env =
    attach (Embed { prog; args; working_dir; env })
  ;;
end

type 'a api_call = 'a Api_call.t

module Property = struct
  type 'a t =
    { get : 'a Or_error.t api_call
    ; set : 'a -> unit Or_error.t api_call
    }
end

let run = Api_call.run
let run_join = Api_call.run_join

module Defun = struct
  module Vim = struct
    type ('f, 'leftmost_input, 'out) t =
      | Nullary : 'output Type.t -> ('output Or_error.t Api_call.t, unit, 'output) t
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
        | Nullary : 'output Type.t -> ('output Or_error.t, unit) t
        | Cons : 'a Type.t * ('b, _) t -> ('a -> 'b, 'a) t

      let return t = Nullary t

      let rec make_fn
        : type fn i.
          Types.client -> (fn, i) t -> fn -> Msgpack.t list -> Msgpack.t Or_error.t
        =
        fun client arity f l ->
        match arity, l with
        | Nullary return_type, [] -> f |> Or_error.map ~f:(Extract.inject return_type)
        | Nullary return_type, [ Msgpack.Nil ] ->
          f |> Or_error.map ~f:(Extract.inject return_type)
        | Cons (leftmost, rest), x :: xs ->
          Extract.value leftmost x
          |> Or_error.bind ~f:(fun v ->
            let f' = f v in
            make_fn client rest f' xs)
        | _, _ -> Or_error.error_s [%message "Wrong number of arguments"]
      ;;

      let ( @-> ) a b = Cons (a, b)
    end

    module Async = struct
      type 'f t =
        | Unit : unit Deferred.t t
        | Cons : 'a Type.t * 'b t -> ('a -> 'b) t

      let rec make_fn
        : type fn.
          Types.client -> string -> fn t -> fn -> Msgpack.t list -> unit Deferred.t
        =
        fun client name arity f l ->
        match arity, l with
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
      let ( @-> ) a b = Cons (a, b)
    end
  end
end

let wrap_viml_function ~type_ ~function_name =
  Defun.Vim.make_fn function_name type_ Fn.id
;;

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
  construct_getset
    ~remote_get:Client.Untested.get_var
    ~remote_set:Client.Untested.set_var
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

let register_request_blocking ({ Client.register_request; _ } as client) ~name ~type_ ~f =
  register_request ~name ~f:(Defun.Ocaml.Sync.make_fn client type_ f)
;;

let register_request_async ({ Client.events; _ } as client) ~name ~type_ ~f =
  Bus.iter_exn events [%here] ~f:(fun { Msgpack_rpc.method_name; params } ->
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

module Api_call = struct
  module Let_syntax = Api_call.Let_syntax
end
