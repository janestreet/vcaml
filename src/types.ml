(* This files only purpose is avoid module dependency cycles via
   re-exporting types and functions. *)

open! Core
open! Async
module Subscriber_key = Unique_id.Int63 ()

module Window = struct
  type t = Nvim_internal.Types.Window.t [@@deriving sexp]

  include Comparable.Make (Nvim_internal.Types.Window)
  include Hashable.Make (Nvim_internal.Types.Window)

  let window_tyid = 1

  let of_msgpack =
    let open Msgpack in
    function
    | Extension { type_id; data } when Int.equal type_id window_tyid ->
      let open Or_error in
      t_of_string (Bytes.to_string data) >>= Nvim_internal.Types.Window.of_msgpack
    | _ -> Or_error.error_string "not a buffer message!"
  ;;

  let to_msgpack t =
    let open Msgpack in
    let data =
      Nvim_internal.Types.Window.to_msgpack t
      |> Msgpack.string_of_t_exn
      |> Bytes.of_string
    in
    Extension { type_id = window_tyid; data }
  ;;
end

module Buf = struct
  type t = Nvim_internal.Types.Buffer.t [@@deriving sexp]

  include Comparable.Make (Nvim_internal.Types.Buffer)
  include Hashable.Make (Nvim_internal.Types.Buffer)

  let buffer_tyid = 0

  let of_msgpack =
    let open Msgpack in
    function
    | Extension { type_id; data } when Int.equal type_id buffer_tyid ->
      let open Or_error.Let_syntax in
      let%bind msg = t_of_string (Bytes.to_string data) in
      Nvim_internal.Types.Buffer.of_msgpack msg
    | _ -> Or_error.error_string "not a buffer message!"
  ;;

  let to_msgpack t =
    let open Msgpack in
    let data =
      Nvim_internal.Types.Buffer.to_msgpack t
      |> Msgpack.string_of_t_exn
      |> Bytes.of_string
    in
    Extension { type_id = buffer_tyid; data }
  ;;
end

module Tabpage = struct
  type t = Nvim_internal.Types.Tabpage.t [@@deriving sexp]

  include Comparable.Make (Nvim_internal.Types.Tabpage)
  include Hashable.Make (Nvim_internal.Types.Tabpage)

  let tabpage_tyid = 2

  let of_msgpack =
    let open Msgpack in
    function
    | Extension { type_id; data } when Int.equal type_id tabpage_tyid ->
      let open Or_error in
      t_of_string (Bytes.to_string data) >>= Nvim_internal.Types.Tabpage.of_msgpack
    | _ -> Or_error.error_string "not a buffer message!"
  ;;

  let to_msgpack t =
    let open Msgpack in
    let data =
      Nvim_internal.Types.Tabpage.to_msgpack t
      |> Msgpack.string_of_t_exn
      |> Bytes.of_string
    in
    Extension { type_id = tabpage_tyid; data }
  ;;
end

type client =
  { events : (Msgpack_rpc.event -> unit) Bus.Read_only.t
  ; call_nvim_api_fn : 'a. 'a Nvim_internal.Types.api_result -> 'a Deferred.Or_error.t
  ; register_request :
      name:string -> f:(Msgpack.t list -> Msgpack.t Or_error.t) -> unit Or_error.t
  ; buffers_attached : int Buf.Table.t
  ; attach_sequencer : unit Sequencer.t
  }

module Client_info = struct
  type version =
    { major : int option
    ; minor : int option
    ; patch : int option
    ; prerelease : string option
    ; commit : string option
    }
  [@@deriving sexp_of]

  type client_type =
    [ `Remote
    | `Ui
    | `Embedder
    | `Host
    | `Plugin
    ]
  [@@deriving sexp_of]

  type client_method =
    { async : bool
    ; nargs : [ `Fixed of int | `Range of int * int ] option
    ; opts : Msgpack.t String.Map.t
    }
  [@@deriving sexp_of]

  type t =
    { version : version option
    ; methods : client_method String.Map.t
    ; attributes : string String.Map.t
    ; name : string option
    ; type_ : client_type option
    }
  [@@deriving sexp_of]
end

module Chan_info = struct
  type t =
    { id : int
    ; stream : [ `Stdio | `Stderr | `Socket | `Job ]
    ; mode : [ `Bytes | `Terminal | `Rpc ]
    ; pty : string option
    ; buffer : Buf.t option
    ; client : Client_info.t option
    }
end

module Phantom = Nvim_internal.Types.Phantom
