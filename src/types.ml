(* This files only purpose is avoid module dependency cycles via re-exporting types and
   functions. *)

open! Core
open! Async
include Nvim_internal.Types

module Client = struct
  module Private = struct
    type t =
      { events : (Msgpack_rpc.event -> unit) Bus.Read_only.t
      ; call_nvim_api_fn : 'a. 'a Api_result.t -> 'a Deferred.Or_error.t
      ; register_request :
          name:string
          -> f:(Msgpack.t list -> Msgpack.t Deferred.Or_error.t)
          -> unit Or_error.t
      ; buffers_attached : int Nvim_internal.Types.Buffer.Table.t
      ; attach_sequencer : unit Sequencer.t
      }

    let of_public = Fn.id
    let to_public = Fn.id
  end

  include Private
end

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

module Channel_info = struct
  type t =
    { id : int
    ; stream : [ `Stdio | `Stderr | `Socket | `Job ]
    ; mode : [ `Bytes | `Terminal | `Rpc ]
    ; pty : string option
    ; buffer : Nvim_internal.Types.Buffer.t option
    ; client : Client_info.t option
    }
end
