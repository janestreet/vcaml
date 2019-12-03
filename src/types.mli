open! Core
open! Async

module Window : sig
  type t = Nvim_internal.Types.Window.t

  include Sexpable.S with type t := t
  include Hashable.S with type t := t
  include Comparable.S with type t := t

  val of_msgpack : Msgpack.t -> t Or_error.t
  val to_msgpack : t -> Msgpack.t
end

module Buf : sig
  type t = Nvim_internal.Types.Buffer.t [@@deriving sexp_of]

  include Sexpable.S with type t := t
  include Hashable.S with type t := t
  include Comparable.S with type t := t

  val of_msgpack : Msgpack.t -> t Or_error.t
  val to_msgpack : t -> Msgpack.t
end

module Tabpage : sig
  type t = Nvim_internal.Types.Tabpage.t [@@deriving sexp_of]

  include Sexpable.S with type t := t
  include Hashable.S with type t := t
  include Comparable.S with type t := t

  val of_msgpack : Msgpack.t -> t Or_error.t
  val to_msgpack : t -> Msgpack.t
end

type client =
  { events : (Msgpack_rpc.event -> unit) Bus.Read_only.t
  ; call_nvim_api_fn : 'a. 'a Nvim_internal.Types.api_result -> 'a Deferred.Or_error.t
  ; register_request :
      name:string -> f:(Msgpack.t list -> Msgpack.t Or_error.t) -> unit Or_error.t
  ; buffers_attached : int Buf.Table.t
  ; attach_sequencer : unit Sequencer.t
  }

module Client_info : sig
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

module Chan_info : sig
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
