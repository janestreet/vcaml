open! Core
open Async
module Internal = Nvim_internal
module Client_info = Client_info
module Channel_info = Channel_info
module Nvim_command = Nvim_command
module Keymap = Keymap
module Buf = Buf
module Window = Window
module Tabpage = Tabpage

module Client : sig
  include module type of Client

  type t = Types.client

  (** A value of type [Connection_type.t] describes the type of connection to
      use, along with the information necessary to construct the [Msgpack_rpc]
      channel. *)
  module Connection_type : sig
    type t =
      | Unix of string
      (** neovim opens up a unix pipe that plugins can connect to, so vcaml
          (when given a path to this pipe), can connect and start talking to
          neovim *)
      | Embed of
          { prog : string
          ; args : string list
          ; working_dir : string
          ; env : (string * string) list
          }
      (** VCaml spawns a neovim instance with all of the provided
          parameters and then communicates with that process with that
          processes stdin and stdout *)
      | Child
      (** The Vcaml program is running as a child of neovim, and
          communicates with neovim via its own stdin and stdout *)
  end

  val attach : Connection_type.t -> (t * Async.Process.t option) Deferred.Or_error.t

  val embed
    :  prog:string
    -> args:string list
    -> working_dir:string
    -> env:(string * string) list
    -> (t * Async.Process.t option) Deferred.Or_error.t
end

(** A [Type.t] is a reified values of a primitive type used in nvim. *)
module Type : sig
  type 'ty t =
    | Nil : unit t
    | Integer : int t
    | Boolean : bool t
    | Array : 'a t -> 'a list t
    | Tuple : 'a t * int -> 'a list t
    | Dict : (Msgpack.t * Msgpack.t) list t
    | String : string t
    | Buffer : Buf.t t
    | Tabpage : Tabpage.t t
    | Window : Window.t t
    | Object : Msgpack.t t
    | Custom :
        { of_msgpack : Msgpack.t -> 'a Or_error.t
        ; to_msgpack : 'a -> Msgpack.t
        }
        -> 'a t
end

(** A ['a api_call] is a thunked call to neovim returning a Msgpack-encoded
    ['a]. No RPC traffic is generated until an [api_call] is invoked via [run]
    or [run_join].

    [api_call]'s can be manipulated with an applicative-like interface.

    A good mental model is that invoking a ['a api_call] should cause exactly
    one RPC message to be sent to the neovim client, and that any operations
    within will not be interrupted. Calls with side effects will occur in the
    order written, so

    {[
      let%map _a = a
      and _b = b
      in
      ()
    ]}

    will cause neovim to first run [a] and then [b].

    This is important for applications that rely on manipulating neovim's
    internal state. In particular, the atomicity guarantee prevents races with
    other pending operations, including user input.

    You can run an [Api_call.t] with [run] or [run_join]. *)
type 'a api_call = 'a Api_call.t

module Api_call : sig
  module Let_syntax : sig
    module Let_syntax : sig
      val map : 'a api_call -> f:('a -> 'b) -> 'b api_call
      val both : 'a api_call -> 'b api_call -> ('a * 'b) api_call
      val return : 'a -> 'a api_call
    end
  end
end

val run : Client.t -> 'a api_call -> 'a Or_error.t Deferred.t
val run_join : Client.t -> 'a Or_error.t api_call -> 'a Or_error.t Deferred.t

(** Contains a getter and setter for a given variable or property. *)
module Property : sig
  type 'a t =
    { get : 'a Or_error.t api_call
    ; set : 'a -> unit Or_error.t api_call
    }
end

module Defun : sig
  (** A [Defun.Vim.t] value is a reified value corresponding to the type of
      a function. It is used by [wrap_viml_function] to produce a regular ocaml
      function of the correct type. *)
  module Vim : sig
    type ('f, 'leftmost_input, 'out) t

    (** Wraps a [Type.t] to be used as the rightmost (return) type of this function. *)
    val return : 'a Type.t -> ('a Or_error.t api_call, unit, 'a) t

    val unary : 'a Type.t -> 'b Type.t -> ('a -> 'b Or_error.t api_call, 'a, 'b) t

    (** Add an extra argument to an existing function arity.

        Using this operator, function types will look extremely closely to how
        the underlying OCaml type will end up. For example, a vim function with
        (OCaml) type [int -> string -> int -> buffer] would use the arity
        [Integer @-> String @-> Integer @-> return Buffer].

        Unfortunately, there is currently no nice way to automatically
        de/serialize more complex/custom types. In those cases, you may need to
        use [Object] or [Dictionary], then wrap the output of
        [wrap_viml_function] to call your serialization or deserialization
        functions. *)
    val ( @-> ) : 'a Type.t -> ('b, _, 'output) t -> ('a -> 'b, 'a, 'output) t
  end

  (** [Defun.Ocaml] is analogous to [Defun.Vim], except used to specify
      OCaml-defined functions callable from neovim.

      A synchronous method call blocks neovim until a response is given. To
      prevent starvation and deadlocking, then, synchronous callbacks should
      not call any neovim functions or otherwise cause an async cycle to be
      run.

      Neovim does not natively expose any way to receive return values from
      asynchronous method calls. *)
  module Ocaml : sig
    module Sync : sig
      type ('f, 'leftmost_input) t

      val return : 'a Type.t -> ('a Or_error.t, unit) t
      val ( @-> ) : 'a Type.t -> ('b, _) t -> ('a -> 'b, 'a) t
    end

    module Async : sig
      type 'f t

      val unit : unit Deferred.t t
      val ( @-> ) : 'a Type.t -> 'b t -> ('a -> 'b) t
    end
  end
end

(** Given the name of a function available in vimscript (VimL) along with its
    arity (see [Defun.Vim]), return a regularly-typed OCaml function that calls
    said function.

    This is intended for client authors to delegate work back to neovim,
    possibly to call an existing vimscript function. Before reaching for this
    function, please check the functions available in [Neovim], [Buf], [Window]
    and [Tabpage] to see that the functionality you intend to wrap isn't
    directly exposed in the API. *)
val wrap_viml_function
  :  type_:('fn, 'leftmost, 'out) Defun.Vim.t
  -> function_name:string
  -> 'fn

val wrap_var : name:string -> type_:'a Type.t -> 'a Property.t
val wrap_option : name:string -> type_:'a Type.t -> 'a Property.t
val wrap_get_vvar : name:string -> type_:'a Type.t -> 'a Or_error.t api_call

(** Register a function callable from neovim. Note that this does not define
    the function in vimL itself, but instead adds a listener to the neovim
    msgpack_rpc bus. Such functions can be invoked from neovim via [rpcrequest]
    or [rpcnotify] along the correct channel. This is so that library wrappers
    can choose how to implement the actual vimL functions as needed.

    A synchronous request sends a request to the OCaml program, and then blocks
    on receiving a response. This can cause deadlocks if said request itself
    attempts to perform neovim operations, so synchronous functions should run
    as quickly as possible without binding on any [Deferred]s. This is enforced
    by the definition of [Defun.Sync.Type].

    Asynchronous requests cannot directly return values to neovim, but do not
    block neovim while running. This can be used to indirectly update neovim
    by, for example, calling another neovim function within the handler (which
    can itself send another async request, etc). *)
val register_request_blocking
  :  Client.t
  -> name:string
  -> type_:('fn, 'leftmost) Defun.Ocaml.Sync.t
  -> f:'fn
  -> unit Or_error.t

val register_request_async
  :  Client.t
  -> name:string
  -> type_:'fn Defun.Ocaml.Async.t
  -> f:'fn
  -> unit

(* This function is meant for users to wrap existing vcaml functions. Library
   maintainers should instead use the [Custom] variant of [Type.t]. *)
val convert_msgpack_response
  :  'a Type.t
  -> Msgpack.t Or_error.t api_call
  -> 'a Or_error.t api_call
