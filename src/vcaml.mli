module Unshadow_buffer := Buffer
module Unshadow_command := Command
open! Core
open Async
module Buffer = Unshadow_buffer
module Channel_info = Channel_info
module Client_info = Client_info
module Color = Color
module Command = Unshadow_command
module Keymap = Keymap
module Mark = Mark
module Mode = Mode
module Nvim = Nvim
module Tabpage = Tabpage
module Type = Nvim_internal.Phantom
module Ui = Ui
module Version = Nvim_internal.Version
module Window = Window

(** Version of Neovim for which this library is built. *)
val version : Version.t

module Client : sig
  type t = Client.t

  (** A value of type [Connection_type.t] describes the type of connection to
      use, along with the information necessary to construct the [Msgpack_rpc]
      channel. *)
  module Connection_type : sig
    type _ t =
      | Unix : string -> Client.t t
      (** neovim opens up a unix pipe that plugins can connect to, so vcaml
          (when given a path to this pipe), can connect and start talking to
          neovim
          A note on using this kind of connection: if the VCaml plugin is being
          run as a bang (!) command, api calls such as command and command_output
          may hang, as they must wait until the invocation of the plugin terminates
          before being executed by neovim.
      *)
      | Embed :
          { prog : string
          ; args : string list
          ; working_dir : string
          ; env : (string * string) list
          }
          -> (Client.t * Async.Process.t) t
      (** VCaml spawns a neovim instance with all of the provided
          parameters and then communicates with that process with that
          processes stdin and stdout *)
      | Child : Client.t t
      (** The Vcaml program is running as a child of neovim, and
          communicates with neovim via its own stdin and stdout *)
  end

  (** The [on_error] callback passed here will be used to handle asynchronous errors from
      the Msgpack RPC layer and is the default fallback in cases where an optional handler
      is not provided for handling errors for specific kinds of messages. *)
  val attach
    :  'a Connection_type.t
    -> on_error:(Error.t -> unit)
    -> 'a Deferred.Or_error.t

  val get_rpc_channel_id : t -> int Deferred.Or_error.t
end

(** A ['a Api_call.t] is a thunked call to neovim returning a Msgpack-encoded
    ['a]. No RPC traffic is generated until an [Api_call.t] is invoked via [run]
    or [run_join].

    [Api_call.t]'s can be manipulated with an applicative-like interface.

    A good mental model is that invoking a ['a Api_call.t] should cause exactly
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
module Api_call : sig
  include Applicative.S with type 'a t = 'a Api_call.t
  include Applicative.Let_syntax with type 'a t := 'a Api_call.t
  module Or_error = Api_call.Or_error
end

val run : Client.t -> 'a Api_call.t -> 'a Deferred.Or_error.t
val run_join : Client.t -> 'a Api_call.Or_error.t -> 'a Deferred.Or_error.t

module Defun : sig
  (** A [Defun.Vim.t] value is a reified value corresponding to the type of
      a function. It is used by [wrap_viml_function] to produce a regular ocaml
      function of the correct type. *)
  module Vim : sig
    type ('f, 'leftmost_input, 'out) t

    (** Wraps a [Type.t] to be used as the rightmost (return) type of this function. Note
        that native (non-API) Vimscript functions always return a value (the default
        return value is 0). *)
    val return : 'a Type.t -> ('a Api_call.Or_error.t, unit, 'a) t

    (** N.B. If you are wrapping a function that takes no arguments, just use [return T].
        Do not use [unary Nil (return T)] or [Nil @-> return T]. *)
    val unary : 'a Type.t -> 'b Type.t -> ('a -> 'b Api_call.Or_error.t, 'a, 'b) t

    (** Add an extra argument to an existing function arity.

        Using this operator, function types will look extremely closely to how the
        underlying OCaml type will end up. For example, a Vim function with (OCaml) type
        [int -> string -> int -> buffer] would use the arity [Integer @-> String @->
        Integer @-> return Buffer]. *)
    val ( @-> ) : 'a Type.t -> ('b, _, 'output) t -> ('a -> 'b, 'a, 'output) t
  end

  (** [Defun.Ocaml] is analogous to [Defun.Vim], except used to specify OCaml-defined
      functions callable from neovim. See [register_request_blocking] and
      [register_request_async] below for usage. *)
  module Ocaml : sig
    module Sync : sig
      type ('f, 'leftmost_input) t

      val return : 'a Type.t -> ('a Deferred.Or_error.t, unit) t
      val ( @-> ) : 'a Type.t -> ('b, _) t -> ('a -> 'b, 'a) t

      module Expert : sig
        (** Supports the rare case of interoperating with a Vimscript function that
            takes a callback that takes a variable number of arguments. *)
        val varargs
          :  args_type:'a Type.t
          -> return_type:'b Type.t
          -> ('a list -> 'b Deferred.Or_error.t, 'a list) t
      end
    end

    module Async : sig
      type 'f t

      val unit : unit Deferred.t t
      val ( @-> ) : 'a Type.t -> 'b t -> ('a -> 'b) t

      module Expert : sig
        val varargs : 'a Type.t -> ('a list -> unit Deferred.t) t
      end
    end
  end
end

(** Given the name of a function available in Vimscript (VimL) along with its
    arity (see [Defun.Vim]), return a regularly-typed OCaml function that calls
    said function.

    This is intended for client authors to delegate work back to Neovim, possibly to call
    an existing Vimscript function. Before reaching for this function, please check the
    functions available in [Nvim], [Buffer], [Window] and [Tabpage] to see that the
    functionality you intend to wrap isn't directly exposed in the API. *)
val wrap_viml_function
  :  type_:('fn, 'leftmost, 'out) Defun.Vim.t
  -> function_name:string
  -> 'fn

(** [register_request_blocking] and [register_request_async] register functions that can
    be called from Neovim via [rpcrequest] and [rpcnotify] respectively. This is achieved
    by adding a listener to the Neovim msgpack_rpc bus.

    A blocking request will block Neovim from processing user input or communication over
    other channels until a response is returned. Neovim will continue to process calls
    sent over the same channel while a blocking request is in flight, which means nested
    calls are supported.

    Asynchronous requests cannot directly return values to Neovim, but they do not block
    Neovim while running. They can be used to indirectly update Neovim and avoid blocking
    user input and other events. *)
val register_request_blocking
  :  Client.t
  -> name:string
  -> type_:('fn, 'leftmost) Defun.Ocaml.Sync.t
  -> f:'fn
  -> unit Or_error.t

val register_request_async
  :  ?on_error:(Error.t -> unit)
  -> Client.t
  -> name:string
  -> type_:'fn Defun.Ocaml.Async.t
  -> f:'fn
  -> unit

module Expert : sig
  module Notifier = Notifier
end
