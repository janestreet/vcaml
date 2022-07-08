module Unshadow_buffer := Buffer
module Unshadow_command := Command
open! Core
open Async
module Buffer = Unshadow_buffer
module Channel_info = Channel_info
module Client_info = Client_info
module Color = Color
module Command = Unshadow_command
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
module Version = Nvim_internal.Version
module Window = Window

(** API version for which this library is built (not the same as the Neovim version). *)
val version : Version.t

module Client : sig
  type 'state t = 'state Client.t

  (** [on_error] is invoked when VCaml fails to parse a response from Neovim and when
      Neovim sends us an asynchronous error event to inform us that it encountered a
      problem with a message we sent. *)
  val create
    :  on_error:[ `Raise | `Call of Vcaml_error.t -> unit ]
    -> [ `not_connected ] t

  (** A value of type [Connection_type.t] describes the type of connection to use, along
      with the information necessary to connect to Neovim.

      With a [Unix] connection, the plugin communicates with Neovim over the unix domain
      socket it uses to serve RPC requests. [Unix `Child] should be used if the plugin is
      launched from within Neovim; if it is launched independently a path to the socket
      will need to be provided.

      With a [Stdio] connection, the plugin communicates with Neovim using its own stdin
      and stdout (which means stdout cannot be used for logging). This connection type
      should only be used if the plugin is launched from Neovim with [jobstart] with
      [rpc:1] in [opts].

      [Stdio] connections are useful for synchronous, "one-shot" plugins where you want to
      synchronously start the process, communicate with Neovim, and shut down. To make
      this work, after starting the plugin, issue an [rpcrequest], which will cause Neovim
      to block. The plugin should register the requested RPC before connecting to Neovim
      to ensure the RPC is defined at the time Neovim's request is handled. After handling
      the request, the plugin should shut down. If you tried to do this with a [Unix]
      connection then after the process is launched you would need to create a new channel
      but Neovim can't do that while in the middle of processing whatever logic it's
      currently executing that launched the process. To achieve synchronicity in this way
      you'd need a continuation - after launching the process you'd need to yield to the
      event loop so the channel could be established, and then the plugin would need to
      invoke a callback in Neovim to continue.

      The [Embed] connection is the inverse of the [Stdio] connection - instead of the
      OCaml app being launched by Neovim, Neovim is launched by the OCaml app. Just as
      in a [Stdio] connection the app's stdin and stdout are used for RPC communication,
      here Neovim's stdin and stdout are used to communicate with the embedding process.
      [Embed] is most useful for testing and for graphical applications that want to
      embed Neovim for editing text. When [Embed] is used the [--embed] flag must be
      passed in [args]. *)
  module Connection_type : sig
    type _ t =
      | Unix : [ `Child | `Socket of string ] -> [ `connected ] Client.t t
      | Stdio : [ `connected ] Client.t t
      | Embed :
          { prog : string
          ; args : string list
          ; working_dir : string
          ; env : Core_unix.env
          }
          -> ([ `connected ] Client.t * Async.Process.t) t
  end

  (** Attach to Neovim over an RPC channel. Once [attach] is called Neovim can start
      sending RPC requests and notifications, so handlers should be registered in advance
      with [register_request_async] and [register_request_blocking] as needed. Registering
      more handlers after attaching is allowed. Calling [attach] twice will raise. *)
  val attach
    :  ?close_reader_and_writer_on_disconnect:(* Default: [true] *) bool
    -> [ `not_connected ] t
    -> 'a Connection_type.t
    -> time_source:Time_source.t
    -> 'a Deferred.Or_error.t

  (** Close the client and release the underlying file descriptors. Can be called safely
      multiple times. *)
  val close : [ `connected ] t -> unit Deferred.t

  (** Returns Neovim's id for the channel over which Neovim and the client communicate.
      This can be useful when you want to set an autocmd or key mapping that issues an
      [rcprequest] or [rpcnotify] when triggered, since these functions requre the channel
      id as an argument. *)
  val channel : [ `connected ] t -> int
end

(** A ['a Api_call.t] is a thunked call to neovim returning a Msgpack-encoded ['a]. No RPC
    traffic is generated until an [Api_call.t] is invoked via [run] or [run_join].

    [Api_call.t]'s can be manipulated with an applicative-like interface.

    A good mental model is that invoking a ['a Api_call.t] should cause exactly one RPC
    message to be sent to the neovim client, and that any operations within will not be
    interrupted. Calls with side effects will occur in the order written, so

    {[
      let%map _a = a
      and _b = b
      in
      ()
    ]}

    will cause Neovim to first run [a] and then [b].

    This is important for applications that rely on manipulating neovim's internal state.
    In particular, the atomicity guarantee prevents races with other pending operations,
    including user input.

    You can run an [Api_call.t] with [run] or [run_join]. *)
module Api_call : sig
  include Applicative.S with type 'a t = 'a Api_call.t
  include Applicative.Let_syntax with type 'a t := 'a Api_call.t
  module Or_error = Api_call.Or_error
end

val run
  :  Source_code_position.t
  -> [ `connected ] Client.t
  -> 'a Api_call.t
  -> 'a Deferred.Or_error.t

val run_join
  :  Source_code_position.t
  -> [ `connected ] Client.t
  -> 'a Api_call.Or_error.t
  -> 'a Deferred.Or_error.t

module Defun : sig
  (** A [Defun.Vim.t] value is a reified value corresponding to the type of a function. It
      is used by [wrap_viml_function] to produce a regular ocaml function of the correct
      type.

      Important notes about [Nil]:

      1. If you are wrapping a function that takes no arguments, just use [return T]. Do
      not use [Nil @-> return T].

      2. If you are wrapping a native (non-API) Vimscript function that does not have an
      explicit return statement, its implicit return is [Integer 0], not [Nil]. *)
  module Vim : sig
    type ('f, 'leftmost_input, 'out) t

    (** Wraps a [Type.t] to be used as the rightmost (return) type of this function. *)
    val return : 'a Type.t -> ('a Api_call.Or_error.t, unit, 'a) t

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
        (** Supports the rare case of interoperating with a Vimscript function that takes
            a callback that takes a variable number of arguments. *)
        val varargs
          :  args_type:'a Type.t
          -> return_type:'b Type.t
          -> ('a list -> 'b Deferred.Or_error.t, 'a list) t
      end
    end

    module Async : sig
      type 'f t

      val unit : unit Deferred.Or_error.t t
      val ( @-> ) : 'a Type.t -> 'b t -> ('a -> 'b) t

      module Expert : sig
        val varargs : 'a Type.t -> ('a list -> unit Deferred.Or_error.t) t
      end
    end
  end
end

(** Given the name of a function available in Vimscript (VimL) along with its arity (see
    [Defun.Vim]), return a regularly-typed OCaml function that calls said function.

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

    When the user presses Ctrl-C to interrupt a blocking call, [keyboard_interrupted]
    will be determined. Use that to run any necessary cleanup. If you call back into
    Neovim during the blocking RPC, consider whether a keyboard interrupt should prevent
    those calls from being run.

    An async request will enqueue logic on Neovim's event loop instead of blocking.
    Importantly, the state of the editor may have changed between the time the async
    request was made and the time Neovim process any of its logic. *)
val register_request_blocking
  :  _ Client.t
  -> name:string
  -> type_:('fn, 'leftmost) Defun.Ocaml.Sync.t
  -> f:(keyboard_interrupted:unit Deferred.t -> client:[ `connected ] Client.t -> 'fn)
  -> unit

val register_request_async
  :  _ Client.t
  -> name:string
  -> type_:'fn Defun.Ocaml.Async.t
  -> f:(client:[ `connected ] Client.t -> 'fn)
  -> unit

module Expert : sig
  module Notifier = Notifier
end

(* These functions are exported solely for the vcaml_plugin library's use. Clients should
   not call them. *)
module Private : sig
  val register_request_blocking
    :  _ Client.t
    -> name:string
    -> type_:('fn, 'leftmost) Defun.Ocaml.Sync.t
    -> f:(keyboard_interrupted:unit Deferred.t -> client:[ `connected ] Client.t -> 'fn)
    -> wrap_f:((unit -> Msgpack.t Deferred.Or_error.t) -> Msgpack.t Deferred.Or_error.t)
    -> unit

  val register_request_async
    :  _ Client.t
    -> name:string
    -> type_:'fn Defun.Ocaml.Async.t
    -> f:(client:[ `connected ] Client.t -> 'fn)
    -> wrap_f:((unit -> unit Deferred.Or_error.t) -> unit Deferred.Or_error.t)
    -> unit
end
