module Unshadow_buffer := Buffer
module Unshadow_command := Command
open Core
open Async
open Import

(** VCaml is a library for interacting with Neovim from OCaml. For an overview, see
    ../README.mdx and for examples see ../plugin/example. *)

module Autocmd = Autocmd
module Buffer = Unshadow_buffer
module Channel_info = Channel_info
module Client_info = Client_info
module Color = Color
module Command = Unshadow_command
module Dynamic_option_info = Dynamic_option_info
module Highlighted_text = Highlighted_text
module Keymap = Keymap
module Mark = Mark
module Mode = Mode
module Namespace = Namespace
module Nvim = Nvim
module Ocaml_from_nvim = Ocaml_from_nvim
module Position = Position
module Tabpage = Tabpage
module Type = Type
module Ui = Ui
module Vcaml_error = Vcaml_error
module Window = Window

(** [Msgpack.pp] with support for Neovim Msgpack extensions. *)
val pp : Formatter.t -> Msgpack.t -> unit

module Nvim_version : sig
  include Semantic_version.S

  (** Version of Neovim that this version of VCaml targets. VCaml was tested against this
      Neovim version, and no other Neovim version is guaranteed to work properly with this
      version of VCaml. *)
  val t : t

  (** API version number for this version of Neovim / VCaml (see `:h api-level`). *)
  val api_level : int

  (** Oldest API version number with which this version of the API is backward-compatible.
      Note, however, that because VCaml models Neovim semantics beyond simple API
      bindings, it may not be usable with an older version of Neovim even if the API is
      backward-compatible. In general, backward-compatibility is not an objective of VCaml
      for a few reasons:

      1. Neovim users generally keep their versions of Neovim up-to-date.

      2. Neovim plugin managers support pinning to particular commits, so when the version
         of VCaml used by a plugin is upgraded, the plugin can communicate the updated
         Neovim version requirements in the README and users can handle the version change
         by pinning to a compatible commit or by upgrading their editor.

      3. Servicing older versions of Neovim is not a priority for the Neovim core team.
         They do not backport patches more than one minor release and do not address bug
         reports for versions of Neovim that are older than the last minor release. *)
  val api_compatible : int
end

(** [block_nvim client f] blocks Neovim while [f] runs. This is useful to ensure that
    Neovim's state does not change across a sequence of calls. Because this blocks the
    user, take care to limit the scope of [f] to the minimal amount of work necessary. Any
    further uses of the client passed to [f] after [f] returns will result in an error
    (e.g., do not store the client in a ref). *)
val block_nvim
  :  here:[%call_pos]
  -> [ `asynchronous ] Client.t
  -> f:([ `blocking ] Client.t -> 'a Deferred.Or_error.t)
  -> 'a Deferred.Or_error.t

(** Same as [block_nvim] but with more explicit modeling of keyboard interrupt errors. *)
val block_nvim'
  :  here:[%call_pos]
  -> [ `asynchronous ] Client.t
  -> f:([ `blocking ] Client.t -> 'a Deferred.Or_error.t)
  -> [ `Ok of 'a | `Keyboard_interrupted | `Error of Error.t ] Deferred.t

module Client : sig
  (** An RPC connection to Neovim over a channel (see `:h channel-intro`). When you create
      a client, ['kind] is [ `asynchronous ], which indicates that RPC calls into Neovim
      can be interleaved by unrelated logic or user activity. In some contexts, such as
      during synchronous RPC handlers and [block_nvim] above, a [ `blocking ] client will
      be provided, indicating that the user is blocked for the duration of the callback.
      Using the [ `blocking ] client outside the callback is invalid and will result in an
      error. *)
  type 'kind t = 'kind Client.t

  module Not_connected = Client.Not_connected
  module Maybe_connected = Client.Maybe_connected

  (** Most users should use the vcaml.plugin library (../plugin/src/vcaml_plugin.mli)
      instead of calling this function. It will set up the scaffolding for a plugin and
      provide you a client. You can create and attach your own client with these functions
      if vcaml.plugin does not fit your use case.

      [on_error] is invoked when VCaml fails to parse a notification from Neovim, when
      Neovim sends us an asynchronous error event to inform us that it encountered a
      problem with a message we sent, or when (if) Neovim unexpectedly violates the
      Msgpack RPC protocol. *)
  val create
    :  name:string
    -> on_error:[ `Raise | `Call of Vcaml_error.t -> unit ]
    -> Not_connected.t

  (** A value of type [Connection_type.t] describes the type of connection to use, along
      with the information necessary to connect to Neovim.

      With a [Socket] connection, the plugin communicates with Neovim over the TCP address
      or unix domain socket it uses to serve RPC requests. If the plugin is launched from
      within Neovim, use [Socket `Infer_from_parent_nvim] to detect the address. If the
      plugin is launched independently, use [Socket (`Address address)] to provide the
      address.

      With a [Stdio] connection, the plugin communicates with Neovim using its own stdin
      and stdout (which means stdout cannot be used for logging). This connection type
      should only be used if the plugin is launched from Neovim with [jobstart] with
      [rpc:1] in [opts].

      [Stdio] connections are useful for synchronous, "one-shot" plugins where you want to
      synchronously start the process, communicate with Neovim, and shut down. To make
      this work, after starting the plugin, issue an [rpcrequest], which will cause Neovim
      to block. The plugin should register the requested RPC before connecting to Neovim
      to ensure the RPC is defined at the time Neovim's request is handled. After handling
      the request, the plugin should shut down. If you tried to do this with a [Socket]
      connection then after the process is launched you would need to create a new channel
      but Neovim can't do that while in the middle of processing whatever logic it's
      currently executing that launched the process. To achieve synchronicity in this way
      you'd need a continuation - after launching the process you'd need to yield to the
      event loop so the channel could be established, and then the plugin would need to
      invoke a callback in Neovim to continue.

      The [Embed] connection is the inverse of the [Stdio] connection - instead of the
      OCaml app being launched by Neovim, Neovim is launched by the OCaml app. Just as in
      a [Stdio] connection the app's stdin and stdout are used for RPC communication, here
      Neovim's stdin and stdout are used to communicate with the embedding process.
      [Embed] is most useful for testing and for graphical applications that want to embed
      Neovim for editing text. When [Embed] is used the [--embed] flag must be passed in
      [args]. *)
  module Connection_type : sig
    type _ t =
      | Socket :
          [ `Infer_from_parent_nvim | `Address of string ]
          -> [ `asynchronous ] Client.t t
      | Stdio : [ `asynchronous ] Client.t t
      | Embed :
          { prog : string
          ; args : string list
          ; working_dir : string
          ; env : Core_unix.env
          }
          -> ([ `asynchronous ] Client.t * Process.t) t
  end

  (** Attach to Neovim over an RPC channel. Once [attach] is called Neovim can start
      sending RPC requests and notifications, so handlers should be registered in advance
      with [register_request_async] and [register_request_blocking] as needed. The
      [Client.Not_connected.t] passed to [attach] should be discarded after this call;
      calling [attach] on it again will raise and new handlers registered with it will be
      ignored. New handlers can still be registered on the returned [Client.t]. *)
  val attach : Not_connected.t -> 'a Connection_type.t -> 'a Deferred.Or_error.t

  (** Close the client and release the underlying file descriptors. Can be called safely
      multiple times. Note that clients provided to RPC handlers and to [block_nvim] share
      the same underlying state as the client that established the connection to Neovim,
      so calling [close] on any of them affects them all. *)
  val close : _ t -> unit Deferred.t

  (** Returns Neovim's id for the channel over which Neovim and the client communicate.
      This can be useful when you want to set an autocmd or key mapping that issues an
      [rcprequest] or [rpcnotify] when triggered, since these functions requre the channel
      id as an argument. *)
  val channel : _ t -> int

  val name : _ t -> string
end

module Expert : sig
  module Atomic = Atomic
  module Notifier = Notifier
end

module Private : sig
  module Nvim_internal = Nvim_internal
  module Nvim_lock = Nvim_lock
  module Source_code_position = Source_code_position

  val attach_client
    :  ?wrap_connection:(Reader.t -> Writer.t -> (Reader.t * Writer.t) Deferred.t)
    -> ?stdio_override:Reader.t * Writer.t
    -> ?time_source:Time_source.t
    -> Client.Not_connected.t
    -> 'a Client.Connection_type.t
    -> 'a Deferred.Or_error.t

  val notify_nvim_of_error : _ Client.t -> here:[%call_pos] -> Error.t -> unit Deferred.t
  val before_sending_response_hook_for_tests : (unit -> unit Deferred.t) option ref
end
[@@alert vcaml_private "This module is for internal VCaml use."]
