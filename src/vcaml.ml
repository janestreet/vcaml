module Unshadow = struct
  module Buffer = Buffer
  module Command = Command
end

open Core
open Async
open Import
module Autocmd = Autocmd
module Buffer = Unshadow.Buffer
module Channel_info = Channel_info
module Client_info = Client_info
module Color = Color
module Command = Unshadow.Command
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

module Nvim_version = struct
  include Semantic_version.Make ()

  let t, api_level, api_compatible =
    match Nvim_internal.api_version with
    | { major; minor; patch; api_level; api_compatible } ->
      let t = of_string [%string "%{major#Int}.%{minor#Int}.%{patch#Int}"] in
      t, api_level, api_compatible
  ;;

  let%expect_test "Display version" =
    print_endline (to_string t);
    [%expect {| 0.9.1 |}];
    return ()
  ;;
end

let pp = Nvim_internal.pp

module Client = struct
  include Client

  module Connection_type = struct
    type 'kind client = 'kind t

    type _ t =
      | Socket :
          [ `Infer_from_parent_nvim | `Address of string ]
          -> [ `asynchronous ] client t
      | Stdio : [ `asynchronous ] client t
      | Embed :
          { prog : string
          ; args : string list
          ; working_dir : string
          ; env : Core_unix.env
          }
          -> ([ `asynchronous ] client * Process.t) t
    [@@deriving sexp_of]
  end

  let create ~name ~on_error =
    let on_error =
      match on_error with
      | `Call f -> f
      | `Raise -> Fn.compose Error.raise Vcaml_error.to_error
    in
    create ~name ~on_error
  ;;

  let attach'
    (type a)
    ?wrap_connection
    ?stdio_override
    ?time_source
    client
    (connection_type : a Connection_type.t)
    : a Deferred.Or_error.t
    =
    let connect reader writer =
      let%bind reader, writer =
        match wrap_connection with
        | None -> return (reader, writer)
        | Some f -> f reader writer
      in
      connect client ?time_source reader writer
    in
    match connection_type with
    | Socket socket ->
      let socket =
        match socket with
        | `Address socket -> socket
        | `Infer_from_parent_nvim -> Sys.getenv_exn "NVIM"
      in
      let%bind reader, writer =
        match Host_and_port.of_string socket with
        | host_and_port ->
          let socket = Tcp.Where_to_connect.of_host_and_port host_and_port in
          let%map _addr, reader, writer = Tcp.connect socket in
          reader, writer
        | exception _ ->
          let socket = Tcp.Where_to_connect.of_file socket in
          let%map _addr, reader, writer = Tcp.connect socket in
          reader, writer
      in
      connect reader writer
    | Stdio ->
      (match stdio_override with
       | None -> connect (force Reader.stdin) (force Writer.stdout)
       | Some (reader, writer) -> connect reader writer)
    | Embed { prog; args; working_dir; env } ->
      (match List.exists args ~f:(String.equal "--embed") with
       | false ->
         Deferred.Or_error.error_s
           [%message
             "Tried to create a VCaml client for an embedded Neovim process, but --embed \
              flag was not passed"
               ~_:(connection_type : _ Connection_type.t)]
       | true ->
         let open Deferred.Or_error.Let_syntax in
         let%bind nvim = Process.create ~prog ~args ~working_dir ~env () in
         let%bind client = connect (Process.stdout nvim) (Process.stdin nvim) in
         return (client, nvim))
  ;;

  let attach client connection_type = attach' client connection_type

  let close client =
    let client = Type_equal.conv Private.eq client in
    client.close ()
  ;;

  let name client =
    let client = Type_equal.conv Private.eq client in
    client.name
  ;;
end

let block_nvim = Client.block_nvim
let block_nvim' = Client.block_nvim'

module Expert = struct
  module Atomic = Atomic
  module Notifier = Notifier
end

module Private = struct
  module Nvim_internal = Nvim_internal
  module Nvim_lock = Nvim_lock
  module Source_code_position = Source_code_position

  let attach_client = Client.attach'

  let notify_nvim_of_error client ~(here : [%call_pos]) error =
    let client = Type_equal.conv Client.Private.eq client in
    client.notify_nvim_of_error ~here error
  ;;

  let before_sending_response_hook_for_tests =
    Client.Private.before_sending_response_hook_for_tests
  ;;
end
