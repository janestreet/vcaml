VCaml
=====

VCaml is a library for building Neovim plugins in OCaml. Here's an overview of the
directory structure:

```
vcaml
|-- src     -- The `vcaml` library: typed primitives for interacting with Neovim.
|-- plugin  -- The `vcaml.plugin` library: high-level wrappers for common use-cases.
|              For usage examples, see the "example" and "templates" subdirectories.
|-- test    -- Tests of VCaml. The `vcaml.test.helpers` library has functions that make
|              testing VCaml plugins easy.
`-- debug   -- The `vcaml.debug` library: meant to be used from an OCaml top-level to
               manually step through an interaction with a Neovim instance. Also includes
               a binary for logging communication between a plugin and Neovim.
```

# The `vcaml.plugin` library

This library provides helpers for two common types of plugins: "oneshot" plugins that
model a single call from Neovim to OCaml, and "persistent" plugins that remain alive in
the background and field RPC requests. Most of the time you will want to use this library
instead of creating a `Vcaml.Client.t` manually.

# The `vcaml` library

The entrypoint to this library is `vcaml.mli`. For calling Neovim API functions, you
should look at the following modules:

- `Buffer`  - Functions for interacting with buffers
- `Window`  - Functions for interacting with windows
- `Tabpage` - Functions for interacting with tabs
- `Keymap`  - Functions for interacting with key mappings
- `Command` - Functions for interacting with commands
- `Autocmd` - Functions for interacting with auto-commands (logic that runs on events)
- `Ui`      - Functions for attaching and detaching a UI
- `Nvim`    - Miscellaneous functions for interacting with Neovim

In general, the API function `nvim_buf_foo` will be `Buffer.foo`; `nvim_foo` will be
`Nvim.foo`. In some cases functions are renamed or exported differently for clarity. If
you are struggling to determine how a function documented in the API is exported in VCaml,
you can grep VCaml for the function name, which will appear in the implementation of
whichever function(s) export(s) it.

To call Neovim atomically, use `Vcaml.block_nvim`. Neovim will remain blocked for the
duration of the callback, and only commands used with the `Client.t` provided to the
callback will be privileged to run during that time.

To model calls from Neovim to OCaml made with `rpcrequest` and `rpcnotify`, see the
`Ocaml_from_nvim` module. If you are using `vcaml.plugin`, you will not need to call the
registration functions yourself.

If you are not using `vcaml.plugin`, you should use `vcaml` by first creating a
`Client.t`, then register RPCs you want available with the registration functions in
`Ocaml_from_nvim`, then calling `Client.attach`, and finally calling `Client.close` when
you are finished with the plugin (or call `exit` to shut down the program).

# Testing with the `vcaml.test.helpers` library

The `vcaml.test.helpers` library has useful helper functions for writing VCaml expect
tests. They take care of spawning an embedded Neovim instance during the test so you can
just write logic with a connected VCaml client. See the plugin tests in plugin/example for
example usage.

# Additional debugging tools

## Debugging Process Communication

In the ./debug/bin directory there is a binary for debugging interaction between two
Msgpack RPC peers. It has modes for debugging communication both over stdio and over unix
domain sockets. See the CLI help for usage information.

## Manual RPC Interaction (REPL)

If you want to experiment with Neovim's semantics, it's often more effective to construct
and send RPC messages manually. If you don't need any code for your experiment, you can
just do this with two vim instances. Get the server name from the target vim instance by
running `:echo v:servername` (you can also do `:let @+ = v:servername` to copy the name
directly into your clipboard). Then in the instance you're using for debugging, run
`let socket = sockconnect("pipe", "servername", { "rpc": 1 })` where "servername" is the
servername you copied. Now just use `rpcrequest` and `rpcnotify` with that socket.

You can also manually drive Neovim from the OCaml top-level using the `vcaml.debug`
library. Build the top-level for `vcaml.debug` and follow along below. For `socket`, use
the servername copied from Neovim.

<!-- Set up the example
```ocaml
let example = Vcaml_readme.setup ()
let socket = Vcaml_readme.socket example;;
```
-->

```ocaml
# module V = Vcaml_debug.Toplevel_client
module V = Vcaml_debug.Toplevel_client
# #install_printer V.pp
# let nvim = V.open_ socket
val nvim : V.t = <abstr>
# V.verbose nvim true
- : unit = ()
# V.request nvim "nvim_exec2" [ String "echo 'hi'"; Map [] ]
OCaml -> Nvim:
[ 0, 2, "nvim_exec2", [ "echo 'hi'", {} ] ]
- : unit = ()
```
If you are following these steps manually with a running Neovim instance, at this point
you should see "hi" echoed in Neovim. To interpret the messages that are being sent and
received, see https://github.com/msgpack-rpc/msgpack-rpc/blob/master/spec.md. The reason
the request ID begins at 2 is that `vcaml.debug` always sends an initial request for the
channel.
```ocaml
# V.receive nvim
Nvim -> OCaml: [ 1, 2, nil, {} ]
- : [ `Connection_closed | `Message of Msgpack.t | `Waiting_for_neovim ] =
`Message [ 1, 2, nil, {} ]
# V.request nvim "nvim_exec2" [ String "quit"; Map [] ]
OCaml -> Nvim: [ 0, 3, "nvim_exec2", [ "quit", {} ] ]
- : unit = ()
# V.close nvim
- : unit = ()
```

<!-- Tear down the example
```ocaml
Vcaml_readme.teardown example;;
```
-->
