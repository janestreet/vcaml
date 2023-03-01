# VCaml

A library for building Neovim plugins in OCaml.

## Reviewing
Most of this library is a direct port of the neovim remote API.
For review, consult the following help pages:
- `:help api.txt`
- `:help msgpack-rpc`
- `:help --embed`
- `:help --headless`
- `:help --listen`

## Structure
Almost all of the functions in the library are small idiomatic shims on
top of the auto-generated functions produced by `lib/neovim_api_wrapper`
Those that have not been tested are in `Untested` submodules.

### `VCaml`
- Library entrypoint
- API type definitions
  -  OCaml -> Neovim: `Defun.Vim.t`
  -  Neovim -> OCaml: `Defun.Ocaml.t`
  -  These types can't be unified for deep GADT reasons.

### `Api_call`
- A type that represents an API call that can be executed
- A let-syntax for combining these types

### `Nvim`
- Functions that operate on global Neovim state

### `Buffer`
- Functions that operate on Neovim buffers

### `Window`
- Functions that operate on Neovim windows

### `Tabpage`
- Functions that operate on Neovim tabpages

### `Ui`
- Functions that operate on the Neovim UI

### `Client`
- `Client.t` is opaque to library users. You can interact with clients
  via the interface exposed in [Vcaml], but for the most part it is an
  opaque object that is just passed to functions that need it.

### `Client_info`
- `Client_info.t` is some data that is present for every plugin,
  embedder, or UI that is attached to Neovim.

### `Keymap`
- A type for representing key mappings

### `Command`
- A type for representing commands (e.g. ":Foo")

### `Color`
- Types for representing colors and highlights

### `Mark`
- A type for representing a mark (e.g. "'m")

### `Mode`
- A type for representing a mode (e.g. "Normal")

### `Transport`
- Generic code for setting up communication between OCaml
  and Neovim

### `Extract`
- Utilities for pulling values out of `VCaml` related msgpack objects.

### `Notifier`
- Functions that let you *send* Neovim asynchronous notifications.
  In most cases this isn't something you want to do: normally you only
  want to (1) send synchronous requests & responses back and forth, and
  (2) receive asynchronous notifications.
