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
Almost all of the functions in the library are small idiomatic shims on top of
the auto-generated functions produced by `lib/neovim_api_wrapper`
Those that have not been tested are in `Untested` submodules.

### `Client`
- Functions for connecting-to and spawning Neovim instances
- Functions that operate on the Neovim connection

### `Buf`
- Functions that operate on Neovim buffers

### `Window`
- Functions that operate on Neovim windows

### `Tabpage`
- Functions that operate on Neovim tabpages

### `Client_info`
- `Client_info` is some data that is present for every plugin,
  embedder, or UI that is attached to Neovim.

### `Api_call`
- A type that represents an un-run api call
- A let-syntax for combining these types

### `Keymap`
- A type for representing key mappings

### `Command`
- A type for representing commands (e.g. ":Foo")

### `Transport`
- Generic code for setting up communication between OCaml 
  and Neovim

### `Types`
- Mostly for avoiding module cycles
- Defines some conversion functions and a Phantom type for
  GADT matching.

### `Extract`
- Utilities for pulling values out of `VCaml` related msgpack objects.

### `VCaml`
- Library entrypoint 
- API type definitions 
  -  OCaml -> Neovim: `Defun.Vim.t`
  -  Neovim -> OCaml: `Defun.Ocaml.t`
  -  These types can't be unified for deep GADT reasons.
