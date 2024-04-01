## Release v0.17.0

This release brings major changes to VCaml. It is compatible with Neovim 0.9.1.

- New: Updated API bindings for Neovim 0.9.1.
- New: Feature parity for Lua in VCaml.
- New: Updated VCaml examples to be more instructive for plugin development.
- New: `block_nvim` function that will block Neovim for the duration of the callback.
- New: API calls no longer need to be batched. Use `block_nvim` to block Neovim.
- New: Fail API calls in blocking callbacks that have been keyboard-interrupted.
- New: `run_in_background` function provided to blocking callbacks to kick off background jobs that talk to Neovim.
- New: Improved typing for many API bindings, including a new GADT for modeling options.
- New: API support for checking `b:changedtick` on asynchronous buffer updates.
- New: Support TCP connections to Neovim.
- New: Tests for many more API functions.
- New: Property-based tests for blocking client semantics.
- Cleanup: Defunctorized and simplified the `vcaml.plugin` interface.
- Removed: `shutdown` argument in persistent plugins (use `shutdown` from Async to shut down).
- Removed: mli-cycler plugin (use OCaml-LSP for cycling).
- Removed: `Api_call.t` applicative that was used for batching.
- Removed: `Luaref`.
- Made [Ui_event.t] forwards-compatible.
- VCaml buffer text is now represented as [String.Utf8.t]

## Release v0.16.0

This release has minor changes from v0.15.0 and is compatible with Neovim 0.7.0. It is known to be incompatible with Neovim 0.7.2+. The next release will have extensive changes and will be compatible with Neovim 0.9.0.

- New: Updated API bindings for Neovim 0.7.0.
- New: `Or_current` submodules have been added to `Buffer`, `Window` and `Tabpage` to indicate the current buffer, window, or tab respectively.
- New: The man-in-the-middle debugger has been rewritten to be more useful and intuitive.
- New: `Keymap.set` now takes `description` and `unique` arguments.
- Cleanup: Misc. changes to the API functions to improve naming & ergonomics.
- Warn: The addition of `Luaref` where present is a mistake and should not be used. These references will be removed in the next version of VCaml, which will also interact better with Lua.

## Release v0.15.0

This release is compatible with Neovim 0.5.0.

- New: Automatic handling of keyboard interrupts.
- New: `vcaml.plugin` library as framework for writing plugins with common patterns.
- New: Examples to demonstrate usage of vcaml.plugin library.
- New: Blocking callbacks are allowed to use Async.
- New: Errors that occur in VCaml callbacks are displayed in Neovim.
- New: Support for typing VimL vararg functions.
- New: `Nvim.echo_in_rpcrequest` hack to support displaying messages while Neovim is blocked.
- New: Top-level debugger for driving Neovim from utop.
- New: Support for drawing Neovim in tests.
- New: Support for sending notifications to Neovim in addition to requests.
- New: Msgpack man-in-the-middle debugger.
- New: Mli-cycler plugin for cycling between ml and mli files.
- Cleanup: Misc. changes to the API functions and types in the API to improve types & ergonomics.
- Fix: Broken behavior in `Buffer.find_by_name_or_create` when passed empty string - it now creates a new buffer instead of returning the current one.
- Fix: Buffer subscription race that could lead to missed events.
- Fix: Sharp corner where registering an async handler could just run the handler at registration time and register the result.
- Fix: Bug where passing too many arguments would still run the function even though the error would be reported.
- Fix: RPC layer robust to write failures.

## Release v0.14.0

No changes.

## Release v0.13.0

Initial release of VCaml.
