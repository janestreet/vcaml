## Release v0.16.0

This release has minor changes from v0.15.0 and is compatible with Neovim 0.7.0. It is
known to be incompatible with Neovim 0.7.2+. The next release will have extensive changes
and will be compatible with Neovim 0.9.0.

Changes in this release:

- Some new bindings have been added for Neovim 0.7.0.
- The man-in-the-middle debugger has been rewritten to be more useful and intuitive.
- `Or_current` submodules have been added to `Buffer`, `Window` and `Tabpage` to indicate
  the current buffer, window, or tab respectively.
- `Keymap.set` now takes `description` and `unique` arguments.
- Some functions have been renamed to be clearer, and are made more ergonomic in cases
  where verbosity did not improve readability.
- The addition of `Luaref` where present is a mistake and should not be used. These
  references will be removed in the next version of VCaml, which will also interact better
  with Lua.
