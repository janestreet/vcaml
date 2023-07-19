# VCaml Examples

This directory has some usage examples for VCaml. You may also want to take a look at the
../templates directory, which has some templates for writing VimL for VCaml plugins.

For an overview of the VCaml libraries, see ../../README.mdx.

VCaml plugins are launched from within Neovim. Each example plugin has a Lua file that
should be sourced to run the example. Make sure to source the file with `:so %` since it
uses `<sfile>` to locate the plugin binary, which will not work properly with
`:so` (no arguments). The plugin examples are ordered below by complexity.

# Hello World
A plugin that provides a `:SayHello` command that takes a name as an argument and says
"Hello, name!"

# Hello World 2
Same as the above plugin, but stays alive to serve multiple RPC requests. It provides a
`:SayGoodbye` command to shut down the plugin.

# Buffer Clock
A plugin that opens a window with a clock displaying the current time. The plugin shuts
down when the window is closed.

# Hangman
A plugin that provides a `:Hangman` command to start a game of Hangman. One player enters
a secret phrase and the other player tries to guess it. In the game window, the lowercase
letter keys are remapped to guess letters. As the player guesses correctly, letters will
be filled in. As the player guesses incorrectly, an image will begin to appear. The plugin
shuts down when the window is closed.
