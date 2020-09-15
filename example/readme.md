# Examples

Basic examples of various types of Vcaml plugins.

## Running

Each example has a file containing vimscript which can be sourced to run the
plugin. You must first build the ocaml.

### Hello World
A "one-shot" plugin which echoes Hello world! in the command line.

### Greetings
An RPC plugin which creates a long-running process and communicates greetings
back over RPC.

### Buffer Clock
A buffer-managing plugin which creates a new window containing a clock with the
current time, and which runs until the buffer is deleted.

### Simple Editor
A buffer-managing-plugin which implements a tiny text editor using 
rebound keys in normal mode!  The example demonstrates how to bind keys, 
and how to respond to RPCs sent by the vim instance.

To run it, open up a new neovim terminal (`:term`), and run
`simple_editor/bin/main.exe`.
