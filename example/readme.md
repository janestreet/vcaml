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
A buffer plugin which creates a new window containing a clock with the current
time, and which runs until the buffer is deleted.

