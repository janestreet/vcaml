# MessagePack Debugger

The purpose of this program is to assist in debugging MessagePack unix sockets (e.g. the
connection between neovim and a VCaml plugin). The debugger acts as a man-in-the-middle,
printing the messages that are being sent back and forth and forwarding them on.

## Usage

First, build the OCaml code in `bin/`, to produce `bin/main.exe`. The executable takes
three command-line arguments:

 * `-socket`: the name of the pre-existing unix socket that the debugger should act as a man-in-the-middle for
 * `-cmd`: a string corresponding to the program that should be run (this program should connect to the aforementioned socket)
 * `-env_variable`: the environment variable that will contain the new socket endpoint for the program to connect to

For example, to debug a VCaml plugin called `my_plugin.exe` which connects to a unix socket contained in `$NVIM_LISTEN_ADDRESS`, run:

```
./bin/main.exe -socket $NVIM_LISTEN_ADDRESS -cmd "my_plugin.exe" -env_variable NVIM_LISTEN_ADDRESS
```
