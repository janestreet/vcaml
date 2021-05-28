open! Core
open! Async
open! Msgpack_debug

let () = Command_unix.run Mitm_debugger.main
