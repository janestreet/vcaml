let () = Command_unix.run (Msgpack_debug.command ~pp:Vcaml.pp ())
