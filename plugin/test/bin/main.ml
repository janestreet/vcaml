module Plugin = Vcaml_plugin.Oneshot.Make (struct
    let name = "vcaml-test-oneshot-plugin"
    let on_error = `Raise

    let bufnr =
      Vcaml_plugin.Oneshot.Rpc.create
        ~name:"bufnr"
        ~type_:Vcaml.Defun.Ocaml.Sync.(return Buffer)
        ~f:(fun ~keyboard_interrupted:_ ~client ->
          Vcaml.run_join [%here] client Vcaml.Nvim.get_current_buf)
    ;;

    let rpc_handlers = [ bufnr ]
  end)

let () = Command_unix.run (Plugin.command ~summary:"Test Oneshot RPC")
