open! Core
open Vcaml

let say_hello =
  Vcaml_plugin.Oneshot.Rpc.create
    "hello"
    ~type_:Ocaml_from_nvim.Blocking.(String @-> return Nil)
    ~f:(fun ~client name -> Nvim.out_writeln client [%string "Hello, %{name}!"])
;;

(** This is an example of a "oneshot" plugin - the process is spawned to handle a single
    RPC request invoked via [rpcrequest]. *)
let command =
  Vcaml_plugin.Oneshot.create
    ~name:"hello-world"
    ~description:"Oneshot plugin to say hello"
    [ say_hello ]
;;
