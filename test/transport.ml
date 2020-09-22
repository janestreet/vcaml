open! Core
open! Async
open Vcaml
open Test_client

let register_dummy_rpc_handler ~name client =
  Deferred.return
    (Vcaml.register_request_blocking
       client
       ~name
       ~type_:Defun.Ocaml.Sync.(Type.Nil @-> return Type.Nil)
       ~f:(fun () -> Or_error.return ()))
;;

let%expect_test "we can have two separate Embedded connections with RPC handlers sharing \
                 names without error (no bleeding state)"
  =
  let%bind () = with_client (register_dummy_rpc_handler ~name:"test") in
  let%map () = with_client (register_dummy_rpc_handler ~name:"test") in
  [%expect {| |}]
;;
