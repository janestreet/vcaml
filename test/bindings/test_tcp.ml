open Core
open Async
open Vcaml
open Vcaml_test_helpers

let%expect_test "connecting over TCP works" =
  let%bind () =
    with_client (fun client ->
      let%bind address =
        Nvim.call_function
          client
          ~name:(`Viml "serverstart")
          ~type_:Nvim.Func.(String @-> return String)
          "127.0.0.1:0"
        >>| ok_exn
      in
      let client = Client.create ~name:"test-tcp-client" ~on_error:`Raise in
      let%bind client = Client.attach client (Socket (`Address address)) >>| ok_exn in
      let%bind () =
        Nvim.exec_viml_and_capture_output client "echo 'TCP client works!'"
        >>| ok_exn
        >>| print_endline
      in
      let%bind () = Client.close client in
      Deferred.Or_error.return ())
  in
  [%expect {| TCP client works! |}];
  return ()
;;
