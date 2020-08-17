open! Core_kernel
open! Async
open! Import
open! Vcaml
open Test_client

let%expect_test "get_height, set_height" =
  let%bind () =
    with_client ~f:(fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind win = run_join client Vcaml.Client.get_current_win in
      let%bind () = run_join client (Window.set_height ~window:win ~height:10) in
      let%bind height = run_join client (Window.get_height ~window:win) in
      print_s [%message (height : int)];
      return ())
  in
  [%expect "(height 10)"];
  return ()
;;
