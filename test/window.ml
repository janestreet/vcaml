open! Core_kernel
open! Async
open! Import
open! Vcaml
open Test_client

let%expect_test "get_height, set_height" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind win = Vcaml.Nvim.get_current_win |> run_join client in
      let%bind () = Window.set_height ~window:win ~height:10 |> run_join client in
      let%bind height = Window.get_height ~window:win |> run_join client in
      print_s [%message (height : int)];
      return ())
  in
  [%expect "(height 10)"];
  return ()
;;

let%expect_test "get_cursor, set_cursor" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind win = Vcaml.Nvim.get_current_win |> run_join client in
      let%bind () =
        Vcaml.Nvim.feedkeys ~keys:"ithisisatest" ~mode:"n" ~escape_csi:false
        |> run_join client
      in
      let%bind { row; col } = Vcaml.Window.get_cursor ~window:win |> run_join client in
      print_s [%message (row : int) (col : int)];
      let%bind () =
        Vcaml.Window.set_cursor ~window:win ~row:1 ~col:5 |> run_join client
      in
      let%bind { row; col } = Vcaml.Window.get_cursor ~window:win |> run_join client in
      print_s [%message (row : int) (col : int)];
      return ())
  in
  [%expect {|
    ((row 1) (col 11))
    ((row 1) (col 5)) |}];
  return ()
;;
