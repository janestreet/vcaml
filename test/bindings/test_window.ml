open Core
open Async
open Vcaml
open Vcaml_test_helpers

let%expect_test "get_height, set_height" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = Window.set_height Current ~height:10 |> run_join [%here] client in
      let%bind height = Window.get_height Current |> run_join [%here] client in
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
      let%bind () =
        Nvim.feedkeys (`Escape_k_special_bytes "ithisisatest") ~mode:"n"
        |> run_join [%here] client
      in
      let%bind { row; col } = Window.get_cursor Current |> run_join [%here] client in
      print_s [%message (row : int) (col : int)];
      let%bind () =
        Window.set_cursor Current { row = 1; col = 5 } |> run_join [%here] client
      in
      let%bind position = Window.get_cursor Current |> run_join [%here] client in
      print_s [%sexp (position : Position.One_indexed_row.t)];
      return ())
  in
  [%expect {|
    ((row 1) (col 11))
    ((row 1) (col 5)) |}];
  return ()
;;
