open! Core
open! Async
open! Import
open Vcaml
open Test_client

let with_event_printing ~f =
  (* These tests may be fragile; the nvim docs don't specify exactly how
     [nvim_buf_lines_event] events are grouped or reported. *)
  with_client (fun client ->
    let subscriber = Buffer.Subscriber.create client in
    let%bind events =
      Buffer.Subscriber.subscribe subscriber ~buffer:`Current ~send_buffer:true
    in
    let events = Or_error.ok_exn events in
    Async.don't_wait_for
    @@ Async.Pipe.iter events ~f:(fun event ->
      print_s [%message (event : Buffer.Event.t)];
      Deferred.unit);
    f client)
;;

let%expect_test "initial event received" =
  let%bind () = with_event_printing ~f:(() |> Deferred.Or_error.return |> Fn.const) in
  [%expect
    {|
    (event
     (Lines (buffer 1) (changedtick (2)) (firstline 0) (lastline -1)
      (linedata ("")) (more false))) |}];
  return ()
;;

let%expect_test "events for some edits" =
  let%bind () =
    with_event_printing ~f:(fun client ->
      let open Async.Deferred.Or_error.Let_syntax in
      let feedkeys_call =
        Vcaml.Nvim.feedkeys ~keys:"ohello, world!" ~mode:"nx" ~escape_csi:false
      in
      let%bind () = feedkeys_call |> run_join client in
      return ())
  in
  [%expect
    {|
    (event
     (Lines (buffer 1) (changedtick (2)) (firstline 0) (lastline -1)
      (linedata ("")) (more false)))
    (event
     (Lines (buffer 1) (changedtick (3)) (firstline 1) (lastline 1)
      (linedata ("")) (more false)))
    (event
     (Lines (buffer 1) (changedtick (4)) (firstline 1) (lastline 2)
      (linedata ("hello, world!")) (more false))) |}];
  return ()
;;

let%expect_test "feedkeys, get_lines, events for those edits" =
  let%bind () =
    with_event_printing ~f:(fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () =
        Vcaml.Nvim.feedkeys ~keys:"ihello" ~mode:"nx" ~escape_csi:false
        |> run_join client
      in
      let%bind () =
        Vcaml.Nvim.feedkeys ~keys:"oworld" ~mode:"nx" ~escape_csi:false
        |> run_join client
      in
      let%bind cur_buf = run_join client Vcaml.Nvim.get_current_buf in
      let%map lines =
        Buffer.get_lines ~buffer:cur_buf ~start:0 ~end_:(-1) ~strict_indexing:true
        |> run_join client
      in
      print_s [%message (lines : string list)])
  in
  [%expect
    {|
    (event
     (Lines (buffer 1) (changedtick (2)) (firstline 0) (lastline -1)
      (linedata ("")) (more false)))
    (event
     (Lines (buffer 1) (changedtick (3)) (firstline 0) (lastline 1)
      (linedata (hello)) (more false)))
    (event
     (Lines (buffer 1) (changedtick (4)) (firstline 1) (lastline 1)
      (linedata ("")) (more false)))
    (event
     (Lines (buffer 1) (changedtick (5)) (firstline 1) (lastline 2)
      (linedata (world)) (more false)))
    (lines (hello world))|}];
  return ()
;;

let%expect_test "set_lines, events for those edits" =
  let%bind () =
    with_event_printing ~f:(fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind cur_buf = Vcaml.Nvim.get_current_buf |> run_join client in
      let command =
        let%map.Api_call _set_lines =
          Buffer.set_lines
            ~buffer:cur_buf
            ~start:0
            ~end_:(-1)
            ~strict_indexing:true
            ~replacement:[ "this"; "is"; "an"; "edit" ]
        and get_lines =
          Buffer.get_lines ~buffer:cur_buf ~start:0 ~end_:(-1) ~strict_indexing:true
        in
        get_lines
      in
      let%bind lines = run_join client command in
      print_s [%message (lines : string list)];
      return ())
  in
  [%expect
    {|
    (event
     (Lines (buffer 1) (changedtick (2)) (firstline 0) (lastline -1)
      (linedata ("")) (more false)))
    (event
     (Lines (buffer 1) (changedtick (3)) (firstline 0) (lastline 1)
      (linedata (this is an edit)) (more false)))
    (lines (this is an edit))|}];
  return ()
;;

let%expect_test "find_by_name_or_create no name prefixes" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind original_buf = Vcaml.Nvim.get_current_buf |> run_join client in
      let%bind new_buf =
        Buffer.find_by_name_or_create ~name:"test_buffer_name" |> run_join client
      in
      let%bind original_buf_name =
        Buffer.get_name ~buffer:original_buf |> run_join client
      in
      let%bind found_buf =
        Buffer.find_by_name_or_create ~name:original_buf_name |> run_join client
      in
      print_s
        [%message (original_buf : Buffer.t) (new_buf : Buffer.t) (found_buf : Buffer.t)];
      return ())
  in
  [%expect {| ((original_buf 1) (new_buf 2) (found_buf 1))|}];
  return ()
;;

let%expect_test "find_by_name_or_create name prefixes" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind new_buf =
        Buffer.find_by_name_or_create ~name:"test_buffer_name" |> run_join client
      in
      let%bind new_buf_prefix_name =
        Buffer.find_by_name_or_create ~name:"test_buffer" |> run_join client
      in
      print_s [%message (new_buf : Buffer.t) (new_buf_prefix_name : Buffer.t)];
      return ())
  in
  [%expect {| ((new_buf 2) (new_buf_prefix_name 3)) |}];
  return ()
;;

let%expect_test "find_by_name_or_create buffers with weird characters" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind buf_with_whitespace =
        Buffer.find_by_name_or_create ~name:"  " |> run_join client
      in
      let%bind buf_with_slash =
        Buffer.find_by_name_or_create ~name:"\\" |> run_join client
      in
      let%bind buf_with_quotes =
        Buffer.find_by_name_or_create ~name:"\"\"" |> run_join client
      in
      print_s
        [%message
          (buf_with_whitespace : Buffer.t)
            (buf_with_slash : Buffer.t)
            (buf_with_quotes : Buffer.t)];
      return ())
  in
  [%expect {|
    ((buf_with_whitespace 2) (buf_with_slash 3) (buf_with_quotes 4)) |}];
  return ()
;;

let%expect_test "set_option" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind buffer = Vcaml.Nvim.get_current_buf |> run_join client in
      let%bind.Deferred modify_success =
        Buffer.set_lines
          ~buffer
          ~start:0
          ~end_:(-1)
          ~strict_indexing:false
          ~replacement:[ "foo!" ]
        |> run_join client
      in
      print_s [%message (modify_success : unit Or_error.t)];
      let%bind () =
        Buffer.set_option
          ~buffer
          ~scope:`Local
          ~name:"modifiable"
          ~type_:Boolean
          ~value:false
        |> run_join client
      in
      let%bind.Deferred modify_error =
        Buffer.set_lines
          ~buffer
          ~start:0
          ~end_:(-1)
          ~strict_indexing:false
          ~replacement:[ "bar!" ]
        |> run_join client
      in
      print_s [%message (modify_error : unit Or_error.t)];
      return ())
  in
  [%expect
    {|
    (modify_success (Ok ()))
    (modify_error
     (Error ("Vim returned error" "Failed to save undo information"))) |}];
  return ()
;;
