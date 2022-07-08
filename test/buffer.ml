open! Core
open! Async
open! Import
open Vcaml
open Test_client

let with_event_printing ~f =
  (* These tests may be fragile; the nvim docs don't specify exactly how
     [nvim_buf_lines_event] events are grouped or reported. *)
  with_client (fun client ->
    let subscriber = Buffer.Subscriber.create client ~on_parse_error:`Raise in
    let%bind events =
      Buffer.Subscriber.subscribe subscriber [%here] ~buffer:Current ~send_buffer:true
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
        Vcaml.Nvim.feedkeys (`Escape_k_special_bytes "ohello, world!") ~mode:"nx"
      in
      let%bind () = feedkeys_call |> run_join [%here] client in
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
        Vcaml.Nvim.feedkeys (`Escape_k_special_bytes "ihello") ~mode:"nx"
        |> run_join [%here] client
      in
      let%bind () =
        Vcaml.Nvim.feedkeys (`Escape_k_special_bytes "oworld") ~mode:"nx"
        |> run_join [%here] client
      in
      let%map lines =
        Buffer.get_lines Current ~start:0 ~end_:(-1) ~strict_indexing:true
        |> run_join [%here] client
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
      let command =
        let%map.Api_call _set_lines =
          Buffer.set_lines
            Current
            ~start:0
            ~end_:(-1)
            ~strict_indexing:true
            ~replacement:[ "this"; "is"; "an"; "edit" ]
        and get_lines =
          Buffer.get_lines Current ~start:0 ~end_:(-1) ~strict_indexing:true
        in
        get_lines
      in
      let%bind lines = run_join [%here] client command in
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

let%expect_test "create, set_name, get_name" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind buffer =
        Vcaml.Buffer.create ~listed:true ~scratch:false |> run_join [%here] client
      in
      let%bind () =
        Vcaml.Buffer.set_name (Id buffer) ~name:"foobar" |> run_join [%here] client
      in
      let%bind name = Vcaml.Buffer.get_name (Id buffer) |> run_join [%here] client in
      print_s [%message (buffer : Buffer.t) (name : string)];
      return ())
  in
  [%expect {| ((buffer 2) (name ${TMPDIR}/foobar))|}];
  return ()
;;

let%expect_test "find_by_name_or_create no name prefixes" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind original_buf = Vcaml.Nvim.get_current_buf |> run_join [%here] client in
      let original_buf_name = "original_buffer_name" in
      let%bind () =
        Buffer.set_name (Id original_buf) ~name:original_buf_name
        |> run_join [%here] client
      in
      let%bind new_buf =
        Buffer.find_by_name_or_create ~name:"new_buffer_name" |> run_join [%here] client
      in
      let%bind found_buf =
        Buffer.find_by_name_or_create ~name:original_buf_name |> run_join [%here] client
      in
      print_s
        [%message (original_buf : Buffer.t) (new_buf : Buffer.t) (found_buf : Buffer.t)];
      return ())
  in
  [%expect {|
         ((original_buf 1) (new_buf 2) (found_buf 1))|}];
  return ()
;;

let%expect_test "find_by_name_or_create name prefixes" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind new_buf =
        Buffer.find_by_name_or_create ~name:"test_buffer_name"
        |> run_join [%here] client
      in
      let%bind new_buf_prefix_name =
        Buffer.find_by_name_or_create ~name:"test_buffer" |> run_join [%here] client
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
        Buffer.find_by_name_or_create ~name:"  " |> run_join [%here] client
      in
      let%bind buf_with_slash =
        Buffer.find_by_name_or_create ~name:"\\" |> run_join [%here] client
      in
      let%bind buf_with_quotes =
        Buffer.find_by_name_or_create ~name:"\"\"" |> run_join [%here] client
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

let%expect_test "get_option and set_option" =
  Backtrace.elide := true;
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let print_modifiability () =
        Buffer.get_option Current ~name:"modifiable" ~type_:Boolean
        |> run_join [%here] client
        >>| fun modifiable -> print_s [%message (modifiable : bool)]
      in
      let%bind () = print_modifiability () in
      let%bind.Deferred modify_success =
        Buffer.set_lines
          Current
          ~start:0
          ~end_:(-1)
          ~strict_indexing:false
          ~replacement:[ "foo!" ]
        |> run_join [%here] client
      in
      print_s [%message (modify_success : unit Or_error.t)];
      let%bind () =
        Buffer.set_option
          Current
          ~scope:`Local
          ~name:"modifiable"
          ~type_:Boolean
          ~value:false
        |> run_join [%here] client
      in
      let%bind () = print_modifiability () in
      let%bind.Deferred modify_error =
        Buffer.set_lines
          Current
          ~start:0
          ~end_:(-1)
          ~strict_indexing:false
          ~replacement:[ "bar!" ]
        |> run_join [%here] client
      in
      print_s [%message (modify_error : unit Or_error.t)];
      return ())
  in
  [%expect
    {|
    (modifiable true)
    (modify_success (Ok ()))
    (modifiable false)
    (modify_error
     (Error
      (("Called from" lib/vcaml/test/buffer.ml:LINE:COL)
       ("Vim returned error" "Buffer is not 'modifiable'" (error_type Exception))))) |}];
  return ()
;;

let%expect_test "get_mark" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind escaped_keys =
        Vcaml.Nvim.replace_termcodes_and_keycodes "ihello<Esc>mma\nworld!<Esc>"
        |> run_join [%here] client
      in
      let%bind () =
        Vcaml.Nvim.feedkeys (`Already_escaped escaped_keys) ~mode:"n"
        |> run_join [%here] client
      in
      let%bind mark = Buffer.get_mark Current ~sym:'m' |> run_join [%here] client in
      print_s [%sexp (mark : Mark.t)];
      return ())
  in
  (* Demonstrate that marks are (1-indexed row, 0-indexed col). *)
  [%expect {| ((sym m) (pos ((row 1) (col 4)))) |}];
  return ()
;;

let%expect_test "Extmark indexing" =
  with_client (fun client ->
    let open Deferred.Or_error.Let_syntax in
    let%bind () =
      Vcaml.Nvim.Untested.set_option "syntax" ~type_:String ~value:"on"
      |> run_join [%here] client
    in
    let%bind escaped_keys =
      Vcaml.Nvim.replace_termcodes_and_keycodes "ihello\nworld!<Esc>"
      |> run_join [%here] client
    in
    let%bind () =
      Vcaml.Nvim.feedkeys (`Already_escaped escaped_keys) ~mode:"n"
      |> run_join [%here] client
    in
    let%bind namespace = Namespace.Untested.create () |> run_join [%here] client in
    let%bind extmark =
      (* Start the extmark range on the 'o' of hello and end on the 'w' of world,
         inclusive. *)
      Buffer.Untested.create_extmark
        Current
        ~namespace
        ~start_inclusive:{ row = 0; col = 4 }
        ~end_exclusive:{ row = 1; col = 1 }
        ()
      |> run_join [%here] client
    in
    let%bind result =
      Buffer.Untested.get_extmark_with_details extmark |> run_join [%here] client
    in
    print_s [%sexp (result : (Position.t * Msgpack.t String.Map.t) option)];
    (* It seems the ([end_row], [end_col]) position in the details dictionary is 0-based
       inclusive (row, col). *)
    [%expect
      {|
      ((((row 0) (col 4))
        ((end_col (Integer 1)) (end_right_gravity (Boolean false))
         (end_row (Integer 1)) (right_gravity (Boolean true))))) |}];
    let%bind result =
      Buffer.Untested.all_extmarks
        Current
        ~namespace
        ~start_inclusive:{ row = 0; col = 0 }
        ~end_inclusive:{ row = 0; col = 3 }
        ()
      |> run_join [%here] client
    in
    print_s [%sexp (result : (_ * Position.t) list)];
    [%expect {| () |}];
    let%bind result =
      Buffer.Untested.all_extmarks
        Current
        ~namespace
        ~start_inclusive:{ row = 1; col = 1 }
        ~end_inclusive:{ row = 2; col = 0 }
        ()
      |> run_join [%here] client
    in
    print_s [%sexp (result : (_ * Position.t) list)];
    [%expect {| () |}];
    let%bind result =
      Buffer.Untested.all_extmarks
        Current
        ~namespace
        ~start_inclusive:{ row = 0; col = 0 }
        ~end_inclusive:{ row = 2; col = 0 }
        ()
      |> run_join [%here] client
    in
    print_s [%sexp (result : (_ * Position.t) list)];
    [%expect {| ((_ ((row 0) (col 4)))) |}];
    return ())
;;
