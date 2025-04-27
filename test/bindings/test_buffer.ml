open Core
open Async
open Vcaml
open Vcaml_test_helpers

let with_event_printing ~f =
  (* These tests may be fragile; the nvim docs don't specify exactly how
     [nvim_buf_lines_event] events are grouped or reported. *)
  with_client (fun client ->
    let%bind events = Buffer.subscribe client Current >>| ok_exn in
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
     (Lines (changedtick (2)) (firstline 0) (lastline -1) (linedata (""))
      (more false)))
    |}];
  return ()
;;

let%expect_test "events for some edits" =
  let%bind () =
    with_event_printing ~f:(fun client ->
      Nvim.feedkeys client (`Raw "ohello, world!") ~mode:"nx")
  in
  [%expect
    {|
    (event
     (Lines (changedtick (2)) (firstline 0) (lastline -1) (linedata (""))
      (more false)))
    (event
     (Lines (changedtick (3)) (firstline 1) (lastline 1) (linedata (""))
      (more false)))
    (event
     (Lines (changedtick (4)) (firstline 1) (lastline 2)
      (linedata ("hello, world!")) (more false)))
    |}];
  return ()
;;

let%expect_test "feedkeys, get_lines, events for those edits" =
  let%bind () =
    with_event_printing ~f:(fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = Nvim.feedkeys client (`Raw "ihello") ~mode:"nx" in
      let%bind () = Nvim.feedkeys client (`Raw "oworld") ~mode:"nx" in
      let%map lines =
        Buffer.get_lines client Current ~start:0 ~end_:(-1) ~strict_indexing:true
      in
      print_s [%message (lines : String.Utf8.t list Buffer.With_changedtick.t)])
  in
  [%expect
    {|
    (event
     (Lines (changedtick (2)) (firstline 0) (lastline -1) (linedata (""))
      (more false)))
    (event
     (Lines (changedtick (3)) (firstline 0) (lastline 1) (linedata (hello))
      (more false)))
    (event
     (Lines (changedtick (4)) (firstline 1) (lastline 1) (linedata (""))
      (more false)))
    (event
     (Lines (changedtick (5)) (firstline 1) (lastline 2) (linedata (world))
      (more false)))
    (lines ((value (hello world)) (changedtick 5)))
    |}];
  return ()
;;

let%expect_test "set_lines, events for those edits" =
  let%bind () =
    with_event_printing ~f:(fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () =
        Buffer.set_lines
          client
          Current
          ~start:0
          ~end_:(-1)
          ~strict_indexing:true
          [ "this"; "is"; "an"; "edit" ]
      in
      let%bind lines =
        Buffer.get_lines client Current ~start:0 ~end_:(-1) ~strict_indexing:true
      in
      print_s [%message (lines : String.Utf8.t list Buffer.With_changedtick.t)];
      return ())
  in
  [%expect
    {|
    (event
     (Lines (changedtick (2)) (firstline 0) (lastline -1) (linedata (""))
      (more false)))
    (event
     (Lines (changedtick (3)) (firstline 0) (lastline 1)
      (linedata (this is an edit)) (more false)))
    (lines ((value (this is an edit)) (changedtick 3)))
    |}];
  return ()
;;

let%expect_test "changedtick, get_text, set_text" =
  Dynamic.set_root Backtrace.elide true;
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind changedtick = Buffer.get_changedtick client Current in
      print_s [%message (changedtick : Buffer.changedtick)];
      let%bind () =
        Buffer.set_text
          client
          Current
          ~changedtick
          ~start_row:0
          ~start_col:0
          ~end_row:0
          ~end_col:0
          [ "test" ]
      in
      let prevtick = changedtick in
      let%bind changedtick = Buffer.get_changedtick client Current in
      print_s [%message (changedtick : Buffer.changedtick)];
      let%bind () =
        match%map.Deferred
          Buffer.set_text
            client
            Current
            ~changedtick:prevtick
            ~start_row:0
            ~start_col:0
            ~end_row:0
            ~end_col:0
            [ "replaced" ]
        with
        | Error error ->
          print_s [%message (error : Error.t)];
          Ok ()
        | Ok () ->
          Or_error.error_s
            [%message "Unexpectedly got [Ok ()] even when changedtick is too old"]
      in
      let%bind text_and_changedtick =
        Buffer.get_text client Current ~start_row:0 ~start_col:0 ~end_row:0 ~end_col:80
      in
      print_s
        [%sexp (text_and_changedtick : String.Utf8.t list Buffer.With_changedtick.t)];
      return ())
  in
  [%expect
    {|
    (changedtick 2)
    (changedtick 3)
    (error
     (("One of the calls in the nvim_call_atomic batch failed"
       (index_of_failure 0) (error_type Exception)
       "nvim_exec2(): Vim(echoerr):Buffer updated since changedtick (wanted 2 but is now 3)")
      (("Called from" lib/vcaml/test/bindings/test_buffer.ml:LINE:COL))))
    ((value (test)) (changedtick 3))
    |}];
  Dynamic.set_root Backtrace.elide false;
  return ()
;;

let%expect_test "create, set_name, get_name" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind buffer = Buffer.create client ~listed:true ~scratch:false in
      let%bind () = Buffer.set_name client (Id buffer) "foobar" in
      let%bind name = Buffer.get_name client (Id buffer) in
      print_s [%message (buffer : Buffer.t) (name : string)];
      return ())
  in
  [%expect {| ((buffer 2) (name ${TMPDIR}/foobar)) |}];
  return ()
;;

let%expect_test "find_by_name_or_create no name prefixes" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind original_buf = Nvim.get_current_buf client in
      let original_buf_name = "original_buffer_name" in
      let%bind () = Buffer.set_name client (Id original_buf) original_buf_name in
      let%bind new_buf = Buffer.find_by_name_or_create client "new_buffer_name" in
      let%bind found_buf = Buffer.find_by_name_or_create client original_buf_name in
      print_s
        [%message (original_buf : Buffer.t) (new_buf : Buffer.t) (found_buf : Buffer.t)];
      return ())
  in
  [%expect {| ((original_buf 1) (new_buf 2) (found_buf 1)) |}];
  return ()
;;

let%expect_test "find_by_name_or_create name prefixes" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind new_buf = Buffer.find_by_name_or_create client "test_buffer_name" in
      let%bind new_buf_prefix_name = Buffer.find_by_name_or_create client "test_buffer" in
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
      let%bind buf_with_whitespace = Buffer.find_by_name_or_create client "  " in
      let%bind buf_with_slash = Buffer.find_by_name_or_create client "\\" in
      let%bind buf_with_quotes = Buffer.find_by_name_or_create client "\"\"" in
      print_s
        [%message
          (buf_with_whitespace : Buffer.t)
            (buf_with_slash : Buffer.t)
            (buf_with_quotes : Buffer.t)];
      return ())
  in
  [%expect {| ((buf_with_whitespace 2) (buf_with_slash 3) (buf_with_quotes 4)) |}];
  return ()
;;

let%expect_test "API for buffer-local options" =
  with_client (fun client ->
    let%bind buffer = Nvim.get_current_buf client >>| ok_exn in
    let print_modifiability buffer =
      let%bind modifiable = Buffer.Option.get client buffer Modifiable >>| ok_exn in
      let%map actually_modifiable =
        Buffer.set_lines
          client
          buffer
          ~start:0
          ~end_:(-1)
          ~strict_indexing:true
          [ "foo!" ]
        >>| Or_error.is_ok
      in
      print_s [%message (modifiable : bool) (actually_modifiable : bool)]
    in
    let print_new_buffer_modifiability () =
      let%map new_buffers_are_modifiable =
        Buffer.Option.get_for_new_buffers client Modifiable >>| ok_exn
      in
      print_s [%message (new_buffers_are_modifiable : bool)]
    in
    let%bind () = print_modifiability Current in
    [%expect {| ((modifiable true) (actually_modifiable true)) |}];
    let%bind () = Buffer.Option.set client Current Modifiable false >>| ok_exn in
    let%bind () = print_modifiability Current in
    [%expect {| ((modifiable false) (actually_modifiable false)) |}];
    let%bind () = Buffer.Option.set client Current Modifiable true >>| ok_exn in
    let%bind () = print_new_buffer_modifiability () in
    [%expect {| (new_buffers_are_modifiable true) |}];
    let%bind () = Buffer.Option.set_for_new_buffers client Modifiable false >>| ok_exn in
    let%bind () = print_new_buffer_modifiability () in
    [%expect {| (new_buffers_are_modifiable false) |}];
    let%bind () = print_modifiability Current in
    [%expect {| ((modifiable true) (actually_modifiable true)) |}];
    let%bind () = Command.exec client "new" >>| ok_exn in
    let%bind () = print_modifiability Current in
    [%expect {| ((modifiable false) (actually_modifiable false)) |}];
    let%bind () = print_modifiability (Id buffer) in
    [%expect {| ((modifiable true) (actually_modifiable true)) |}];
    Deferred.Or_error.return ())
;;

let%expect_test "API for global-local buffer options" =
  with_client (fun client ->
    let%bind buffer = Nvim.get_current_buf client >>| ok_exn in
    let print_default_dictionary () =
      Buffer.Option.get_default client Dictionary
      >>| ok_exn
      >>| [%sexp_of: string list]
      >>| print_s
    in
    let print_dictionary buffer =
      Buffer.Option.get client buffer Dictionary
      >>| ok_exn
      >>| [%sexp_of: string list]
      >>| print_s
    in
    let%bind () = print_dictionary Current in
    [%expect {| ("") |}];
    let%bind () = print_default_dictionary () in
    [%expect {| ("") |}];
    let%bind () = Command.exec client "new" >>| ok_exn in
    let%bind () = Buffer.Option.set client Current Dictionary [ "dict" ] >>| ok_exn in
    let%bind () =
      Buffer.Option.set_default client Dictionary [ "default-dict" ] >>| ok_exn
    in
    let%bind () = print_default_dictionary () in
    [%expect {| (default-dict) |}];
    let%bind () = print_dictionary Current in
    [%expect {| (dict) |}];
    let%bind () = print_dictionary (Id buffer) in
    [%expect {| (default-dict) |}];
    Deferred.Or_error.return ())
;;

let%expect_test "get_mark, set_mark, delete_mark" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind escaped_keys =
        Nvim.replace_termcodes_and_keycodes client "ihello<Esc>mma\nworld!<Esc>"
      in
      let%bind () = Nvim.feedkeys client (`Keycodes escaped_keys) ~mode:"n" in
      let%bind mark = Buffer.get_mark client Current ~sym:'m' in
      print_s [%sexp (mark : Mark.t Buffer.With_changedtick.t)];
      let%bind () =
        Buffer.set_mark client Current { sym = 'm'; pos = { row = 1; col = 0 } }
      in
      let%bind mark = Buffer.get_mark client Current ~sym:'m' in
      print_s [%sexp (mark : Mark.t Buffer.With_changedtick.t)];
      let%bind () = Buffer.delete_mark client Current 'm' in
      let%bind.Deferred mark = Buffer.get_mark client Current ~sym:'m' in
      print_s [%sexp (mark : Mark.t Buffer.With_changedtick.t Or_error.t)];
      return ())
  in
  (* Demonstrate that marks are (1-indexed row, 0-indexed col). *)
  [%expect
    {|
    ((value ((sym m) (pos ((row 1) (col 4))))) (changedtick 5))
    ((value ((sym m) (pos ((row 1) (col 0))))) (changedtick 5))
    (Error
     (("Mark not set in buffer" (buffer Current) (sym m))
      (msgpack (Array ((Int 0) (Int 0))))))
    |}];
  return ()
;;

let%expect_test "get_var, set_var, delete_var" =
  Dynamic.set_root Backtrace.elide true;
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = Buffer.set_var client Current "foo" ~type_:Bool ~value:true in
      let%bind.Deferred value = Buffer.get_var client Current "foo" ~type_:Bool in
      print_s [%sexp (value : bool Or_error.t)];
      let%bind () = Buffer.delete_var client Current "foo" in
      let%bind.Deferred value = Buffer.get_var client Current "foo" ~type_:Bool in
      print_s [%sexp (value : bool Or_error.t)];
      return ())
  in
  Dynamic.set_root Backtrace.elide false;
  [%expect
    {|
    (Ok true)
    (Error
     (("Vim returned error" "Key not found: foo" (error_type Validation))
      (("Called from" lib/vcaml/test/bindings/test_buffer.ml:LINE:COL))))
    |}];
  return ()
;;

let%expect_test "unload and wipeout semantics (also exists, loaded)" =
  with_client (fun client ->
    Dynamic.set_root Backtrace.elide true;
    let%bind buffer = Buffer.create client ~listed:true ~scratch:false >>| ok_exn in
    let get_status () =
      let open Deferred.Or_error.Let_syntax in
      match%bind Buffer.exists client buffer with
      | false -> return "Invalid"
      | true ->
        (match%map Buffer.loaded client buffer with
         | true -> "Loaded"
         | false -> "Unloaded")
    in
    let test (f : here:[%call_pos] -> _) ~even_if_modified =
      let%map result =
        let open Deferred.Or_error.Let_syntax in
        let%bind () =
          Command.exec client "sbuffer" ~range_or_count:(Count (buffer :> int))
        in
        let%bind () = Buffer.Option.set client (Id buffer) Modified true in
        let%bind before = get_status () in
        let%bind () = f client (Buffer.Or_current.Id buffer) ~even_if_modified in
        let%bind after = get_status () in
        return [%message (before : string) (after : string)]
      in
      print_s [%sexp (result : Sexp.t Or_error.t)]
    in
    let%bind () = test Buffer.unload ~even_if_modified:false in
    [%expect
      {|
      (Error
       (("Vim returned error" "Failed to unload buffer." (error_type Exception))
        (("Called from" lib/vcaml/test/bindings/test_buffer.ml:LINE:COL))))
      |}];
    let%bind () = test Buffer.unload ~even_if_modified:true in
    [%expect {| (Ok ((before Loaded) (after Unloaded))) |}];
    let%bind () = test Buffer.wipeout ~even_if_modified:false in
    [%expect
      {|
      (Error
       (("Vim returned error" "Failed to unload buffer." (error_type Exception))
        (("Called from" lib/vcaml/test/bindings/test_buffer.ml:LINE:COL))))
      |}];
    let%bind () = test Buffer.wipeout ~even_if_modified:true in
    [%expect {| (Ok ((before Loaded) (after Invalid))) |}];
    Dynamic.set_root Backtrace.elide false;
    Deferred.Or_error.return ())
;;

let%expect_test "line_count, get_byte_offset_of_line" =
  with_client (fun client ->
    let open Deferred.Or_error.Let_syntax in
    let%bind () =
      Buffer.set_lines
        client
        Current
        ~start:0
        ~end_:(-1)
        ~strict_indexing:true
        [ "foo"; "bar" ]
    in
    let%bind line_count = Buffer.line_count client Current in
    let%bind line2_offset =
      (* There are 4 bytes before the second line begins: "foo\n". *)
      Buffer.get_byte_offset_of_line client Current ~line:1
    in
    print_s
      [%message
        ""
          (line_count : int Buffer.With_changedtick.t)
          (line2_offset : int Buffer.With_changedtick.t)];
    [%expect
      {|
      ((line_count ((value 2) (changedtick 3)))
       (line2_offset ((value 4) (changedtick 3))))
      |}];
    return ())
;;

let%expect_test "open_term, Nvim.send_to_channel" =
  with_ui_client (fun client ui ->
    let test ~modified =
      let open Deferred.Or_error.Let_syntax in
      let%bind () = Command.exec client "enew" in
      let%bind () = Buffer.Option.set client Current Modified modified in
      let%bind channel = Buffer.open_term client Current in
      let%bind () = Nvim.send_to_channel client ~channel "foo\r\nbar\r\n" in
      let%map screen = get_screen_contents ui in
      print_endline screen
    in
    let%bind result = test ~modified:true in
    print_s [%sexp (result : unit Or_error.t)];
    [%expect {| (Error "Cannot open terminal in modified buffer.") |}];
    let%bind result = test ~modified:false in
    print_s [%sexp (result : unit Or_error.t)];
    [%expect
      {|
      ╭────────────────────────────────────────────────────────────────────────────────╮
      │foo                                                                             │
      │bar                                                                             │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │                                                                                │
      │[Scratch]                                                     1,1            All│
      │                                                                                │
      ╰────────────────────────────────────────────────────────────────────────────────╯
      (Ok ())
      |}];
    Deferred.Or_error.return ())
;;

let%expect_test "Extmark indexing" =
  with_client (fun client ->
    let open Deferred.Or_error.Let_syntax in
    let%bind () = Buffer.Option.set client Current Syntax "on" in
    let%bind escaped_keys =
      Nvim.replace_termcodes_and_keycodes client "ihello\nworld!<Esc>"
    in
    let%bind () = Nvim.feedkeys client (`Keycodes escaped_keys) ~mode:"n" in
    let%bind namespace = Namespace.create client () in
    let%bind extmark =
      (* Start the extmark range on the 'o' of hello and end on the 'w' of world,
         inclusive. *)
      Buffer.Untested.create_extmark
        client
        Current
        ~namespace
        ~start_inclusive:{ row = 0; col = 4 }
        ~end_exclusive:{ row = 1; col = 1 }
        ()
    in
    let%bind result = Buffer.Untested.get_extmark_with_details client extmark in
    print_s
      [%sexp
        (result : (Position.t * Msgpack.t String.Map.t) option Buffer.With_changedtick.t)];
    (* It seems the ([end_row], [end_col]) position in the details dictionary is 0-based
       inclusive (row, col). *)
    [%expect
      {|
      ((value
        ((((row 0) (col 4))
          ((end_col (Int 1)) (end_right_gravity (Bool false)) (end_row (Int 1))
           (ns_id (Int 1)) (right_gravity (Bool true))))))
       (changedtick 5))
      |}];
    let%bind result =
      Buffer.Untested.all_extmarks
        client
        Current
        ~namespace
        ~start_inclusive:{ row = 0; col = 0 }
        ~end_inclusive:{ row = 0; col = 3 }
    in
    print_s [%sexp (result : (_ * Position.t) list Buffer.With_changedtick.t)];
    [%expect {| ((value ()) (changedtick 5)) |}];
    let%bind result =
      Buffer.Untested.all_extmarks
        client
        Current
        ~namespace
        ~start_inclusive:{ row = 1; col = 1 }
        ~end_inclusive:{ row = 2; col = 0 }
    in
    print_s [%sexp (result : (_ * Position.t) list Buffer.With_changedtick.t)];
    [%expect {| ((value ()) (changedtick 5)) |}];
    let%bind result =
      Buffer.Untested.all_extmarks
        client
        Current
        ~namespace
        ~start_inclusive:{ row = 0; col = 0 }
        ~end_inclusive:{ row = 2; col = 0 }
    in
    print_s [%sexp (result : (_ * Position.t) list Buffer.With_changedtick.t)];
    [%expect {| ((value ((_ ((row 0) (col 4))))) (changedtick 5)) |}];
    return ())
;;

let%expect_test "Subscribing twice to the same buffer fails" =
  let%bind () =
    Expect_test_helpers_async.require_does_raise_async (fun () ->
      with_client (fun client ->
        let open Deferred.Or_error.Let_syntax in
        let%bind (_ : _ Pipe.Reader.t) = Buffer.subscribe client Current in
        let%bind (_ : _ Pipe.Reader.t) = Buffer.subscribe client Current in
        return ()))
  in
  [%expect {| ("Already subscribed to buffer" (buffer 1)) |}];
  return ()
;;

let%expect_test "Racing subscriptions to the same buffer fail" =
  let%bind () =
    Expect_test_helpers_async.require_does_raise_async (fun () ->
      with_client (fun client ->
        let open Deferred.Or_error.Let_syntax in
        let%bind (_ : _ Pipe.Reader.t) = Buffer.subscribe client Current
        and (_ : _ Pipe.Reader.t) = Buffer.subscribe client Current in
        return ()))
  in
  [%expect {| ("Already subscribed to buffer" (buffer 1)) |}];
  return ()
;;

let%expect_test "Resubscribing to a buffer succeeds" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind events = Buffer.subscribe client Current in
      Pipe.close_read events;
      let%bind (_ : _ Pipe.Reader.t) = Buffer.subscribe client Current in
      return ())
  in
  [%expect {| |}];
  return ()
;;

let%expect_test "Racing resubscriptions to the same buffer fail" =
  let%bind () =
    Expect_test_helpers_async.require_does_raise_async (fun () ->
      with_client (fun client ->
        let open Deferred.Or_error.Let_syntax in
        let%bind buf =
          (* To exercise the resubscription race effectively we use an explicit buffer
             reference rather than [Current] so we don't need to wait for a VCaml
             roundtrip to resolve the buffer, since by that time the [Detach] event will
             have been sent. *)
          Nvim.get_current_buf client
        in
        let%bind events = Buffer.subscribe client (Id buf) in
        Pipe.close_read events;
        let%bind (_ : _ Pipe.Reader.t) = Buffer.subscribe client (Id buf)
        and (_ : _ Pipe.Reader.t) = Buffer.subscribe client (Id buf) in
        return ()))
  in
  [%expect {| ("Already subscribing to buffer" (buffer 1)) |}];
  return ()
;;

let%expect_test "Still in good state after failing to attach (can retry)" =
  let%bind () =
    Expect_test_helpers_async.require_does_not_raise_async (fun () ->
      with_client (fun client ->
        let%bind pipe = Buffer.subscribe client Current >>| ok_exn in
        match%bind Buffer.subscribe client Current with
        | Ok (_ : _ Pipe.Reader.t) -> failwith "Made two subscriptions to the same buffer"
        | Error _ ->
          Pipe.close_read pipe;
          (* Now the subscription should succeed. *)
          Buffer.subscribe client Current |> Deferred.Or_error.ignore_m))
  in
  [%expect {| |}];
  return ()
;;
