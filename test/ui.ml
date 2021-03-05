open! Core_kernel
open! Async
open! Import
open! Vcaml
open Test_client

module Ui = struct
  type t =
    { mutable buffer : string array array
    ; mutable cursor_x : int
    ; mutable cursor_y : int
    ; mutable saw_resize_or_clear : bool
    ; flush_bus : (string -> unit) Bus.Read_write.t
    }

  let create () =
    { buffer = [||]
    ; cursor_x = 0
    ; cursor_y = 0
    ; saw_resize_or_clear = false
    ; flush_bus =
        Bus.create
          [%here]
          Arity1
          ~on_subscription_after_first_write:
            Bus.On_subscription_after_first_write.Allow_and_send_last_value
          ~on_callback_raise:Error.raise
    }
  ;;

  let subscribe t ~f =
    let (_ : _ Bus.Subscriber.t) = Bus.subscribe_exn t.flush_bus [%here] ~f in
    ()
  ;;

  let ui_to_string t =
    let module Buffer = Core.Buffer in
    let buffer = Buffer.create 0 in
    Buffer.add_string buffer "╭";
    Buffer.add_string
      buffer
      (List.init (Array.length t.buffer.(0)) ~f:(Fn.const "─") |> String.concat);
    Buffer.add_string buffer "╮";
    Buffer.add_char buffer '\n';
    Array.iter t.buffer ~f:(fun row ->
      if String.equal row.(0) "─"
      then Buffer.add_string buffer "├"
      else Buffer.add_string buffer "│";
      Array.iter row ~f:(fun string -> Buffer.add_string buffer string);
      if String.equal (Array.last row) "─"
      then Buffer.add_string buffer "┤"
      else Buffer.add_string buffer "│";
      Buffer.add_char buffer '\n');
    Buffer.add_string buffer "╰";
    Buffer.add_string
      buffer
      (List.init (Array.length t.buffer.(0)) ~f:(Fn.const "─") |> String.concat);
    Buffer.add_string buffer "╯";
    Buffer.contents buffer
  ;;

  (* applies a message from the neovim "redraw" ui message sequence. *)
  let apply t (message : Msgpack.t) =
    match message with
    | Array
        (String
           ( "option_set"
           | "default_colors_set"
           | "update_fg"
           | "update_bg"
           | "update_sp"
           | "highlight_set"
           | "mode_change"
           | "mode_info_set" )
         :: _) -> ()
    | Array (String "flush" :: _) ->
      if t.saw_resize_or_clear then Bus.write t.flush_bus (ui_to_string t)
    | Array (String "clear" :: _) ->
      t.saw_resize_or_clear <- true;
      Array.iter t.buffer ~f:(fun row ->
        Array.fill row ~pos:0 ~len:(Array.length row) " ")
    | Array [ String "cursor_goto"; Array [ Integer y; Integer x ] ] ->
      t.cursor_x <- x;
      t.cursor_y <- y
    | Array (String "put" :: contents) ->
      (try
         List.iter contents ~f:(fun c ->
           match c with
           | Array [ String c ] ->
             t.buffer.(t.cursor_y).(t.cursor_x) <- c;
             t.cursor_x <- t.cursor_x + 1
           | other -> raise_s [%message "unexpected contents" (other : Msgpack.t)])
       with
       | _ -> ())
    | Array [ String "resize"; Array [ Integer width; Integer height ] ] ->
      t.saw_resize_or_clear <- true;
      let new_array = Array.init height ~f:(fun _ -> Array.create ~len:width " ") in
      Array.iteri t.buffer ~f:(fun y row ->
        Array.iteri row ~f:(fun x c ->
          if x < width && y < height then new_array.(y).(x) <- c));
      t.buffer <- new_array
    | message -> print_s [%message "BUG" "unhandled application" (message : Msgpack.t)]
  ;;
end

let get_screen_contents ?(width = 80) ?(height = 30) client =
  let open Deferred.Or_error.Let_syntax in
  let contents_ivar = Ivar.create () in
  Clock_ns.run_after
    (Time_ns.Span.of_int_sec 5)
    (fun () -> Ivar.fill_if_empty contents_ivar "Timeout!")
    ();
  let contents = Ivar.read contents_ivar in
  let ui = Ui.create () in
  Ui.subscribe ui ~f:(Ivar.fill_if_empty contents_ivar);
  Vcaml.register_request_async
    client
    ~name:"redraw"
    ~type_:Vcaml.Defun.Ocaml.Async.rest
    ~f:(fun all ->
      List.iter all ~f:(Ui.apply ui);
      Deferred.unit);
  let%bind () =
    Vcaml.Client.Untested.ui_attach ~width ~height ~options:[] |> run_join client
  in
  let%bind.Deferred contents = contents in
  let%bind () = Vcaml.Client.Untested.ui_detach |> run_join client in
  return contents
;;

let%expect_test "get_screen_contents" =
  let%bind () =
    let open Deferred.Or_error.Let_syntax in
    with_client (fun client ->
      let%bind screen = get_screen_contents client in
      print_endline screen;
      return ())
  in
  [%expect
    {|
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │                                                                                │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │[No Name]                                                     0,0-1          All│
    │                                                                                │
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;

let%expect_test "get screen contents multiple times" =
  let%bind () =
    let open Deferred.Or_error.Let_syntax in
    with_client (fun client ->
      let%bind screen = get_screen_contents client in
      print_endline screen;
      let%bind () =
        Vcaml.Client.feedkeys ~keys:"ihello world" ~mode:"n" ~escape_csi:true
        |> run_join client
      in
      let%bind screen = get_screen_contents client in
      print_endline screen;
      return ())
  in
  [%expect
    {|
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │                                                                                │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │[No Name]                                                     0,0-1          All│
    │                                                                                │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │hello world                                                                     │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │~                                                                               │
    │[No Name] [+]                                                 1,12           All│
    │                                                                                │
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;

let%expect_test "screen contents after typing hello world" =
  let%bind () =
    let open Deferred.Or_error.Let_syntax in
    with_client (fun client ->
      let%bind () =
        Vcaml.Client.feedkeys ~keys:"ihello world" ~mode:"n" ~escape_csi:true
        |> run_join client
      in
      let%bind () = Vcaml.Client.command ~command:"vsplit" |> run_join client in
      let%bind () = Vcaml.Client.command ~command:"split" |> run_join client in
      let%bind screen = get_screen_contents client in
      print_endline screen;
      return ())
  in
  [%expect
    {|
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │hello world                             │hello world                            │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │[No Name] [+]         1,12           All│~                                      │
    │hello world                             │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │~                                       │~                                      │
    │[No Name] [+]         1,12           All [No Name] [+]        1,12           All│
    │                                                                                │
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;

let wait_until_text_shows_up ?(timeout = Time_ns.Span.of_sec 2.0) client ~f =
  let open Deferred.Or_error.Let_syntax in
  let prev_screen_contents = ref "" in
  let%map.Deferred result =
    Clock_ns.with_timeout
      timeout
      (Deferred.Or_error.repeat_until_finished () (fun () ->
         let%bind output = get_screen_contents client in
         prev_screen_contents := output;
         if f output
         then return (`Finished ())
         else (
           let%map _ = Deferred.ok (Async.after (Time.Span.of_sec 0.1)) in
           `Repeat ())))
  in
  match result with
  | `Result x -> x
  | `Timeout ->
    (* print here instead of returning the string in the error in order to
       keep the sexp-printing from ruining all the unicode chars *)
    print_endline
      ("ERROR: timeout when looking for value on screen\n"
       ^ "previous screen contents:\n"
       ^ !prev_screen_contents);
    Error (Error.of_string "ERROR: timeout when looking for value on screen")
;;

let%expect_test "timeout occurs" =
  let%map () =
    Expect_test_helpers_async.require_does_raise_async [%here] (fun () ->
      let open Deferred.Or_error.Let_syntax in
      with_client (fun client ->
        let%bind () = Client.command ~command:"e term://sh" |> run_join client in
        let%bind () = Client.command ~command:"file my-terminal" |> run_join client in
        let%bind () =
          wait_until_text_shows_up
            client
            ~f:(String.is_substring ~substring:"sh-4.2$")
        in
        let%bind _ =
          wait_until_text_shows_up
            client
            ~timeout:(Time_ns.Span.of_sec 0.01)
            ~f:(Fn.const false)
        in
        return ()))
  in
  [%expect
    {|
    ERROR: timeout when looking for value on screen
    previous screen contents:
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │sh-4.2$                                                                         │
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
    │                                                                                │
    │my-terminal                                                   1,1            All│
    │                                                                                │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    "ERROR: timeout when looking for value on screen" |}]
;;

let%expect_test "open up sh" =
  let%bind () =
    let open Deferred.Or_error.Let_syntax in
    with_client (fun client ->
      let%bind () = Client.command ~command:"e term://sh" |> run_join client in
      let%bind () = Client.command ~command:"file my-terminal" |> run_join client in
      let%bind () =
        wait_until_text_shows_up client ~f:(String.is_substring ~substring:"sh-4.2$")
      in
      let%bind screen = get_screen_contents client in
      print_endline screen;
      return ())
  in
  [%expect
    {|
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │sh-4.2$                                                                         │
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
    │                                                                                │
    │my-terminal                                                   1,1            All│
    │                                                                                │
    ╰────────────────────────────────────────────────────────────────────────────────╯ |}];
  return ()
;;
