open! Core
open! Async

let neovim_path = Core.Sys.getenv "NEOVIM_PATH" |> Option.value ~default:"nvim"

let args =
  [ "--headless"
  ; (* Start the editor without a gui *)
    "-n" (* Disable swapfiles *)
  ; "--embed"
  (* use stdin and stdout instead of unix pipe for communication with the
     plugin *)
  ; "-u" (* Start with no vimrc *)
  ; "NONE"
  ; "-i" (* Start with no shada files *)
  ; "NONE"
  ]
;;

let with_client ?(on_error = Error.raise) f =
  let temp_dir = Filename_unix.temp_dir "neovim" "test" in
  let nvim_log_file = Core.(temp_dir ^/ "nvim_low_level_log.txt") in
  let%bind client, _process =
    Vcaml.Client.attach
      (Embed
         { prog = neovim_path
         ; args
         ; working_dir = temp_dir
         ; env = [ "NVIM_LOG_FILE", nvim_log_file ]
         })
      ~on_error
    >>| ok_exn
  in
  f client >>| ok_exn
;;

let simple k to_sexp =
  with_client (fun client ->
    let%map.Deferred.Or_error result = Vcaml.run_join client k in
    print_s (to_sexp result))
;;

module Test_ui = struct
  type t =
    { mutable buffer : string array array
    ; mutable cursor_col : int
    ; mutable cursor_row : int
    ; flush_bus : (string -> unit) Bus.Read_write.t
    }

  let create () =
    { buffer = [||]
    ; cursor_col = 0
    ; cursor_row = 0
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
  let apply t (event : Vcaml.Ui.Event.t) =
    match event with
    | Flush -> Bus.write t.flush_bus (ui_to_string t)
    | Clear ->
      Array.iter t.buffer ~f:(fun row ->
        Array.fill row ~pos:0 ~len:(Array.length row) " ")
    | Cursor_goto { row; col } ->
      t.cursor_col <- col;
      t.cursor_row <- row
    | Put { str } ->
      t.buffer.(t.cursor_row).(t.cursor_col) <- str;
      t.cursor_col <- t.cursor_col + 1
    | Resize { width; height } ->
      let new_array = Array.init height ~f:(fun _ -> Array.create ~len:width " ") in
      Array.iteri t.buffer ~f:(fun y row ->
        Array.iteri row ~f:(fun x c ->
          if x < width && y < height then new_array.(y).(x) <- c));
      t.buffer <- new_array
    | Mode_info_set _
    | Mode_change _
    | Highlight_set _
    | Update_fg _
    | Update_bg _
    | Update_sp _
    | Option_set _
    | Default_colors_set _ -> ()
    | _ -> print_s [%message "Ignored UI event" (event : Vcaml.Ui.Event.t)]
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
  let ui = Test_ui.create () in
  Test_ui.subscribe ui ~f:(Ivar.fill_if_empty contents_ivar);
  let%bind ui =
    Vcaml.Ui.attach
      client
      ~width
      ~height
      ~options:Vcaml.Ui.Options.empty
      ~on_event:(Test_ui.apply ui)
  in
  let%bind.Deferred contents = contents in
  let%bind () = Vcaml.Ui.detach ui in
  return contents
;;

let wait_until_text ?(timeout = Time_ns.Span.of_int_sec 2) client ~f =
  let open Deferred.Or_error.Let_syntax in
  let wait_until_text ~f =
    let is_timed_out = ref false in
    Clock_ns.run_after timeout (fun () -> is_timed_out := true) ();
    let%bind result =
      let repeating () =
        let%bind output = get_screen_contents client in
        match f output, !is_timed_out with
        | true, _ -> return (`Finished (Ok ()))
        | false, true -> return (`Finished (Error output))
        | false, false ->
          let%map _ = Deferred.ok (Clock_ns.after (Time_ns.Span.of_sec 0.1)) in
          `Repeat ()
      in
      Deferred.Or_error.repeat_until_finished () repeating
    in
    match result with
    | Ok () -> return ()
    | Error screen_contents ->
      (* print here instead of returning the string in the error in order to
         keep the sexp-printing from ruining all the unicode chars *)
      let error = Error.of_string "ERROR: timeout when looking for value on screen" in
      printf !"%{Error.to_string_hum}\n%s\n" error screen_contents;
      Deferred.Or_error.fail error
  in
  let wait_until_text_stabilizes () =
    let prev_text = ref None in
    let%bind () =
      wait_until_text ~f:(fun text ->
        match !prev_text with
        | Some prev_text when String.equal text prev_text -> true
        | Some _ | None ->
          prev_text := Some text;
          false)
    in
    return (Option.value_exn !prev_text)
  in
  let%bind () = wait_until_text ~f in
  wait_until_text_stabilizes ()
;;
