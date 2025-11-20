open Core
open Async
open Vcaml

let neovim_path = "nvim"
let hundred_ms = Time_ns.Span.create ~ms:100 ()

let time_source_at_epoch =
  Time_source.read_only (Time_source.create ~now:Time_ns.epoch ())
;;

(* Start with no init.vim, no shada file, and no swap files. *)
let default_args = [ "--clean"; "-n" ]

(* Start the editor without a gui, use stdin and stdout instead of Unix pipe for
   communication with the plugin, place socket relative to the temporary working directory
   since there's some undocumented internal limit for the socket length (it doesn't appear
   in `:h limits). *)
let required_args = [ "--headless"; "--embed"; "--listen"; "./socket" ]
let verbose_env_var = "VCAML_VERBOSE"
let elide_backtraces_env_var = "VCAML_ELIDE_BACKTRACES"

type verbose_debugging =
  { wrap_connection : (Reader.t -> Writer.t -> (Reader.t * Writer.t) Deferred.t) option
  ; done_logging : unit Deferred.t
  }

let verbose_debugging' ~log =
  let peer_name = "Neovim" in
  let my_name = "Plugin" in
  let done_logging = Ivar.create () in
  let wrap_connection reader writer =
    Shutdown.don't_finish_before (Ivar.read done_logging);
    let%bind ( reader
             , writer
             , `Stopped_reading stopped_reading
             , `Stopped_writing stopped_writing )
      =
      let open Msgpack_debug in
      let `Peer_1_to_2 sent, `Peer_2_to_1 receive =
        create_debug_printers ~pp ~peer1:my_name ~peer2:peer_name log
      in
      Man_in_the_middle_debugger.wrap_connection_to_peer
        { name = peer_name; reader; writer }
        ~my_name
        ~f:(function
        | `Sent -> sent
        | `Received -> receive)
    in
    let announce_closed_connection angstrom_exit_status ~from ~to_ =
      [%message
        [%string "%{from} -> %{to_}: Connection closed"]
          (angstrom_exit_status : (unit, string) Result.t)]
      |> Sexp.to_string_hum
      |> Out_channel.output_string log;
      Out_channel.output_char log '\n';
      Out_channel.flush log
    in
    don't_wait_for
      (let%map () =
         let%map exit_status = stopped_reading in
         announce_closed_connection exit_status ~from:peer_name ~to_:my_name
       and () =
         let%map exit_status = stopped_writing in
         announce_closed_connection exit_status ~from:my_name ~to_:peer_name
       in
       Ivar.fill_exn done_logging ());
    return (reader, writer)
  in
  { wrap_connection = Some wrap_connection; done_logging = Ivar.read done_logging }
;;

let verbose_debugging ~verbose =
  match verbose with
  | false -> { wrap_connection = None; done_logging = return () }
  | true -> verbose_debugging' ~log:stderr
;;

let replace_timestamp_in_log_message =
  let open Re in
  let exact_digits n = repn digit n (Some n) in
  let at_least_digits n = repn digit n None in
  [ exact_digits 4
  ; char '-'
  ; exact_digits 2
  ; char '-'
  ; exact_digits 2
  ; char 'T'
  ; exact_digits 2
  ; char ':'
  ; exact_digits 2
  ; char ':'
  ; exact_digits 2
  ; char '.'
  ; at_least_digits 3
  ]
  |> seq
  |> compile
  |> replace ~all:false ~f:(fun _ -> "TIMESTAMP")
;;

let replace_line_numbers_in_log_message =
  let open Re in
  [ group (rep1 (diff any (set ": "))); char ':'; rep1 digit; char ':' ]
  |> seq
  |> compile
  |> replace ~f:(fun group ->
    match Group.get_opt group 1 with
    | None -> Group.get group 0
    | Some prefix -> prefix ^ ":LINE:")
;;

let with_client
  ?(args = default_args)
  ?env
  ?links
  ?(time_source = time_source_at_epoch)
  ?(on_error = `Raise)
  ?(before_connecting = fun _ -> return ())
  ?(verbose = false)
  ?(warn_if_neovim_exits_early = true)
  f
  =
  Expect_test_helpers_async.within_temp_dir ?links (fun () ->
    let args = required_args @ args in
    let%bind working_dir = Sys.getcwd () in
    let xdg_cache_home = working_dir ^/ ".cache" in
    let xdg_config_home = working_dir ^/ ".config" in
    let xdg_data_home = working_dir ^/ ".local/share" in
    let xdg_state_home = working_dir ^/ ".local/state" in
    let nvim_log_file = xdg_state_home ^/ "nvim/log" in
    let verbose_log_file = Filename_unix.temp_file ~in_dir:working_dir "vcaml" ".log" in
    (* We set this variable in the current environment instead of just extending Neovim's
       environment so that clients that are attached with [attach_client] will also use
       verbose logging. *)
    if verbose
    then
      (Unix.putenv [@ocaml.alert "-unsafe_multidomain"])
        ~key:verbose_env_var
        ~data:verbose_log_file;
    let env =
      let env =
        let base =
          Core_unix.Env.expand
            (`Extend
              [ "XDG_CACHE_HOME", xdg_cache_home
              ; "XDG_CONFIG_HOME", xdg_config_home
              ; "XDG_DATA_HOME", xdg_data_home
              ; "XDG_STATE_HOME", xdg_state_home
                (*=Not set:
                  - XDG_RUNTIME_DIR, which is used for stdpath("run")
                  - XDG_CONFIG_DIRS, which is used for stdpath("config_dirs")
                  - XDG_DATA_DIRS,   which is used for stdpath("data_dirs") *)
              ; "NVIM_RPLUGIN_MANIFEST", "rplugin.vim"
              ; ( elide_backtraces_env_var
                , [%string.global "%{Dynamic.get Backtrace.elide#Bool}"] )
              ])
        in
        match env with
        | None -> base
        | Some getenv -> Core_unix.Env.expand ~base (getenv (`Tmpdir working_dir))
      in
      `Replace_raw env
    in
    let client = Client.create ~name:"test-client" ~on_error in
    let%bind () = before_connecting client in
    let { wrap_connection; done_logging } = verbose_debugging ~verbose in
    let%bind client, process =
      Private.attach_client
        ?wrap_connection
        client
        (Embed { prog = neovim_path; args; working_dir; env })
        ~time_source
      >>| ok_exn
    in
    let exited_early = ref true in
    don't_wait_for
      (let%map exit_or_signal = Process.wait process in
       if !exited_early && warn_if_neovim_exits_early
       then
         print_s
           [%message
             "Neovim exited before the test finished"
               (exit_or_signal : Unix.Exit_or_signal.t)]);
    let%bind result =
      Monitor.try_with_join_or_error ~rest:`Log (fun () ->
        let open Deferred.Or_error.Let_syntax in
        let vim_did_enter = Ivar.create () in
        let%bind (_ : Autocmd.Id.t) =
          let%bind group = Autocmd.Group.create client "vim-did-enter-can-start-test" in
          Autocmd.create
            client
            ~description:"Vim finished initializing - can start test"
            ~group
            ~patterns_or_buffer:(Patterns [ "*" ])
            ~events:[ VimEnter ]
            (Ocaml_from_nvim.Callback.anon_rpc (fun ~run_in_background:_ ~client:_ ->
               Ivar.fill_if_empty vim_did_enter ();
               return ()))
        in
        let%bind () =
          (* Neovim may have finished initializing before we registered the autocmd. *)
          match%bind Nvim.get_vvar client "vim_did_enter" ~type_:Bool with
          | false -> return ()
          | true ->
            Ivar.fill_if_empty vim_did_enter ();
            return ()
        in
        let%bind () = Ivar.read vim_did_enter |> Deferred.ok in
        let%bind () =
          (* This check has a sharp corner: if a plugin's startup includes a command that
             would raise an error but is silenced with [:silent!], that will still get
             flagged here because [:silent!] sets [v:errmsg]. As a work-around, save
             [v:errmsg] before invoking [:silent!] and restore it afterward. *)
          match%bind Nvim.get_vvar client "errmsg" ~type_:String with
          | "" -> return ()
          | error -> Deferred.Or_error.error_string error
        in
        f client)
    in
    exited_early := false;
    let%bind () = Client.close client in
    let%bind () = done_logging in
    let%bind () =
      (* Because the client is embedded, stdin and stdout are used for Msgpack RPC.
         However, there still may be errors reported on stderr that we should capture. *)
      let%map stderr = Process.stderr process |> Reader.contents >>| String.split_lines
      and low_level_log =
        match%bind Sys.file_exists_exn nvim_log_file with
        | false -> return []
        | true ->
          Reader.file_lines nvim_log_file
          >>| List.filter ~f:(Fn.non String.is_empty)
          >>| (function
           | [] -> []
           | lines ->
             let lines =
               List.map lines ~f:(fun line ->
                 line
                 |> replace_timestamp_in_log_message
                 |> replace_line_numbers_in_log_message)
             in
             [ [ "-----  NVIM_LOG_FILE  -----" ]
             ; lines
             ; [ "---------------------------" ]
             ]
             |> List.concat)
      and verbose_log =
        (* This file will be populated by VCaml plugins that are launched during
           integration tests under verbose mode (in other words, when the test setup is
           test<->neovim<->plugin). The test<->neovim communication is just logged to
           stderr and captured by the expect test framework. *)
        Reader.file_lines verbose_log_file
      in
      match
        [ stderr; verbose_log; low_level_log ]
        |> List.concat
        |> List.filter ~f:(Fn.non String.is_empty)
      with
      | [] -> ()
      | error_lines -> print_endline (String.concat error_lines ~sep:"\n")
    in
    if verbose then (Unix.unsetenv [@ocaml.alert "-unsafe_multidomain"]) verbose_env_var;
    return (ok_exn result))
;;

let print_s ?mach sexp =
  let working_dir = Sys_unix.getcwd () in
  let rec filter ~tmp_dir : Sexp.t -> Sexp.t = function
    | Atom atom ->
      Atom (String.substr_replace_all atom ~pattern:tmp_dir ~with_:"${TMPDIR}")
    | List list -> List (List.map list ~f:(filter ~tmp_dir))
  in
  (* [Expect_test_helpers_async.with_temp_dir] uses this suffix. *)
  match String.is_suffix working_dir ~suffix:".tmp" with
  | false -> print_s ?mach sexp
  | true -> print_s ?mach (filter sexp ~tmp_dir:working_dir)
;;

module Test_ui = struct
  type t =
    { mutable buffer : string array array
    ; mutable cursor_col : int
    ; mutable cursor_row : int
    ; flushed : [ `Awaiting_first_flush | `Flush of string ] Mvar.Read_write.t
    ; reader : Ui.Event.t Pipe.Reader.t
    }
  [@@ocaml.warning "-69"]

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
      if String.equal (Array.last_exn row) "─"
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

  (* Applies a message from the neovim "redraw" ui message sequence. *)
  let apply t (event : Ui.Event.t) =
    let unflush t =
      match Mvar.peek t.flushed with
      | None | Some `Awaiting_first_flush -> ()
      | Some (`Flush _) -> ignore (Mvar.take_now_exn t.flushed : _)
    in
    match event with
    | Flush { unparsed_fields = _ } ->
      (match Mvar.peek t.flushed with
       | Some `Awaiting_first_flush -> ignore (Mvar.take_now_exn t.flushed : _)
       | None | Some (`Flush _) -> Mvar.set t.flushed (`Flush (ui_to_string t)))
    | Grid_line { grid = 1; row; col_start; data; unparsed_fields = _ } ->
      unflush t;
      let col = ref col_start in
      let write str =
        t.buffer.(row).(!col) <- str;
        incr col
      in
      List.iter data ~f:(function
        | Array ([ String str ] | [ String str; Int _ ]) -> write str
        | Array [ String str; Int _; Int repeat ] ->
          for _ = 1 to repeat do
            write str
          done
        | _ -> raise_s [%message "Malformed gridline data" (data : Msgpack.t list)])
    | Grid_clear { grid = 1; unparsed_fields = _ } ->
      unflush t;
      Array.iter t.buffer ~f:(fun row ->
        Array.fill row ~pos:0 ~len:(Array.length row) " ")
    | Grid_cursor_goto { grid = 1; row; col; unparsed_fields = _ } ->
      unflush t;
      t.cursor_col <- col;
      t.cursor_row <- row
    | Grid_resize { grid = 1; width; height; unparsed_fields = _ } ->
      unflush t;
      let new_array = Array.init height ~f:(fun _ -> Array.create ~len:width " ") in
      Array.iteri t.buffer ~f:(fun y row ->
        Array.iteri row ~f:(fun x c ->
          if x < width && y < height then new_array.(y).(x) <- c));
      t.buffer <- new_array
    | Grid_scroll
        { grid = 1; top; bot; left = _; right = _; rows; cols = 0; unparsed_fields = _ }
      ->
      (* In Neovim 0.9.1, [cols] is fixed at [0] so we never need [left] or [right]. *)
      unflush t;
      (* Establish our understanding of grid scrolling. If this is violated we are
         probably misinterpreting this event. *)
      assert (abs rows < bot - top);
      (match Sign.of_int rows with
       | Zero -> ()
       | Neg ->
         for i = bot - 1 downto top - rows do
           t.buffer.(i) <- Array.copy t.buffer.(i + rows)
         done
       | Pos ->
         for i = top to bot - 1 - rows do
           t.buffer.(i) <- Array.copy t.buffer.(i + rows)
         done)
    | Win_viewport _ ->
      (* This only applies to ext_multigrid but is sent anyway due to a bug:
         https://github.com/neovim/neovim/issues/14956 *)
      ()
    | Unknown_event _ ->
      (* [Unknown_event] can occur when VCaml is used with a more recent version of Neovim
         than the one against which it was tested. `:h api-contract` says that new events
         should be ignored by older clients. *)
      ()
    | Busy_start _
    | Busy_stop _
    | Default_colors_set _
    | Highlight_set _
    | Hl_attr_define _
    | Hl_group_set _
    | Mode_change _
    | Mode_info_set _
    | Mouse_off _
    | Mouse_on _
    | Option_set _
    | Set_icon _
    | Set_title _
    | Update_bg _
    | Update_fg _
    | Update_sp _ -> ()
    | _ -> raise_s [%message "Ignored UI event" (event : Ui.Event.t)]
  ;;

  let attach ?(width = 80) ?(height = 30) here client =
    let open Deferred.Or_error.Let_syntax in
    let%bind reader =
      Ui.attach
        ~here
        client
        ~width
        ~height
        ~options:Ui.Options.default
        ~only_enable_options_supported_by_other_attached_uis:true
    in
    let t =
      { buffer = [||]; cursor_col = 0; cursor_row = 0; flushed = Mvar.create (); reader }
    in
    Mvar.set t.flushed `Awaiting_first_flush;
    don't_wait_for (Pipe.iter_without_pushback reader ~f:(apply t));
    return t
  ;;

  let with_ui ?width ?height ~(here : [%call_pos]) client f =
    let open Deferred.Or_error.Let_syntax in
    let%bind t = attach here client ?width ?height in
    let%bind result = f t in
    Pipe.close_read t.reader;
    return result
  ;;
end

let rec get_screen_contents ui =
  match Pipe.is_closed ui.Test_ui.reader with
  | true ->
    Deferred.Or_error.error_s [%message "Tried to get screen contents of detached UI"]
  | false ->
    (match Mvar.peek ui.Test_ui.flushed with
     | None ->
       let%bind () = Mvar.value_available ui.flushed in
       get_screen_contents ui
     | Some `Awaiting_first_flush ->
       let%bind () = Clock_ns.after hundred_ms in
       get_screen_contents ui
     | Some (`Flush screen) ->
       (* Attempt to confirm that Neovim has finished sending updates. We don't want to
          grab a flush if more data is immediately following. *)
       choose
         [ choice (Mvar.taken ui.flushed) (fun () -> get_screen_contents ui)
         ; choice (Clock_ns.after hundred_ms) (fun () -> Deferred.Or_error.return screen)
         ]
       |> Deferred.join)
;;

let wait_until_text ?(timeout = Time_ns.Span.of_int_sec 2) ~(here : [%call_pos]) ui ~f =
  let open Deferred.Or_error.Let_syntax in
  let wait_until_text ~f =
    let is_timed_out = ref false in
    Clock_ns.run_after timeout (fun () -> is_timed_out := true) ();
    let%bind result =
      let repeating () =
        let%bind output = get_screen_contents ui in
        match f output, !is_timed_out with
        | true, _ -> return (`Finished (Ok ()))
        | false, true -> return (`Finished (Error output))
        | false, false ->
          let%map _ = Deferred.ok (Clock_ns.after hundred_ms) in
          `Repeat ()
      in
      Deferred.Or_error.repeat_until_finished () repeating
    in
    match result with
    | Ok () -> return ()
    | Error screen_contents ->
      (* print here instead of returning the string in the error in order to keep the
         sexp-printing from ruining all the unicode chars *)
      let error =
        Error.create_s
          (List
             [ Atom "ERROR: timeout when looking for value on screen"
             ; List
                 [ Atom "Called from"; [%sexp (here : Private.Source_code_position.t)] ]
             ])
      in
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

let with_ui_client
  ?width
  ?height
  ?args
  ?env
  ?links
  ?time_source
  ?on_error
  ?before_connecting
  ?verbose
  ?warn_if_neovim_exits_early
  f
  =
  with_client
    ?args
    ?env
    ?links
    ?time_source
    ?on_error
    ?before_connecting
    ?verbose
    ?warn_if_neovim_exits_early
    (fun client -> Test_ui.with_ui ?width ?height client (fun ui -> f client ui))
;;

let socket_client
  ?(time_source = time_source_at_epoch)
  ?(on_error = `Raise)
  ?(before_connecting = fun _ -> return ())
  ?(verbose = false)
  socket
  =
  let client = Client.create ~name:"test-client" ~on_error in
  let%bind () = before_connecting client in
  (* There's nothing we can do with [done_logging] here and it's not clear that it's worth
     cluttering the interface by returning it. *)
  let { wrap_connection; done_logging = _ } = verbose_debugging ~verbose in
  Private.attach_client ?wrap_connection client ~time_source (Socket (`Address socket))
;;

module For_debugging = struct
  let with_ui_client
    ?(time_source = time_source_at_epoch)
    ?(on_error = `Raise)
    ?(before_connecting = fun _ -> return ())
    ?(verbose = false)
    ~socket
    f
    =
    let { wrap_connection; done_logging } = verbose_debugging ~verbose in
    let%bind client =
      let client = Client.create ~name:"test-client" ~on_error in
      let%bind () = before_connecting client in
      Private.attach_client
        ?wrap_connection
        client
        ~time_source
        (Socket (`Address socket))
      >>| ok_exn
    in
    let%bind attached_uis = Ui.describe_attached_uis client >>| ok_exn in
    let width, height =
      attached_uis
      |> List.map ~f:(fun { width; height; _ } -> width, height)
      |> List.unzip
      |> Tuple2.map ~f:(List.min_elt ~compare)
      |> Tuple2.map ~f:(fun opt -> Option.value_exn opt)
    in
    let%bind result =
      Test_ui.with_ui ~width ~height client (fun ui -> f client ui) >>| ok_exn
    in
    let%bind () = Client.close client in
    let%map () = done_logging in
    result
  ;;
end

module Private = struct
  include Private

  let neovim_path = neovim_path

  (* It's a bit strange and unfortunate that the plugin library needs to rely on the test
     helpers library for an operation as fundamental as attaching a client to Neovim, but
     trying to abstract away this dependency would probably be difficult and may be more
     confusing. The dependency is real - we want plugins to have support for running in
     tests with the verbose logging defined in this library. *)
  let attach_client ?stdio_override ?time_source client connection_type =
    let () =
      match Sys.getenv elide_backtraces_env_var with
      | Some "true" -> Dynamic.set_root Backtrace.elide true
      | Some "false" -> Dynamic.set_root Backtrace.elide false
      | _ -> ()
    in
    let wrap_connection =
      match Sys.getenv verbose_env_var with
      | None -> None
      | Some logfile ->
        (* Logging a header and footer is useful - the header demarcates the place in the
           output where verbose logs change from one client to another, and presence of
           the footer indicates that the logs are complete. *)
        let log = Out_channel.create logfile in
        let header_middle = [%string "  %{Client.Not_connected.name client}  "] in
        let header_side = String.make ((90 - String.length header_middle) / 2) '-' in
        let header = String.concat [ header_side; header_middle; header_side ] in
        let footer = String.make (String.length header) '-' in
        Out_channel.output_string log header;
        Out_channel.output_char log '\n';
        let { wrap_connection; done_logging } = verbose_debugging' ~log in
        upon done_logging (fun () ->
          Out_channel.output_string log footer;
          Out_channel.output_char log '\n';
          Out_channel.close log);
        wrap_connection
    in
    Private.attach_client
      ?wrap_connection
      ?stdio_override
      ?time_source
      client
      connection_type
  ;;
end

let%expect_test "We cannot have two blocking RPCs with the same name" =
  let register_dummy_rpc_handler ~name client =
    Ocaml_from_nvim.register_request_blocking
      (Connected client)
      ~name
      ~type_:Ocaml_from_nvim.Blocking.(return Nil)
      ~f:(fun ~run_in_background:_ ~client:_ -> Deferred.Or_error.return ())
  in
  let%map () =
    with_client (fun client ->
      register_dummy_rpc_handler client ~name:"test";
      Expect_test_helpers_base.require_does_raise (fun () ->
        register_dummy_rpc_handler client ~name:"test");
      Deferred.Or_error.return ())
  in
  [%expect {| (Failure "Already defined synchronous RPC: test") |}]
;;

let%expect_test "We cannot have two async RPCs with the same name" =
  let register_dummy_rpc_handler ~name client =
    Ocaml_from_nvim.register_request_async
      (Connected client)
      ~name
      ~type_:Ocaml_from_nvim.Async.(unit)
      ~f:(fun ~client:_ -> Deferred.Or_error.return ())
  in
  let%map () =
    with_client (fun client ->
      register_dummy_rpc_handler client ~name:"test";
      Expect_test_helpers_base.require_does_raise (fun () ->
        register_dummy_rpc_handler client ~name:"test");
      Deferred.Or_error.return ())
  in
  [%expect {| (Failure "Already defined asynchronous RPC: test") |}]
;;

(* We allow this in case a plugin wants to implement slightly different semantics based on
   whether it is called with [rpcrequest] or [rpcnotify]. *)
let%expect_test "We can have an async RPC and a blocking RPC with the same name" =
  let%map () =
    with_client (fun client ->
      let name = "test" in
      Ocaml_from_nvim.register_request_blocking
        (Connected client)
        ~name
        ~type_:Ocaml_from_nvim.Blocking.(return Nil)
        ~f:(fun ~run_in_background:_ ~client:_ -> Deferred.Or_error.return ());
      Ocaml_from_nvim.register_request_async
        (Connected client)
        ~name
        ~type_:Ocaml_from_nvim.Async.(unit)
        ~f:(fun ~client:_ -> Deferred.Or_error.return ());
      Deferred.Or_error.return ())
  in
  [%expect {| |}]
;;

let%expect_test "We can have two separate Embedded connections with RPC handlers sharing \
                 names without error (no bleeding state)"
  =
  let register_dummy_rpc_handler ~name client =
    Ocaml_from_nvim.register_request_blocking
      (Connected client)
      ~name
      ~type_:Ocaml_from_nvim.Blocking.(return Nil)
      ~f:(fun ~run_in_background:_ ~client:_ -> Deferred.Or_error.return ());
    Deferred.Or_error.return ()
  in
  let%bind () = with_client (register_dummy_rpc_handler ~name:"test") in
  let%map () = with_client (register_dummy_rpc_handler ~name:"test") in
  [%expect {| |}]
;;
