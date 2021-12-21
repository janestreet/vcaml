open! Core
open! Async
open! Vcaml
open! Vcaml_plugin
open Deferred.Or_error.Let_syntax

(* Simple Vcaml plugin to create a new buffer/window which the user can use as a
   scratchpad, with no vim bindings. The plugin is killed on window close.
   The editor only supports lowercase/uppercase letters + space/enter/backspace. *)

let of_ascii_list ascii_chars =
  ascii_chars |> List.map ~f:(Fn.compose String.of_char Char.of_int_exn)
;;

let lowercase = of_ascii_list (List.init 26 ~f:(( + ) 97))
let uppercase = of_ascii_list (List.init 26 ~f:(( + ) 65))
let typable_characters = lowercase @ uppercase

module Special_characters = struct
  let space = "space"
  let backspace = "bs"
  let enter = "cr"
  let all = [ space; backspace; enter ]
end

module State = struct
  type t =
    { buffer : Buffer.t Set_once.t
    ; window : Window.t Set_once.t
    }
  [@@deriving sexp_of]
end

module type S = Vcaml_plugin.Persistent.S with type state := State.t

let create_plugin ~sequencer =
  let module Arg = struct
    let name = "simple-editor"

    type state = State.t [@@deriving sexp_of]

    let init_state () = { State.buffer = Set_once.create (); window = Set_once.create () }

    let set_modifiable buffer value =
      Buffer.set_option ~buffer ~scope:`Local ~name:"modifiable" ~type_:Boolean ~value
    ;;

    let get_split_window =
      let%map.Api_call.Or_error () = Nvim.command ~command:"split"
      and new_win = Nvim.get_current_win in
      new_win
    ;;

    let shutdown_plugin_when_buffer_is_closed ~new_win ~buffer ~chan_id =
      let shutdown_on_leave =
        Printf.sprintf
          "autocmd BufWinLeave <buffer> silent! call rpcnotify(%d, 'shutdown')"
          chan_id
      in
      Api_call.Or_error.all_unit
        [ Nvim.set_current_win ~window:new_win
        ; Nvim.set_current_buf ~buffer
        ; Nvim.command ~command:"setlocal ve+=onemore"
        ; Nvim.command ~command:shutdown_on_leave
        ]
    ;;

    let add_vim_binding ~chan_id ~key_bind ~rpc_name =
      Nvim.command
        ~command:
          (Printf.sprintf
             "nnoremap <silent> <buffer> %s :call rpcnotify(%d, \"%s\")<cr>"
             key_bind
             chan_id
             rpc_name)
    ;;

    let add_key_listener ~chan_id key =
      add_vim_binding ~chan_id ~key_bind:key ~rpc_name:key
    ;;

    let add_special_character_listener ~chan_id key =
      add_vim_binding ~chan_id ~key_bind:(Printf.sprintf "<%s>" key) ~rpc_name:key
    ;;

    let add_key_listeners_api_call chan_id =
      let typable_api_call =
        typable_characters |> List.map ~f:(add_key_listener ~chan_id)
      in
      let special_api_call =
        Special_characters.all |> List.map ~f:(add_special_character_listener ~chan_id)
      in
      typable_api_call @ special_api_call |> Api_call.Or_error.all_unit
    ;;

    let on_startup client state ~shutdown:_ =
      let chan_id = Client.rpc_channel_id client in
      let%bind buffer =
        Buffer.find_by_name_or_create ~name:"simple-editor" |> run_join [%here] client
      in
      let%bind new_win = run_join [%here] client get_split_window in
      let%bind () =
        shutdown_plugin_when_buffer_is_closed ~new_win ~buffer ~chan_id
        |> run_join [%here] client
      in
      let%bind () = add_key_listeners_api_call chan_id |> run_join [%here] client in
      let%bind () = set_modifiable buffer false |> run_join [%here] client in
      Set_once.set_exn state.State.buffer [%here] buffer;
      Set_once.set_exn state.State.window [%here] new_win;
      return ()
    ;;

    let on_error = `Raise
    let vimscript_notify_fn = None
    let on_shutdown _client _state = return ()

    let get_nth_line ~client ~buffer n =
      Buffer.get_lines ~buffer ~start:(n - 1) ~end_:n ~strict_indexing:true
      |> run_join [%here] client
    ;;

    let with_modifiable ~buffer ~api_call =
      let%map.Api_call unlock_buf_or_err = set_modifiable buffer true
      and api_res_or_err = api_call
      and lock_buf_or_err = set_modifiable buffer false in
      let%bind.Or_error () = Or_error.all_unit [ unlock_buf_or_err; lock_buf_or_err ] in
      api_res_or_err
    ;;

    let set_lines ~client ~buffer ~start_at ~num_lines ~content =
      with_modifiable
        ~buffer
        ~api_call:
          (Buffer.set_lines
             ~buffer
             ~start:start_at
             ~end_:(start_at + num_lines)
             ~replacement:content
             ~strict_indexing:false)
      |> run_join [%here] client
    ;;

    let set_line ~client ~buffer ~content ~row =
      set_lines ~client ~buffer ~start_at:(row - 1) ~num_lines:1 ~content
    ;;

    let set_cursor ~client ~window ~row ~col =
      Window.set_cursor ~window { row; col } |> run_join [%here] client
    ;;

    let split_string_at st i =
      match i with
      | 0 -> "", st
      | i -> String.slice st 0 i, String.slice st i 0
    ;;

    let add_newline_at_mark client buffer window { Position.One_indexed_row.row; col } =
      let%bind line = get_nth_line ~client ~buffer row in
      let first_line, second_line = split_string_at (List.nth_exn line 0) col in
      let%bind () = set_line ~client ~buffer ~content:[ first_line; second_line ] ~row in
      set_cursor ~client ~window ~row:(row + 1) ~col:0
    ;;

    let insert_character_in_line col character line =
      let before, after = split_string_at line col in
      String.concat [ before; character; after ]
    ;;

    let set_character_at_mark
          character
          client
          buffer
          window
          { Position.One_indexed_row.row; col }
      =
      let%bind line = get_nth_line ~client ~buffer row in
      let new_line = line |> List.map ~f:(insert_character_in_line col character) in
      let%bind () = set_line ~client ~buffer ~content:new_line ~row in
      set_cursor ~client ~window ~row ~col:(col + 1)
    ;;

    let delete_from_beginning_of_line client buffer window row =
      let%bind curr_line = get_nth_line ~client ~buffer row in
      let%bind prev_line = get_nth_line ~client ~buffer (row - 1) in
      let result = String.concat (prev_line @ curr_line) in
      let%bind () =
        set_lines ~client ~buffer ~start_at:(row - 2) ~num_lines:2 ~content:[ result ]
      in
      set_cursor
        ~client
        ~window
        ~row:(row - 1)
        ~col:(String.length (List.nth_exn prev_line 0))
    ;;

    let delete_in_line client buffer window row col =
      let%bind line = get_nth_line ~client ~buffer row in
      let new_line =
        line
        |> List.map ~f:String.to_list
        |> List.map ~f:(List.filteri ~f:(fun i _ -> i <> col - 1))
        |> List.map ~f:String.of_char_list
      in
      let%bind () = set_line ~client ~buffer ~content:new_line ~row in
      set_cursor ~client ~window ~row ~col:(col - 1)
    ;;

    let delete_character_at_mark
          client
          buffer
          window
          { Position.One_indexed_row.row; col }
      =
      match row, col with
      | 1, 0 -> Deferred.Or_error.return ()
      | r, 0 -> delete_from_beginning_of_line client buffer window r
      | r, c -> delete_in_line client buffer window r c
    ;;

    let handle_edit_request ~f client { State.buffer; window } ~shutdown:_ =
      let buffer = Set_once.get_exn buffer [%here] in
      let window = Set_once.get_exn window [%here] in
      let%bind mark = Window.get_cursor ~window |> run_join [%here] client in
      f client buffer window mark
    ;;

    let handle_enter_request = handle_edit_request ~f:add_newline_at_mark
    let handle_key_rpc_request key = handle_edit_request ~f:(set_character_at_mark key)
    let handle_backspace_request = handle_edit_request ~f:delete_character_at_mark

    let create_rpc_handler ~rpc_name ~handling_fn =
      Vcaml_plugin.Persistent.Rpc.create_async
        ~name:rpc_name
        ~type_:Defun.Ocaml.Async.unit
        ~f:(fun state ~shutdown ~client ->
          Throttle.enqueue sequencer (fun () -> handling_fn client state ~shutdown))
    ;;

    let create_key_rpc_handler ~key ~str_to_insert =
      create_rpc_handler ~rpc_name:key ~handling_fn:(handle_key_rpc_request str_to_insert)
    ;;

    let call_shutdown _client _state ~shutdown =
      shutdown ();
      Deferred.Or_error.return ()
    ;;

    let alphabet_handlers =
      typable_characters
      |> List.map ~f:(fun key -> create_key_rpc_handler ~key ~str_to_insert:key)
    ;;

    let space_handler =
      create_key_rpc_handler ~key:Special_characters.space ~str_to_insert:" "
    ;;

    let enter_handler =
      create_rpc_handler
        ~rpc_name:Special_characters.enter
        ~handling_fn:handle_enter_request
    ;;

    let backspace_handler =
      create_rpc_handler
        ~rpc_name:Special_characters.backspace
        ~handling_fn:handle_backspace_request
    ;;

    let shutdown_handler =
      create_rpc_handler ~rpc_name:"shutdown" ~handling_fn:call_shutdown
    ;;

    let rpc_handlers =
      alphabet_handlers
      @ [ space_handler; enter_handler; backspace_handler; shutdown_handler ]
    ;;
  end
  in
  (module Vcaml_plugin.Persistent.Make (Arg) : S)
;;

let main =
  let (module Main) = create_plugin ~sequencer:(Sequencer.create ()) in
  Main.command ~summary:"A simple editor plugin for Neovim"
;;

module For_testing = struct
  module type S = Vcaml_plugin.Persistent.For_testing.S with type plugin_state := State.t

  let create_plugin ~sequencer =
    let (module Plugin) = create_plugin ~sequencer in
    (module Plugin.For_testing : S)
  ;;
end
