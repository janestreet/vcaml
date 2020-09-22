open! Core
open! Async
open Vcaml
open Deferred.Or_error.Let_syntax

module Buffer_data = struct
  type t =
    { current_file_pattern : File_pattern.t option
    ; file_patterns : File_pattern.t list
    }

  let fetch_from_vim client =
    let%bind buffer = Client.get_current_buf |> Vcaml.run_join client in
    let%bind filename = Buf.get_name ~buffer |> Vcaml.run_join client in
    let%bind.Deferred file_patterns = File_pattern.list filename in
    return { file_patterns; current_file_pattern = File_pattern.of_filename filename }
  ;;
end

let swap_vim_in_direction swap_in_direction client =
  let%bind { current_file_pattern; file_patterns } = Buffer_data.fetch_from_vim client in
  match swap_in_direction ~current_file_pattern ~file_patterns with
  | None -> return ()
  | Some file_pattern ->
    let%bind new_buffer =
      Buf.find_by_name_or_create ~name:(File_pattern.to_filename file_pattern)
      |> Vcaml.run_join client
    in
    Client.set_current_buf ~buffer:new_buffer |> Vcaml.run_join client
;;

module Echo_file_patterns = Vcaml_plugin.Oneshot.Make (struct
    let execute client =
      let%bind { file_patterns; _ } = Buffer_data.fetch_from_vim client in
      let stringified_file_list =
        List.map ~f:File_pattern.to_short_filename file_patterns |> String.concat ~sep:", "
      in
      Client.command ~command:(Printf.sprintf "echom \"%s\"" stringified_file_list)
      |> Vcaml.run_join client
    ;;
  end)

module List_file_patterns_in_fzf = Vcaml_plugin.Oneshot.Make (struct
    let execute client =
      let%bind { file_patterns; _ } = Buffer_data.fetch_from_vim client in
      match file_patterns with
      | [] -> return ()
      | hd :: _ as file_patterns ->
        let lines = List.map ~f:File_pattern.to_short_filename file_patterns in
        let fzf_config =
          Msgpack.Map
            [ String "source", Array (List.map ~f:(fun line -> Msgpack.String line) lines)
            ; String "down", Integer (List.length lines + 2)
            ; String "sink", String "e"
            ; String "dir", String (File_pattern.dirname hd)
            ]
        in
        let%bind (_ : Msgpack.t) =
          Vcaml.run_join client (Client.call_function ~fn:"fzf#run" ~args:[ fzf_config ])
        in
        return ()
    ;;
  end)

module Next_file_pattern = Vcaml_plugin.Oneshot.Make (struct
    let execute = swap_vim_in_direction File_pattern.next
  end)

module Prev_file_pattern = Vcaml_plugin.Oneshot.Make (struct
    let execute = swap_vim_in_direction File_pattern.prev
  end)

let main =
  Command.group
    ~summary:"plugin to cycle between ml, mli, and intf files"
    [ ( "list-fzf"
      , List_file_patterns_in_fzf.command
          ~summary:"list all possible files to cycle through in fzf"
          () )
    ; ( "list"
      , Echo_file_patterns.command ~summary:"echo all possible files to cycle through" ()
      )
    ; ( "next"
      , Next_file_pattern.command
          ~summary:"switch the current buffer to the next file in the list"
          () )
    ; ( "prev"
      , Prev_file_pattern.command
          ~summary:"switch the current buffer to the prev file in the list"
          () )
    ]
;;
