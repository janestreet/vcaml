open! Core
open! Async
open Vcaml
open Deferred.Or_error.Let_syntax

module Buffer_data = struct
  type t =
    { current_file_pattern : File_pattern.t option
    ; is_redundant_mli : bool
    ; file_patterns : File_pattern.t list
    }

  let fetch_from_vim client =
    let%bind buffer = Nvim.get_current_buf |> run_join [%here] client in
    let%bind filename = Buffer.get_name ~buffer |> run_join [%here] client in
    let%bind.Deferred file_patterns = File_pattern.list filename in
    let current_file_pattern = File_pattern.of_filename filename in
    let%bind.Deferred is_redundant_mli =
      match current_file_pattern with
      | None -> Deferred.return false
      | Some current_file_pattern -> File_pattern.is_redundant_mli current_file_pattern
    in
    return { file_patterns; current_file_pattern; is_redundant_mli }
  ;;
end

let swap_vim_in_direction swap_in_direction client ~sink =
  let%bind { current_file_pattern; file_patterns; is_redundant_mli } =
    Buffer_data.fetch_from_vim client
  in
  match swap_in_direction ~current_file_pattern ~is_redundant_mli ~file_patterns with
  | None -> return ()
  | Some file_pattern ->
    Nvim.command ~command:[%string {| %{sink} %{File_pattern.to_filename file_pattern} |}]
    |> run_join [%here] client
;;

let next_file_pattern = swap_vim_in_direction File_pattern.next
let prev_file_pattern = swap_vim_in_direction File_pattern.prev

let echo_file_patterns client =
  let%bind { file_patterns; _ } = Buffer_data.fetch_from_vim client in
  let stringified_file_list =
    List.map ~f:File_pattern.to_short_filename file_patterns |> String.concat ~sep:", "
  in
  Nvim.command ~command:(Printf.sprintf "echom \"%s\"" stringified_file_list)
  |> run_join [%here] client
;;

let list_file_patterns_in_fzf client =
  let run_fzf =
    wrap_viml_function ~function_name:"MliCyclerFzf" ~type_:Defun.Vim.(unary Object Nil)
  in
  let%bind { file_patterns; _ } = Buffer_data.fetch_from_vim client in
  match file_patterns with
  | [] -> return ()
  | hd :: _ as file_patterns ->
    let lines = List.map ~f:File_pattern.to_short_filename file_patterns in
    let config =
      Msgpack.Map
        [ String "source", Array (List.map ~f:(fun line -> Msgpack.String line) lines)
        ; String "down", Integer (List.length lines + 2)
        ; String "dir", String (File_pattern.dirname hd)
        ]
    in
    let%bind () = run_join [%here] client (run_fzf config) in
    return ()
;;

let main =
  let module Command = Async.Command in
  let run f () =
    let%bind client =
      Client.attach (Unix `Child) ~time_source:(Time_source.wall_clock ())
    in
    f client
  in
  Command.group
    ~summary:"plugin to cycle between ml, mli, and intf files"
    [ ( "list-fzf"
      , Command.async_or_error
          ~summary:"list all possible files to cycle through in fzf"
          (Command.Param.return (run list_file_patterns_in_fzf)) )
    ; ( "list"
      , Command.async_or_error
          ~summary:"echo all possible files to cycle through"
          (Command.Param.return (run echo_file_patterns)) )
    ; ( "next"
      , Command.async_or_error
          ~summary:"switch the current buffer to the next file in the list"
          (let%map_open.Command () = return ()
           and sink =
             flag_optional_with_default_doc
               "sink"
               string
               [%sexp_of: string]
               ~default:"edit"
               ~doc:"STRING Neovim command to invoke with next file as argument"
           in
           run (next_file_pattern ~sink)) )
    ; ( "prev"
      , Command.async_or_error
          ~summary:"switch the current buffer to the previous file in the list"
          (let%map_open.Command () = return ()
           and sink =
             flag_optional_with_default_doc
               "sink"
               string
               [%sexp_of: string]
               ~default:"edit"
               ~doc:"STRING Neovim command to invoke with previous file as argument"
           in
           run (prev_file_pattern ~sink)) )
    ]
;;

module For_testing = struct
  let next_file_pattern = next_file_pattern ~sink:"edit"
  let prev_file_pattern = prev_file_pattern ~sink:"edit"
  let echo_file_patterns = echo_file_patterns
  let list_file_patterns_in_fzf = list_file_patterns_in_fzf
end
