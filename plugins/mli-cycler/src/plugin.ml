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
    wrap_viml_function
      ~function_name:"MliCyclerFzf"
      ~type_:Defun.Vim.(Dict @-> return Nil)
  in
  let%bind { file_patterns; _ } = Buffer_data.fetch_from_vim client in
  match file_patterns with
  | [] -> return ()
  | hd :: _ as file_patterns ->
    let lines = List.map ~f:File_pattern.to_short_filename file_patterns in
    let config : (Msgpack.t * Msgpack.t) list =
      [ String "source", Array (List.map ~f:(fun line -> Msgpack.String line) lines)
      ; String "down", Integer (List.length lines + 2)
      ; String "dir", String (File_pattern.dirname hd)
      ]
    in
    let%bind () = run_join [%here] client (run_fzf config) in
    return ()
;;

module Plugin = Vcaml_plugin.Oneshot.Make (struct
    let name = "mli-cycler"
    let on_error = `Raise

    let rpc_handlers =
      let rpc = Vcaml_plugin.Oneshot.Rpc.create in
      [ rpc
          ~name:"list-fzf"
          ~type_:Defun.Ocaml.Sync.(return Nil)
          ~f:(fun ~keyboard_interrupted:_ ~client -> list_file_patterns_in_fzf client)
      ; rpc
          ~name:"list"
          ~type_:Defun.Ocaml.Sync.(return Nil)
          ~f:(fun ~keyboard_interrupted:_ ~client -> echo_file_patterns client)
      ; rpc
          ~name:"next"
          ~type_:Defun.Ocaml.Sync.(String @-> return Nil)
          ~f:(fun ~keyboard_interrupted:_ ~client sink -> next_file_pattern client ~sink)
      ; rpc
          ~name:"prev"
          ~type_:Defun.Ocaml.Sync.(String @-> return Nil)
          ~f:(fun ~keyboard_interrupted:_ ~client sink -> prev_file_pattern client ~sink)
      ]
    ;;
  end)

let main = Plugin.command ~summary:"Launch mli-cycler and await rpc."

module For_testing = struct
  let next_file_pattern = next_file_pattern ~sink:"edit"
  let prev_file_pattern = prev_file_pattern ~sink:"edit"
  let echo_file_patterns = echo_file_patterns
  let list_file_patterns_in_fzf = list_file_patterns_in_fzf
end
