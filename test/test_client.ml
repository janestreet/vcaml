open! Core_kernel
open! Async
open Deferred.Or_error.Let_syntax

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

let with_client f =
  let temp_dir = Core.Filename.temp_dir "neovim" "test" in
  let nvim_log_file = Core.(temp_dir ^/ "nvim_low_level_log.txt") in
  let%bind client, _process =
    Vcaml.Client.embed
      ~prog:neovim_path
      ~args
      ~env:[ "NVIM_LOG_FILE", nvim_log_file ]
      ~working_dir:temp_dir
  in
  f client
;;

let with_client f = with_client f |> Deferred.map ~f:ok_exn

let simple k to_sexp =
  with_client (fun client ->
    let%map result = Vcaml.run_join client k in
    print_s (to_sexp result))
;;
