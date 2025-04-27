open Core
open Async
open Vcaml

type state =
  { buffer : Buffer.t
  ; secret : string
  ; hint : char array
  ; bad_guesses : char Queue.t
  ; mutable started_guessing : bool
  }

(* Camel art by cruzer@diku.dk, taken from https://ascii.co.uk/art/camel. *)
module Art = struct
  let initial =
    [ {|   _____________         |}
    ; {| 〈             〉       |}
    ; {|   ‾‾‾‾‾‾‾‾‾‾‾‾‾         |}
    ; {|                         |}
    ; {|                         |}
    ; {|                         |}
    ; {|                         |}
    ; {|                         |}
    ; {|                         |}
    ; {|                         |}
    ; {|                         |}
    ]
  ;;

  let bad_guess1 =
    [ {|   _____________         |}
    ; {| 〈             〉       |}
    ; {|   ‾‾‾‾‾‾‾‾‾‾‾‾‾         |}
    ; {|                         |}
    ; {|            oo           |}
    ; {|                         |}
    ; {|                         |}
    ; {|                         |}
    ; {|                         |}
    ; {|                         |}
    ; {|                         |}
    ]
  ;;

  let bad_guess2 =
    [ {|   _____________         |}
    ; {| 〈             〉       |}
    ; {|   ‾‾‾‾‾‾‾‾‾‾‾‾‾         |}
    ; {|      \      //          |}
    ; {|       \   _oo\          |}
    ; {|          (__/ \         |}
    ; {|                         |}
    ; {|                         |}
    ; {|                         |}
    ; {|                         |}
    ; {|                         |}
    ]
  ;;

  let bad_guess3 =
    [ {|   _____________         |}
    ; {| 〈             〉       |}
    ; {|   ‾‾‾‾‾‾‾‾‾‾‾‾‾         |}
    ; {|      \      //          |}
    ; {|       \   _oo\          |}
    ; {|          (__/ \         |}
    ; {|             \  \        |}
    ; {|             (           |}
    ; {|                         |}
    ; {|                         |}
    ; {|                         |}
    ]
  ;;

  let bad_guess4 =
    [ {|   _____________         |}
    ; {| 〈             〉       |}
    ; {|   ‾‾‾‾‾‾‾‾‾‾‾‾‾         |}
    ; {|      \      //          |}
    ; {|       \   _oo\          |}
    ; {|          (__/ \         |}
    ; {|             \  \______  |}
    ; {|             (         ) |}
    ; {|              \_______/  |}
    ; {|                         |}
    ; {|                         |}
    ]
  ;;

  let bad_guess5 =
    [ {|   _____________         |}
    ; {| 〈             〉       |}
    ; {|   ‾‾‾‾‾‾‾‾‾‾‾‾‾         |}
    ; {|      \      //          |}
    ; {|       \   _oo\          |}
    ; {|          (__/ \  _  _   |}
    ; {|             \  \/ \/ \  |}
    ; {|             (         ) |}
    ; {|              \_______/  |}
    ; {|                         |}
    ; {|                         |}
    ]
  ;;

  let bad_guess6 =
    [ {|   _____________                              |}
    ; {| 〈             〉                            |}
    ; {|   ‾‾‾‾‾‾‾‾‾‾‾‾‾                              |}
    ; {|      \      //                               |}
    ; {|       \   _oo\                               |}
    ; {|          (__/ \  _  _                        |}
    ; {|             \  \/ \/ \                       |}
    ; {|             (         )\                     |}
    ; {|              \_______/  \                    |}
    ; {|               [[] [[]                        |}
    ; {|               [[] [[]  Art by cruzer@diku.dk |}
    ]
  ;;

  let states =
    [| initial; bad_guess1; bad_guess2; bad_guess3; bad_guess4; bad_guess5; bad_guess6 |]
    |> Array.map ~f:(List.map ~f:String.rstrip)
  ;;
end

let set_char ~(here : [%call_pos]) ~client ~buffer ~row ~col char =
  Buffer.set_text
    ~here
    client
    (Id buffer)
    ~start_row:row
    ~end_row:row
    ~start_col:col
    ~end_col:(col + 1)
    [ Char.to_string char ]
;;

let append_line ~(here : [%call_pos]) ~client ~buffer line =
  Buffer.set_lines
    ~here
    client
    (Id buffer)
    ~start:(-1)
    ~end_:(-1)
    ~strict_indexing:true
    [ line ]
;;

(* We require a blocking client here to ensure that no unrelated logic interleaves the
   drawing commands. *)
let draw_state
  ({ buffer; hint; bad_guesses; _ } as state)
  ~message
  ~(client : [ `blocking ] Client.t)
  =
  let open Deferred.Or_error.Let_syntax in
  let%bind () = Buffer.Option.set client (Id buffer) Readonly false in
  let%bind () = Buffer.Option.set client (Id buffer) Modifiable true in
  let%bind () =
    (* Draw the art. *)
    Buffer.set_lines
      client
      (Id buffer)
      ~start:0
      ~end_:(-1)
      ~strict_indexing:true
      Art.states.(Queue.length bad_guesses)
  in
  let%bind () =
    (* Draw the game instructions. *)
    match state.started_guessing with
    | true -> return ()
    | false ->
      Buffer.set_lines
        client
        (Id buffer)
        ~start:4
        ~end_:6
        ~strict_indexing:true
        [ " Try to guess the secret. Type a letter to guess."
        ; " As you guess incorrectly, an image will appear."
        ]
  in
  let%bind () =
    (* Draw the bad guesses. *)
    Deferred.Or_error.repeat_until_finished 0 (fun idx ->
      match idx = Queue.length bad_guesses with
      | true -> return (`Finished ())
      | false ->
        let%map () =
          let col = 5 + (2 * idx) in
          set_char ~client ~buffer ~row:1 ~col (Queue.get bad_guesses idx)
        in
        `Repeat (idx + 1))
  in
  let%bind () =
    (* Draw the hint. *)
    append_line ~client ~buffer (String.of_array hint)
  in
  let%bind () =
    match message with
    | None -> return ()
    | Some message -> append_line ~client ~buffer message
  in
  let%bind () = Buffer.Option.set client (Id buffer) Modified false in
  let%bind () = Buffer.Option.set client (Id buffer) Modifiable false in
  let%bind () = Buffer.Option.set client (Id buffer) Readonly true in
  return ()
;;

let guess ~hint ~secret char =
  let guessed_a_letter = ref false in
  for i = 0 to Array.length hint - 1 do
    if Char.equal char secret.[i]
    then (
      hint.(i) <- char;
      guessed_a_letter := true)
  done;
  !guessed_a_letter
;;

let remap_letter_keys ~(here : [%call_pos]) client ~state =
  let%tydi { buffer; secret; hint; bad_guesses; started_guessing = _ } = state in
  let iter_a_to_z ~f =
    Deferred.Or_error.repeat_until_finished 'a' (fun char ->
      let%map.Deferred.Or_error () = f char in
      match char with
      | 'z' -> `Finished ()
      | _ -> `Repeat (char |> Char.to_int |> Int.succ |> Char.of_int_exn))
  in
  iter_a_to_z ~f:(fun char ->
    Keymap.set
      ~here
      client
      ()
      ~mode:Normal
      ~nowait:true
      ~silent:true
      ~scope:(`Buffer_local (Id buffer))
      ~lhs:(Char.to_string char)
      ~rhs:
        (Ocaml_from_nvim.Callback.anon_rpc (fun ~run_in_background ~client ->
           let open Deferred.Or_error.Let_syntax in
           let char = Char.uppercase char in
           state.started_guessing <- true;
           match Queue.mem bad_guesses char ~equal:Char.equal with
           | true -> return ()
           | false ->
             let guessed_a_letter = guess ~hint ~secret char in
             if not guessed_a_letter then Queue.enqueue bad_guesses char;
             let message =
               match guessed_a_letter with
               | true ->
                 (match String.equal (String.of_array hint) secret with
                  | false -> None
                  | true -> Some "You guessed the secret!")
               | false ->
                 (match Queue.length bad_guesses = Array.length Art.states - 1 with
                  | false -> None
                  | true -> Some ("Failed to guess the secret: " ^ secret))
             in
             Option.iter message ~f:(fun _ ->
               run_in_background (fun client ->
                 let%bind () =
                   (* Disable guessing. *)
                   iter_a_to_z ~f:(fun char ->
                     Keymap.set
                       client
                       ~mode:Normal
                       ~nowait:true
                       ~silent:true
                       ~scope:(`Buffer_local (Id buffer))
                       ~lhs:(Char.to_string char)
                       ~rhs:(Viml "<Nop>")
                       ())
                 in
                 exit 0));
             draw_state state ~client ~message)))
;;

let on_startup client =
  let open Deferred.Or_error.Let_syntax in
  let cancelled = "<Esc>" in
  let%bind secret =
    Nvim.call_function
      client
      ~name:(`Viml "inputsecret")
      ~type_:Nvim.Func.(Dict @-> return String)
      ([ "prompt", Msgpack.String "Enter a word or phrase for the other player to guess: "
       ; "cancelreturn", String cancelled
       ]
       |> String.Map.of_alist_exn)
  in
  match String.equal secret cancelled with
  | true -> exit 0
  | false ->
    let secret =
      let secret = String.uppercase secret in
      match
        String.find secret ~f:(function
          | ' ' | 'A' .. 'Z' -> false
          | _ -> true)
      with
      | Some invalid_char ->
        raise_s [%message "Invalid secret" secret (invalid_char : char)]
      | None ->
        (* Clean up whitespace in the secret phrase. *)
        secret
        |> String.split ~on:' '
        |> List.filter ~f:(Fn.non String.is_empty)
        |> (function
         | [] -> failwith "Secret must have at least one letter"
         | words -> String.concat words ~sep:" ")
    in
    let%bind buffer = Buffer.create client ~listed:false ~scratch:true in
    let%bind () = Buffer.Option.set client (Id buffer) Bufhidden "wipe" in
    let hint = Array.create ~len:(String.length secret) '_' in
    ignore (guess ~hint ~secret ' ' : bool);
    let state =
      { buffer; secret; hint; bad_guesses = Queue.create (); started_guessing = false }
    in
    let%bind () = remap_letter_keys client ~state in
    let%bind () =
      block_nvim client ~f:(fun client -> draw_state state ~client ~message:None)
    in
    let%bind () = Command.exec client "sbuffer" ~range_or_count:(Count (buffer :> int)) in
    return state
;;

let get_buffer_rpc =
  Vcaml_plugin.Persistent.Rpc.create_blocking
    "buffer"
    ~type_:Ocaml_from_nvim.Blocking.(return Buffer)
    ~f:(fun { buffer; _ } ~run_in_background:_ ~client:_ ->
      Deferred.Or_error.return buffer)
;;

let command =
  Vcaml_plugin.Persistent.create
    ~name:"hangman"
    ~description:"Play hangman in Neovim"
    ~on_startup
    ~notify_fn:(`Lua "hangman_setup")
    [ get_buffer_rpc ]
;;
