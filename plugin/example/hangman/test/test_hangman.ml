open Core
open Async
open Vcaml
open Vcaml_test_helpers

let play_hangman ~secret ~guesses =
  with_ui_client
    ~links:
      [ "../hangman.lua", `In_temp_as, "hangman.lua"
      ; "../bin/main.exe", `In_path_as, "main.exe"
      ]
    (fun client ui ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = Command.exec client "source" ~args:[ "hangman.lua" ] in
      let%bind () = Command.exec client "Hangman" in
      let wait_for_text_and_print_screen ~(here : [%call_pos]) f =
        wait_until_text ~timeout:Time_ns.Span.second ~here ui ~f >>| print_endline
      in
      let guess char =
        let%bind () = Nvim.feedkeys client (`Raw (Char.to_string char)) ~mode:"m" in
        wait_for_text_and_print_screen (fun output ->
          let lines = String.split_lines output in
          let bad_guesses = List.nth_exn lines 2 in
          let hint = List.nth_exn lines 12 in
          let char = Char.uppercase char in
          String.mem bad_guesses char || String.mem hint char)
      in
      let%bind () =
        wait_for_text_and_print_screen
          (String.is_substring ~substring:"Enter a word or phrase")
      in
      let%bind (_ : int) = Nvim.Fast.input client [%string "%{secret}<CR>"] in
      let%bind () =
        wait_for_text_and_print_screen (fun screen ->
          String.is_substring screen ~substring:"___")
      in
      let%bind () = Deferred.Or_error.List.iter ~how:`Sequential guesses ~f:guess in
      return ())
;;

let%expect_test "winning game of hangman" =
  let%map () =
    play_hangman
      ~secret:"this is a test"
      ~guesses:[ 't'; 's'; 'i'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'g' (* no-op *); 'h' ]
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
    │Enter a word or phrase for the other player to guess:                           │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │   _____________                                                                │
    │ 〈             〉                                                              │
    │   ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                                │
    │                                                                                │
    │ Try to guess the secret. Type a letter to guess.                               │
    │ As you guess incorrectly, an image will appear.                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │____ __ _ ____                                                                  │
    │~                                                                               │
    │~                                                                               │
    │[Scratch] [RO]                                                1,1            All│
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
    │[No Name]                                                     0,0-1          All│
    │Enter a word or phrase for the other player to guess: **************            │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │   _____________                                                                │
    │ 〈             〉                                                              │
    │   ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │T___ __ _ T__T                                                                  │
    │~                                                                               │
    │~                                                                               │
    │[Scratch] [RO]                                                1,1            All│
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
    │[No Name]                                                     0,0-1          All│
    │Enter a word or phrase for the other player to guess: **************            │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │   _____________                                                                │
    │ 〈             〉                                                              │
    │   ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │T__S _S _ T_ST                                                                  │
    │~                                                                               │
    │~                                                                               │
    │[Scratch] [RO]                                                1,1            All│
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
    │[No Name]                                                     0,0-1          All│
    │Enter a word or phrase for the other player to guess: **************            │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │   _____________                                                                │
    │ 〈             〉                                                              │
    │   ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │T_IS IS _ T_ST                                                                  │
    │~                                                                               │
    │~                                                                               │
    │[Scratch] [RO]                                                1,1            All│
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
    │[No Name]                                                     0,0-1          All│
    │Enter a word or phrase for the other player to guess: **************            │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │   _____________                                                                │
    │ 〈             〉                                                              │
    │   ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │T_IS IS A T_ST                                                                  │
    │~                                                                               │
    │~                                                                               │
    │[Scratch] [RO]                                                1,1            All│
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
    │[No Name]                                                     0,0-1          All│
    │Enter a word or phrase for the other player to guess: **************            │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │   _____________                                                                │
    │ 〈 B           〉                                                              │
    │   ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                                │
    │                                                                                │
    │            oo                                                                  │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │T_IS IS A T_ST                                                                  │
    │~                                                                               │
    │~                                                                               │
    │[Scratch] [RO]                                                1,1            All│
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
    │[No Name]                                                     0,0-1          All│
    │Enter a word or phrase for the other player to guess: **************            │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │   _____________                                                                │
    │ 〈 B C         〉                                                              │
    │   ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                                │
    │      \      //                                                                 │
    │       \   _oo\                                                                 │
    │          (__/ \                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │T_IS IS A T_ST                                                                  │
    │~                                                                               │
    │~                                                                               │
    │[Scratch] [RO]                                                1,1            All│
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
    │[No Name]                                                     0,0-1          All│
    │Enter a word or phrase for the other player to guess: **************            │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │   _____________                                                                │
    │ 〈 B C D       〉                                                              │
    │   ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                                │
    │      \      //                                                                 │
    │       \   _oo\                                                                 │
    │          (__/ \                                                                │
    │             \  \                                                               │
    │             (                                                                  │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │T_IS IS A T_ST                                                                  │
    │~                                                                               │
    │~                                                                               │
    │[Scratch] [RO]                                                1,1            All│
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
    │[No Name]                                                     0,0-1          All│
    │Enter a word or phrase for the other player to guess: **************            │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │   _____________                                                                │
    │ 〈 B C D       〉                                                              │
    │   ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                                │
    │      \      //                                                                 │
    │       \   _oo\                                                                 │
    │          (__/ \                                                                │
    │             \  \                                                               │
    │             (                                                                  │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │T_IS IS A TEST                                                                  │
    │~                                                                               │
    │~                                                                               │
    │[Scratch] [RO]                                                1,1            All│
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
    │[No Name]                                                     0,0-1          All│
    │Enter a word or phrase for the other player to guess: **************            │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │   _____________                                                                │
    │ 〈 B C D F     〉                                                              │
    │   ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                                │
    │      \      //                                                                 │
    │       \   _oo\                                                                 │
    │          (__/ \                                                                │
    │             \  \______                                                         │
    │             (         )                                                        │
    │              \_______/                                                         │
    │                                                                                │
    │                                                                                │
    │T_IS IS A TEST                                                                  │
    │~                                                                               │
    │~                                                                               │
    │[Scratch] [RO]                                                1,1            All│
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
    │[No Name]                                                     0,0-1          All│
    │Enter a word or phrase for the other player to guess: **************            │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │   _____________                                                                │
    │ 〈 B C D F G   〉                                                              │
    │   ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                                │
    │      \      //                                                                 │
    │       \   _oo\                                                                 │
    │          (__/ \  _  _                                                          │
    │             \  \/ \/ \                                                         │
    │             (         )                                                        │
    │              \_______/                                                         │
    │                                                                                │
    │                                                                                │
    │T_IS IS A TEST                                                                  │
    │~                                                                               │
    │~                                                                               │
    │[Scratch] [RO]                                                1,1            All│
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
    │[No Name]                                                     0,0-1          All│
    │Enter a word or phrase for the other player to guess: **************            │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │   _____________                                                                │
    │ 〈 B C D F G   〉                                                              │
    │   ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                                │
    │      \      //                                                                 │
    │       \   _oo\                                                                 │
    │          (__/ \  _  _                                                          │
    │             \  \/ \/ \                                                         │
    │             (         )                                                        │
    │              \_______/                                                         │
    │                                                                                │
    │                                                                                │
    │T_IS IS A TEST                                                                  │
    │~                                                                               │
    │~                                                                               │
    │[Scratch] [RO]                                                1,1            All│
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
    │[No Name]                                                     0,0-1          All│
    │Enter a word or phrase for the other player to guess: **************            │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │   _____________                                                                │
    │ 〈 B C D F G   〉                                                              │
    │   ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                                │
    │      \      //                                                                 │
    │       \   _oo\                                                                 │
    │          (__/ \  _  _                                                          │
    │             \  \/ \/ \                                                         │
    │             (         )                                                        │
    │              \_______/                                                         │
    │                                                                                │
    │                                                                                │
    │THIS IS A TEST                                                                  │
    │You guessed the secret!                                                         │
    │~                                                                               │
    │[Scratch] [RO]                                                1,1            All│
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
    │[No Name]                                                     0,0-1          All│
    │Enter a word or phrase for the other player to guess: **************            │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}]
;;

let%expect_test "losing game of hangman" =
  let%map () = play_hangman ~secret:"wrong" ~guesses:[ 'a'; 'b'; 'c'; 'd'; 'e'; 'f' ] in
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
    │Enter a word or phrase for the other player to guess:                           │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │   _____________                                                                │
    │ 〈             〉                                                              │
    │   ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                                │
    │                                                                                │
    │ Try to guess the secret. Type a letter to guess.                               │
    │ As you guess incorrectly, an image will appear.                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │_____                                                                           │
    │~                                                                               │
    │~                                                                               │
    │[Scratch] [RO]                                                1,1            All│
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
    │[No Name]                                                     0,0-1          All│
    │Enter a word or phrase for the other player to guess: *****                     │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │   _____________                                                                │
    │ 〈 A           〉                                                              │
    │   ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                                │
    │                                                                                │
    │            oo                                                                  │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │_____                                                                           │
    │~                                                                               │
    │~                                                                               │
    │[Scratch] [RO]                                                1,1            All│
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
    │[No Name]                                                     0,0-1          All│
    │Enter a word or phrase for the other player to guess: *****                     │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │   _____________                                                                │
    │ 〈 A B         〉                                                              │
    │   ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                                │
    │      \      //                                                                 │
    │       \   _oo\                                                                 │
    │          (__/ \                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │_____                                                                           │
    │~                                                                               │
    │~                                                                               │
    │[Scratch] [RO]                                                1,1            All│
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
    │[No Name]                                                     0,0-1          All│
    │Enter a word or phrase for the other player to guess: *****                     │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │   _____________                                                                │
    │ 〈 A B C       〉                                                              │
    │   ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                                │
    │      \      //                                                                 │
    │       \   _oo\                                                                 │
    │          (__/ \                                                                │
    │             \  \                                                               │
    │             (                                                                  │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │_____                                                                           │
    │~                                                                               │
    │~                                                                               │
    │[Scratch] [RO]                                                1,1            All│
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
    │[No Name]                                                     0,0-1          All│
    │Enter a word or phrase for the other player to guess: *****                     │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │   _____________                                                                │
    │ 〈 A B C D     〉                                                              │
    │   ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                                │
    │      \      //                                                                 │
    │       \   _oo\                                                                 │
    │          (__/ \                                                                │
    │             \  \______                                                         │
    │             (         )                                                        │
    │              \_______/                                                         │
    │                                                                                │
    │                                                                                │
    │_____                                                                           │
    │~                                                                               │
    │~                                                                               │
    │[Scratch] [RO]                                                1,1            All│
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
    │[No Name]                                                     0,0-1          All│
    │Enter a word or phrase for the other player to guess: *****                     │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │   _____________                                                                │
    │ 〈 A B C D E   〉                                                              │
    │   ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                                │
    │      \      //                                                                 │
    │       \   _oo\                                                                 │
    │          (__/ \  _  _                                                          │
    │             \  \/ \/ \                                                         │
    │             (         )                                                        │
    │              \_______/                                                         │
    │                                                                                │
    │                                                                                │
    │_____                                                                           │
    │~                                                                               │
    │~                                                                               │
    │[Scratch] [RO]                                                1,1            All│
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
    │[No Name]                                                     0,0-1          All│
    │Enter a word or phrase for the other player to guess: *****                     │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    ╭────────────────────────────────────────────────────────────────────────────────╮
    │   _____________                                                                │
    │ 〈 A B C D E F 〉                                                              │
    │   ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                                │
    │      \      //                                                                 │
    │       \   _oo\                                                                 │
    │          (__/ \  _  _                                                          │
    │             \  \/ \/ \                                                         │
    │             (         )\                                                       │
    │              \_______/  \                                                      │
    │               [[] [[]                                                          │
    │               [[] [[]  Art by cruzer@diku.dk                                   │
    │_____                                                                           │
    │Failed to guess the secret: WRONG                                               │
    │~                                                                               │
    │[Scratch] [RO]                                                1,1            All│
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
    │[No Name]                                                     0,0-1          All│
    │Enter a word or phrase for the other player to guess: *****                     │
    ╰────────────────────────────────────────────────────────────────────────────────╯
    |}]
;;
