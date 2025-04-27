open Core
open Async
open Vcaml
open Vcaml_test_helpers

let%expect_test "Events are typed correctly" =
  with_client (fun client ->
    let typed_events = Autocmd.Event.all in
    let%bind documented_events, missing_events =
      let%bind runtime =
        Nvim.eval_viml_expression client "$VIMRUNTIME" ~result_type:String >>| ok_exn
      in
      let%map tags = Reader.file_lines (runtime ^/ "doc/tags") in
      List.filter_map tags ~f:(fun line ->
        match String.split line ~on:'\t' with
        | [ ("Cmd-event" | "FileExplorer" | "UserGettingBored"); _; _ ] -> None
        | [ tag; "autocmd.txt"; _pattern ] ->
          (match Char.is_uppercase tag.[0] with
           | false -> None
           | true ->
             (match tag.[0] with
              | ('E' | 'W')
                when String.drop_prefix tag 1 |> Int.of_string_opt |> Option.is_some ->
                (* Omit tags for warnings and errors. *)
                None
              | _ -> Some tag))
        | [ tag; "lsp.txt"; _pattern ] ->
          (match Char.is_uppercase tag.[0] with
           | false -> None
           | true ->
             (match tag with
              (* [LspProgressUpdate] and [LspRequest] are not autocommands themselves -
                 they are valid patterns for the [User] autocmd. *)
              | "LSP" | "LspProgressUpdate" | "LspRequest" -> None
              | _ -> Some tag))
        | _ -> None)
      |> List.partition_map ~f:(fun event ->
        match Autocmd.Event.of_string event with
        | event -> First event
        | exception _ -> Second event)
    in
    let extra_events =
      Set.diff
        (Autocmd.Event.Set.of_list typed_events)
        (Autocmd.Event.Set.of_list documented_events)
    in
    print_s
      [%message.omit_nil
        (missing_events : string list) (extra_events : Autocmd.Event.Set.t)];
    [%expect {| () |}];
    Deferred.Or_error.return ())
;;

module Maybe_group = Comparable.Make_plain (struct
    type t = Autocmd.Group.t option [@@deriving compare, sexp_of]
  end)

let%expect_test "get, create, delete, clear" =
  with_client (fun client ->
    let open Deferred.Or_error.Let_syntax in
    let all_events = Nonempty_list.of_list_exn Autocmd.Event.all in
    let print_summary () =
      let%map autocmds =
        Autocmd.get client ()
        >>| Maybe_group.Map.of_list_with_key_multi ~get_key:(fun { Autocmd.group; _ } ->
          group)
        >>| Map.map ~f:(fun autocmds -> List.length autocmds)
      in
      autocmds
      |> Map.to_alist
      |> List.map ~f:(fun (group, count) ->
        [%message (group : Autocmd.Group.t option) (count : int)])
      |> [%sexp_of: Sexp.t list]
      |> print_s
    in
    let%bind () = print_summary () in
    (* This is a summary of the initial autocommands from the runtime. The output is
       stable modulo version upgrades, and changes due to upgrades are innocuous. *)
    [%expect
      {|
      (((group ()) (count 10)) ((group (1)) (count 1)) ((group (2)) (count 1))
       ((group (3)) (count 1)) ((group (4)) (count 1)) ((group (5)) (count 6))
       ((group (6)) (count 1)) ((group (7)) (count 72)) ((group (8)) (count 9))
       ((group (9)) (count 3)) ((group (10)) (count 48)) ((group (11)) (count 12))
       ((group (12)) (count 20)) ((group (13)) (count 64)) ((group (14)) (count 3))
       ((group (15)) (count 1)) ((group (17)) (count 1)))
      |}];
    let%bind () =
      Autocmd.get client ()
      >>| List.map ~f:(fun event -> event.Autocmd.group)
      >>| List.filter_opt
      >>| List.dedup_and_sort ~compare:Autocmd.Group.compare
      >>= Deferred.Or_error.List.iter ~how:`Sequential ~f:(fun group ->
        Autocmd.clear client () ~group:(`Group group) ~events:all_events)
    in
    let%bind () = print_summary () in
    [%expect {| (((group ()) (count 10))) |}];
    let%bind () = Autocmd.clear client () ~group:`Not_in_any_group ~events:all_events in
    let%bind () = print_summary () in
    [%expect {| () |}];
    let get_and_print ?group ?events ?patterns_or_buffer () =
      Autocmd.get client ?group ?events ?patterns_or_buffer ()
      >>| List.iter ~f:(fun autocmd -> print_s [%sexp (autocmd : Autocmd.t)])
    in
    let%bind group1 = Autocmd.Group.create client "MyGroup1" in
    let%bind group2 = Autocmd.Group.create client "MyGroup2" in
    let%bind autocmd1 =
      Autocmd.create
        client
        ~description:"First autocmd"
        ~once:true
        ~nested:true
        ~group:group1
        ~patterns_or_buffer:(Buffer Current)
        ~events:[ WinEnter ]
        (Viml "echo 'Hello!'")
    in
    let%bind (_ : Autocmd.Id.t) =
      Autocmd.create
        client
        ~description:"Second autocmd"
        ~group:group2
        ~patterns_or_buffer:(Patterns [ "*.lua" ])
        ~events:[ FileType ]
        (Viml "echo 'Hello, Lua!'")
    in
    let%bind (_ : Autocmd.Id.t) =
      Autocmd.create
        client
        ~description:"Third autocmd"
        ~group:group2
        ~patterns_or_buffer:(Patterns [ "*" ])
        ~events:[ BufWinEnter ]
        (Viml "echo 'BufWinEnter'")
    in
    let%bind () = get_and_print ~events:[ WinLeave ] () in
    [%expect {| |}];
    let%bind () = get_and_print ~events:[ WinEnter ] () in
    [%expect
      {|
      ((id (9)) (group (18)) (group_name (MyGroup1))
       (description ("First autocmd")) (event WinEnter)
       (pattern_or_buffer (Buffer 1)) (once true) (command "echo 'Hello!'"))
      |}];
    let%bind () = get_and_print ~group:group1 () in
    [%expect
      {|
      ((id (9)) (group (18)) (group_name (MyGroup1))
       (description ("First autocmd")) (event WinEnter)
       (pattern_or_buffer (Buffer 1)) (once true) (command "echo 'Hello!'"))
      |}];
    let%bind () = get_and_print ~group:group2 () in
    [%expect
      {|
      ((id (11)) (group (19)) (group_name (MyGroup2))
       (description ("Third autocmd")) (event BufWinEnter)
       (pattern_or_buffer (Pattern *)) (once false) (command "echo 'BufWinEnter'"))
      ((id (10)) (group (19)) (group_name (MyGroup2))
       (description ("Second autocmd")) (event FileType)
       (pattern_or_buffer (Pattern *.lua)) (once false)
       (command "echo 'Hello, Lua!'"))
      |}];
    let%bind () = get_and_print ~patterns_or_buffer:(Patterns [ "*.lua" ]) () in
    [%expect
      {|
      ((id (10)) (group (19)) (group_name (MyGroup2))
       (description ("Second autocmd")) (event FileType)
       (pattern_or_buffer (Pattern *.lua)) (once false)
       (command "echo 'Hello, Lua!'"))
      |}];
    let%bind () = get_and_print ~patterns_or_buffer:(Buffer Current) () in
    [%expect
      {|
      ((id (9)) (group (18)) (group_name (MyGroup1))
       (description ("First autocmd")) (event WinEnter)
       (pattern_or_buffer (Buffer 1)) (once true) (command "echo 'Hello!'"))
      |}];
    let%bind () =
      Autocmd.clear
        client
        ~patterns_or_buffer:(Patterns [ "*" ])
        ()
        ~group:(`Group group2)
        ~events:[ BufWinEnter; BufWinLeave ]
    in
    let%bind () = get_and_print () in
    [%expect
      {|
      ((id (10)) (group (19)) (group_name (MyGroup2))
       (description ("Second autocmd")) (event FileType)
       (pattern_or_buffer (Pattern *.lua)) (once false)
       (command "echo 'Hello, Lua!'"))
      ((id (9)) (group (18)) (group_name (MyGroup1))
       (description ("First autocmd")) (event WinEnter)
       (pattern_or_buffer (Buffer 1)) (once true) (command "echo 'Hello!'"))
      |}];
    let%bind () = Autocmd.Group.delete client group2 in
    let%bind () = get_and_print () in
    [%expect
      {|
      ((id (9)) (group (18)) (group_name (MyGroup1))
       (description ("First autocmd")) (event WinEnter)
       (pattern_or_buffer (Buffer 1)) (once true) (command "echo 'Hello!'"))
      |}];
    let%bind () = Autocmd.delete client autocmd1 in
    let%bind () = get_and_print () in
    [%expect {| |}];
    return ())
;;

let%expect_test "exec" =
  with_ui_client ~args:[ "--noplugin" ] (fun client ui ->
    let open Deferred.Or_error.Let_syntax in
    let%bind group = Autocmd.Group.create client "Test group" in
    let%bind (_ : Autocmd.Id.t) =
      Autocmd.create
        client
        ~description:"Say hello"
        ~group
        ~events:[ User ]
        ~patterns_or_buffer:(Buffer Current)
        (Viml {| echo "Hello!" |})
    in
    let%bind () =
      Autocmd.exec client () ~events:[ User ] ~patterns_or_buffer:(Buffer Current)
    in
    let%bind () = get_screen_contents ui >>| print_endline in
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
      │Hello!                                                                          │
      ╰────────────────────────────────────────────────────────────────────────────────╯
      |}];
    return ())
;;
