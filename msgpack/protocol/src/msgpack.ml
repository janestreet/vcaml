open Core

module Internal = struct
  module Parser = Parser
  module Serializer = Serializer
end

module type Msgpackable = sig
  type t

  val of_msgpack : Message.t -> t Or_error.t
  val to_msgpack : t -> Message.t
end

include Message

let t_of_string = Parser.parse
let t_of_string_exn s = Or_error.ok_exn (Parser.parse s)
let string_of_t_exn = Serializer.message_to_string_exn

let pp =
  let open Stdlib.Format in
  let pp_print_list =
    let pp_sep formatter () =
      pp_print_cut formatter ();
      pp_print_string formatter ", "
    in
    fun ~break_lists ~open_ch ~close_ch pp_elem formatter xs ->
      if break_lists then pp_open_hvbox formatter 0;
      pp_print_char formatter open_ch;
      (match xs with
       | [] -> ()
       | _ :: _ ->
         pp_print_char formatter ' ';
         (pp_print_list ~pp_sep pp_elem) formatter xs;
         pp_print_space formatter ());
      pp_print_char formatter close_ch;
      if break_lists then pp_close_box formatter ()
  in
  let pp_print_map =
    let pp_key_value ~break_lists pp_key pp_value formatter (key, value) =
      pp_open_hvbox formatter 0;
      pp_open_hbox formatter ();
      pp_key ~break_lists:false formatter key;
      pp_close_box formatter ();
      pp_print_char formatter ':';
      pp_print_break formatter 1 4;
      pp_value ~break_lists formatter value;
      pp_close_box formatter ()
    in
    fun ~break_lists ~pp_key ~pp_value formatter xs ->
      pp_print_list
        ~break_lists
        ~open_ch:'{'
        ~close_ch:'}'
        (pp_key_value ~break_lists pp_key pp_value)
        formatter
        xs
  in
  fun ?pp_ext formatter t ->
    let rec pp ~break_lists formatter = function
      | Nil -> pp_print_string formatter "nil"
      | Int x -> Int.pp formatter x
      | Int64 x | Uint64 x -> Int64.pp formatter x
      | Bool x -> Bool.pp formatter x
      | Float x -> Float.pp formatter x
      | String x -> String.pp formatter x
      | Binary x -> Bytes.pp formatter x
      | Array xs ->
        pp_print_list
          ~break_lists
          ~open_ch:'['
          ~close_ch:']'
          (pp ~break_lists)
          formatter
          xs
      | Map xs -> pp_print_map ~break_lists ~pp_key:pp ~pp_value:pp formatter xs
      | Ext ext ->
        (match pp_ext with
         | None -> default_pp_ext ~break_lists formatter ext
         | Some pp_ext -> pp_ext formatter ext)
    and default_pp_ext ~break_lists formatter { Custom.type_id; data } =
      pp_print_map
        ~break_lists
        ~pp_key:pp
        ~pp_value:pp
        formatter
        [ String "type", Int type_id; String "data", String (Bytes.to_string data) ]
    in
    pp ~break_lists:true formatter t
;;

let%expect_test "Test the pretty printer" =
  let pp_ext formatter { Custom.type_id = _; data } =
    let open Stdlib.Format in
    assert (Int.equal (Bytes.length data) 1);
    pp_print_int formatter (Bytes.get data 0 |> Char.to_int)
  in
  let pp ?pp_ext t =
    let open Stdlib.Format in
    pp_set_geometry std_formatter ~max_indent:40 ~margin:91;
    pp ?pp_ext std_formatter t;
    pp_print_newline std_formatter ()
  in
  let test_array len = Array (List.init len ~f:(fun _ -> String "test")) in
  pp Nil;
  [%expect {| nil |}];
  pp (Int 0);
  [%expect {| 0 |}];
  pp (Int64 0L);
  [%expect {| 0 |}];
  pp (Uint64 0L);
  [%expect {| 0 |}];
  pp (Bool false);
  [%expect {| false |}];
  pp (Float 0.0);
  [%expect {| 0. |}];
  pp (String "test");
  [%expect {| "test" |}];
  pp (test_array 0);
  [%expect {| [] |}];
  pp (test_array 9);
  [%expect
    {| [ "test", "test", "test", "test", "test", "test", "test", "test", "test" ] |}];
  pp (test_array 12);
  [%expect
    {|
    [ "test"
    , "test"
    , "test"
    , "test"
    , "test"
    , "test"
    , "test"
    , "test"
    , "test"
    , "test"
    , "test"
    , "test"
    ]
    |}];
  pp (Map []);
  [%expect {| {} |}];
  pp (Map [ test_array 12, Bool false; String "hello", Int 10 ]);
  [%expect
    {|
    { [ "test", "test", "test", "test", "test", "test", "test", "test", "test", "test", "test", "test" ]:
          false
    , "hello": 10
    }
    |}];
  pp (Ext { type_id = 0; data = Bytes.of_string "\x41" });
  [%expect {| { "type": 0, "data": "A" } |}];
  pp ~pp_ext (Ext { type_id = 0; data = Bytes.of_string "\x41" });
  [%expect {| 65 |}];
  (* Now a practical example... *)
  Array
    [ Int 0
    ; Int 1
    ; String "nvim_call_atomic"
    ; Array
        [ Array [ String "nvim_command"; Array [ String "echo 'hi'" ] ]
        ; Array
            [ String "nvim_get_commands"; Array [ Map [ String "builtin", Bool false ] ] ]
        ; Array
            [ String "nvim_open_win"
            ; Array
                [ Ext { type_id = 0; data = Bytes.of_string "\x01" }
                ; Bool false
                ; Map
                    [ String "relative", String "editor"
                    ; String "width", Int 30
                    ; String "height", Int 20
                    ; String "zindex", Int 150
                    ; String "style", String "minimal"
                    ; String "border", String "rounded"
                    ; String "noautocmd", Bool true
                    ]
                ]
            ]
        ]
    ]
  |> pp ~pp_ext;
  [%expect
    {|
    [ 0
    , 1
    , "nvim_call_atomic"
    , [ [ "nvim_command", [ "echo 'hi'" ] ]
      , [ "nvim_get_commands", [ { "builtin": false } ] ]
      , [ "nvim_open_win"
        , [ 1
          , false
          , { "relative": "editor"
            , "width": 30
            , "height": 20
            , "zindex": 150
            , "style": "minimal"
            , "border": "rounded"
            , "noautocmd": true
            }
          ]
        ]
      ]
    ]
    |}]
;;
