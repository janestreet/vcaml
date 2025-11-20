open Core
open Async
open Vcaml
open Vcaml_test_helpers

let%expect_test "create, delete, user_defined_commands" =
  with_client ~args:[ "--noplugin" ] (fun client ->
    let open Deferred.Or_error.Let_syntax in
    let test ~scope =
      let%bind () =
        Command.create
          client
          ~bar:true
          ~nargs:One
          ~completion:User
          ()
          ~name:"SayHi"
          ~scope
          (Viml [%string {|echo "Hi, ".<q-args>|}])
      in
      let%bind commands = Command.user_defined_commands client ~scope in
      print_s [%sexp (commands : Command.Definition.t Or_error.t String.Map.t)];
      let%bind () = Command.delete client "SayHi" ~scope in
      let%bind () =
        Command.create
          client
          ~nargs:At_most_one
          ~completion:(Custom { f = "CompleteFoobar" })
          ~range_or_count:(Range { default = All; of_ = Lines })
          ()
          ~name:"Foobar"
          ~scope
          (Viml [%string {|call Foobar(<line1>, <line2>)|}])
      in
      let%bind commands = Command.user_defined_commands client ~scope in
      print_s [%sexp (commands : Command.Definition.t Or_error.t String.Map.t)];
      return ()
    in
    let%bind () = test ~scope:`Global in
    [%expect
      {|
      ((SayHi
        (Ok
         ((name SayHi) (definition "echo \"Hi, \".<q-args>") (script_id -9)
          (keepscript false) (bang false) (bar true) (register false) (nargs One)
          (range_or_count ()) (completion (User))))))
      ((Foobar
        (Ok
         ((name Foobar) (definition "call Foobar(<line1>, <line2>)") (script_id -9)
          (keepscript false) (bang false) (bar false) (register false)
          (nargs At_most_one) (range_or_count ((Range (default All) (of_ Lines))))
          (completion ((Custom (f CompleteFoobar))))))))
      |}];
    let%bind () = test ~scope:(`Buffer_local Current) in
    [%expect
      {|
      ((SayHi
        (Ok
         ((name SayHi) (definition "echo \"Hi, \".<q-args>") (script_id -9)
          (keepscript false) (bang false) (bar true) (register false) (nargs One)
          (range_or_count ()) (completion (User))))))
      ((Foobar
        (Ok
         ((name Foobar) (definition "call Foobar(<line1>, <line2>)") (script_id -9)
          (keepscript false) (bang false) (bar false) (register false)
          (nargs At_most_one) (range_or_count ((Range (default All) (of_ Lines))))
          (completion ((Custom (f CompleteFoobar))))))))
      |}];
    return ())
;;

let%expect_test "exec" =
  with_client (fun client ->
    let%bind () =
      Buffer.set_lines
        client
        Current
        ~start:0
        ~end_:(-1)
        ~strict_indexing:true
        (List.init 10 ~f:(fun idx -> Int.to_string (idx + 1)))
      >>| ok_exn
    in
    let%bind () = Command.exec client ~range_or_count:(Count 3) "delete" >>| ok_exn in
    let show_buffer_lines () =
      Buffer.get_lines client Current ~start:0 ~end_:(-1) ~strict_indexing:true
      >>| ok_exn
      >>| Buffer.With_changedtick.value
      >>| List.map ~f:(fun line -> Int.of_string (String.Utf8.to_string line))
      >>| [%sexp_of: int list]
      >>| print_s
    in
    let%bind () = show_buffer_lines () in
    [%expect {| (4 5 6 7 8 9 10) |}];
    let%bind () =
      Command.exec
        client
        ~range_or_count:(Range { start_inclusive = 4; end_inclusive = 6 })
        ~register:'x'
        "yank"
      >>| ok_exn
    in
    let%bind () = Command.exec client ~bang:true ~register:'x' "put" >>| ok_exn in
    let%bind () = show_buffer_lines () in
    [%expect {| (7 8 9 4 5 6 7 8 9 10) |}];
    let%bind () =
      Command.exec_and_capture_output
        client
        ~filter:(Only "9")
        ~range_or_count:(Range { start_inclusive = 1; end_inclusive = 10 })
        "list"
      >>| ok_exn
      >>| print_endline
    in
    [%expect
      {|
      9
      9
      |}];
    Deferred.Or_error.return ())
;;

let%expect_test "parse" =
  with_client (fun client ->
    let open Deferred.Or_error.Let_syntax in
    let parse ~(here : [%call_pos]) cmd =
      let%map result = Command.Fast.parse ~here client cmd in
      print_s [%sexp (result : Command.Parse_result.t)]
    in
    let%bind () = parse "sandbox filter! /foo/ :let" in
    [%expect
      {|
      ((name let) (range_or_count ()) (bang false) (bar false)
       (register Not_applicable) (args ()) (expand_args false) (nargs Any)
       (nextcmd ())
       (modifiers
        ((filter ((Excluding foo))) (silent No) (unsilent false) (sandbox true)
         (noautocmd false) (browse false) (confirm false) (hide false)
         (horizontal false) (vertical false) (split ()) (keepalt false)
         (keepjumps false) (keepmarks false) (keeppatterns false) (lockmarks false)
         (noswapfile false) (tab ()) (verbose ()))))
      |}];
    let%bind () = parse "confirm write | quit" in
    [%expect
      {|
      ((name write) (range_or_count ()) (bang false) (bar true)
       (register Not_applicable) (args ()) (expand_args true) (nargs At_most_one)
       (nextcmd (quit))
       (modifiers
        ((filter ()) (silent No) (unsilent false) (sandbox false) (noautocmd false)
         (browse false) (confirm true) (hide false) (horizontal false)
         (vertical false) (split ()) (keepalt false) (keepjumps false)
         (keepmarks false) (keeppatterns false) (lockmarks false)
         (noswapfile false) (tab ()) (verbose ()))))
      |}];
    let%bind () = parse "3verbose 0tab sp" in
    [%expect
      {|
      ((name split) (range_or_count ()) (bang false) (bar true)
       (register Not_applicable) (args ()) (expand_args true) (nargs At_most_one)
       (nextcmd ())
       (modifiers
        ((filter ()) (silent No) (unsilent false) (sandbox false) (noautocmd false)
         (browse false) (confirm false) (hide false) (horizontal false)
         (vertical false) (split ()) (keepalt false) (keepjumps false)
         (keepmarks false) (keeppatterns false) (lockmarks false)
         (noswapfile false) (tab (0)) (verbose (3)))))
      |}];
    let%bind () = parse "silent! botright sp %" in
    [%expect
      {|
      ((name split) (range_or_count ()) (bang false) (bar true)
       (register Not_applicable) (args (%)) (expand_args true) (nargs At_most_one)
       (nextcmd ())
       (modifiers
        ((filter ()) (silent (Yes (silence_errors true))) (unsilent false)
         (sandbox false) (noautocmd false) (browse false) (confirm false)
         (hide false) (horizontal false) (vertical false) (split (Bottom_right))
         (keepalt false) (keepjumps false) (keepmarks false) (keeppatterns false)
         (lockmarks false) (noswapfile false) (tab ()) (verbose ()))))
      |}];
    let%bind () = parse "3d x" in
    [%expect
      {|
      ((name delete)
       (range_or_count ((Count (count 3) (of_ Lines) (source Specified))))
       (bang false) (bar true) (register (This x)) (args ()) (expand_args false)
       (nargs Zero) (nextcmd ())
       (modifiers
        ((filter ()) (silent No) (unsilent false) (sandbox false) (noautocmd false)
         (browse false) (confirm false) (hide false) (horizontal false)
         (vertical false) (split ()) (keepalt false) (keepjumps false)
         (keepmarks false) (keeppatterns false) (lockmarks false)
         (noswapfile false) (tab ()) (verbose ()))))
      |}];
    let%bind () =
      Buffer.set_lines
        client
        Current
        ~start:0
        ~end_:(-1)
        ~strict_indexing:true
        (List.init 10 ~f:Int.to_string)
    in
    let%bind () = Window.set_cursor client Current { row = 7; col = 0 } in
    let%bind () = parse ".,$d" in
    [%expect
      {|
      ((name delete)
       (range_or_count
        ((Range (start_inclusive 7) (end_inclusive 10) (of_ Lines))))
       (bang false) (bar true) (register Not_provided) (args ())
       (expand_args false) (nargs Zero) (nextcmd ())
       (modifiers
        ((filter ()) (silent No) (unsilent false) (sandbox false) (noautocmd false)
         (browse false) (confirm false) (hide false) (horizontal false)
         (vertical false) (split ()) (keepalt false) (keepjumps false)
         (keepmarks false) (keeppatterns false) (lockmarks false)
         (noswapfile false) (tab ()) (verbose ()))))
      |}];
    return ())
;;

let%expect_test "parse fails for nonexistent command" =
  with_client (fun client ->
    Dynamic.set_root Backtrace.elide true;
    let%bind () =
      match%map Command.Fast.parse client "nonexistent" with
      | Ok parse_result ->
        print_s
          [%message
            "Test unexpectedly didn't error" (parse_result : Command.Parse_result.t)]
      | Error error -> print_s [%sexp (error : Error.t)]
    in
    [%expect
      {|
      (("Vim returned error"
        "Error while parsing command line: E492: Not an editor command: nonexistent"
        (error_type Exception))
       (("Called from" lib/vcaml/test/bindings/test_command.ml:LINE:COL)))
      |}];
    Dynamic.set_root Backtrace.elide false;
    return (Ok ()))
;;

let%expect_test "parsing doesn't fail for a nonexistent nextcmd" =
  with_client (fun client ->
    let open Deferred.Let_syntax in
    let%bind () =
      match%map Command.Fast.parse client "split | nonexistent" with
      | Error error -> print_s [%message "Test unexpectedly errored" (error : Error.t)]
      | Ok parse_result -> print_s [%sexp (parse_result : Command.Parse_result.t)]
    in
    [%expect
      {|
      ((name split) (range_or_count ()) (bang false) (bar true)
       (register Not_applicable) (args ()) (expand_args true) (nargs At_most_one)
       (nextcmd (nonexistent))
       (modifiers
        ((filter ()) (silent No) (unsilent false) (sandbox false) (noautocmd false)
         (browse false) (confirm false) (hide false) (horizontal false)
         (vertical false) (split ()) (keepalt false) (keepjumps false)
         (keepmarks false) (keeppatterns false) (lockmarks false)
         (noswapfile false) (tab ()) (verbose ()))))
      |}];
    return (Ok ()))
;;

(* The tests below exercise the modeling of range/count command attributes. *)

module Range_or_count = struct
  include Command.Range_or_count

  module Of = struct
    include Of

    let all = [ Lines ]
  end

  module Spec = struct
    open Spec

    module Default_range = struct
      type t = Default_range.t =
        | Current
        | All
      [@@deriving enumerate]
    end

    module How_count_can_be_passed = struct
      type t = How_count_can_be_passed.t =
        | Only_in_line_number_position
        | In_line_number_position_or_as_first_argument
      [@@deriving enumerate]
    end

    let all_of_int = [ 0; 1 ]

    type nonrec t = t =
      | Range of
          { default : Default_range.t
          ; of_ : Of.t
          }
      | Count of
          { default : int
          ; can_be_passed : How_count_can_be_passed.t
          ; of_ : Of.t
          }
    [@@deriving enumerate]

    let to_string = function
      | Range { default = Current; of_ = _ } -> "-range"
      | Range { default = All; of_ = _ } -> "-range=%"
      | Count { default; can_be_passed = Only_in_line_number_position; of_ = _ } ->
        [%string "-range=%{default#Int}"]
      | Count
          { default
          ; can_be_passed = In_line_number_position_or_as_first_argument
          ; of_ = _
          } -> [%string "-count=%{default#Int}"]
    ;;

    let ill_defined_cases =
      [ "-range", "-count"
      ; "-range", "-count=1"
      ; "-range=%", "-count"
      ; "-range=%", "-count=1"
      ; "-count", "-range"
      ; "-count=1", "-range"
      ; "-count", "-range=%"
      ; "-count=1", "-range=%"
      ]
    ;;
  end
end

let%expect_test "[range_or_count] interpretation roundtrips in command definitions" =
  with_client (fun client ->
    let open Deferred.Or_error.Let_syntax in
    let name = "Foo" in
    let command = {|echo "foobar"|} in
    let scope = `Global in
    let test range_or_count =
      let%bind () =
        Command.create client ?range_or_count () ~name ~scope (Viml command)
      in
      let expected = Option.map range_or_count ~f:Range_or_count.Spec.to_string in
      let%map commands = Command.user_defined_commands client ~scope in
      let%tydi { range_or_count; _ } = Map.find_exn commands name |> ok_exn in
      let actual = Option.map range_or_count ~f:Range_or_count.Spec.to_string in
      print_s [%message (expected : string option) (actual : string option)]
    in
    let%bind () =
      Deferred.Or_error.List.iter
        ~how:`Sequential
        [%all: Range_or_count.Spec.t option]
        ~f:test
    in
    [%expect
      {|
      ((expected ()) (actual ()))
      ((expected (-range)) (actual (-range)))
      ((expected (-range=%)) (actual (-range=%)))
      ((expected (-range=0)) (actual (-range=0)))
      ((expected (-range=1)) (actual (-range=1)))
      ((expected (-count=0)) (actual (-count=0)))
      ((expected (-count=1)) (actual (-count=1)))
      |}];
    let test_ill_defined_case ~attr1 ~attr2 =
      let%bind () =
        Command.exec client ~bang:true "command" ~args:[ attr1; attr2; name; command ]
      in
      let expected = Some [%string "%{attr1} %{attr2}"] in
      let%map commands = Command.user_defined_commands client ~scope in
      let%tydi { range_or_count; _ } = Map.find_exn commands name |> ok_exn in
      let actual = Option.map range_or_count ~f:Range_or_count.Spec.to_string in
      print_s [%message (expected : string option) (actual : string option)]
    in
    let%bind () =
      Deferred.Or_error.List.iter
        ~how:`Sequential
        Range_or_count.Spec.ill_defined_cases
        ~f:(fun (attr1, attr2) -> test_ill_defined_case ~attr1 ~attr2)
    in
    (* This demonstrates the point in the documentation of [Range_or_count] that we assume
       the command is well-defined and model it as a command that takes a count. There is
       no way to reliably distinguish these cases from the [-count] ones. *)
    [%expect
      {|
      ((expected ("-range -count")) (actual (-count=0)))
      ((expected ("-range -count=1")) (actual (-count=1)))
      ((expected ("-range=% -count")) (actual (-count=0)))
      ((expected ("-range=% -count=1")) (actual (-count=1)))
      ((expected ("-count -range")) (actual (-count=0)))
      ((expected ("-count=1 -range")) (actual (-count=1)))
      ((expected ("-count -range=%")) (actual (-count=0)))
      ((expected ("-count=1 -range=%")) (actual (-count=1)))
      |}];
    return ())
;;

let%expect_test "Successfully exec a [-range=N] command by passing a count" =
  with_client (fun client ->
    let open Deferred.Or_error.Let_syntax in
    let%bind () =
      Command.create
        client
        ()
        ~range_or_count:
          (* We define the command to take a count but, per [:h command-count], this type
             of count is registered in Neovim with [-range=N] *)
          (Count
             { default = 0; can_be_passed = Only_in_line_number_position; of_ = Lines })
        ~name:"ShowCount"
        ~scope:`Global
        (Viml "echo <count>")
    in
    let%bind () =
      Command.exec_and_capture_output client ~range_or_count:(Count 1) "ShowCount"
      >>| print_endline
    in
    [%expect {| 1 |}];
    return ())
;;

let%expect_test "Compare native command invocations with range/count against structured \
                 invocations via the API"
  =
  with_client (fun client ->
    (* Set up conditions s.t. the default argument for [-range] is 100 and the default
       argument for [-range=%] is 200. This makes recognizing these values (or their
       indirect influence) in the output clearer. *)
    let%bind () =
      Buffer.set_lines
        client
        Current
        ~start:0
        ~end_:(-1)
        ~strict_indexing:true
        (List.init 200 ~f:(fun _ -> ""))
      >>| ok_exn
    in
    let%bind () = Window.set_cursor client Current { row = 100; col = 0 } >>| ok_exn in
    let module Test = struct
      module Input = struct
        type t =
          { range : int list [@sexp.list]
          ; arg : int option [@sexp.option]
          }
        [@@deriving sexp_of]
      end

      module Output = struct
        module Parse = struct
          module Range_or_count = struct
            type t = Command.Parse_result.Range_or_count.t

            let sexp_of_t : t -> Sexp.t = function
              | Range { start_inclusive; end_inclusive; of_ = _ } ->
                List
                  [ Atom "Range"
                  ; List
                      [ Atom [%string.global "%{start_inclusive#Int}"]
                      ; Atom [%string.global "%{end_inclusive#Int}"]
                      ]
                  ]
              | Count { count; source = _; of_ = _ } ->
                List [ Atom "Count"; Atom [%string.global "%{count#Int}"] ]
              | Ambiguous_count_or_singleton_range { value; of_ = _ } ->
                List
                  [ Atom "Ambiguous_count_or_singleton_range"
                  ; Atom [%string.global "%{value#Int}"]
                  ]
            ;;
          end

          type t =
            { range_or_count : Range_or_count.t option [@sexp.option]
            ; args : string list [@sexp.list]
            }
          [@@deriving sexp_of]
        end

        module Exec = struct
          type t =
            { line1 : int
            ; line2 : int
            ; range : int
            ; count : int
            }
          [@@deriving equal, sexp_of]
        end

        module Vim_error = struct
          let sexp_of_t t =
            let sexp = Error.sexp_of_t t in
            match sexp with
            | List
                [ List
                    [ Atom "Vim returned error"
                    ; error
                    ; List [ Atom "error_type"; Atom ("Exception" | "Validation") ]
                    ]
                ; _bt
                ] -> error
            | _ -> sexp
          ;;
        end

        module Or_vim_error = struct
          let sexp_of_t sexp_of_ok = function
            | Ok ok -> sexp_of_ok ok
            | Error error -> Vim_error.sexp_of_t error
          ;;
        end
      end
    end
    in
    let (test_inputs : Test.Input.t list) =
      [ { range = []; arg = None }
      ; { range = [ 2 ]; arg = None }
      ; { range = []; arg = Some 2 }
      ; { range = [ 2 ]; arg = Some 5 }
      ; { range = [ 2; 5 ]; arg = None }
      ; { range = [ 2; 5 ]; arg = Some 11 }
      ]
    in
    let test ~attrs =
      let name = "Foo" in
      let%bind () =
        Command.exec
          client
          ~bang:true
          "command"
          ~args:(attrs @ [ name; "echo '<line1>,<line2>,<range>,<count>'" ])
        >>| ok_exn
      in
      Deferred.List.iter
        test_inputs
        ~how:(`Max_concurrent_jobs 1)
        ~f:(fun ({ range; arg } as input) ->
          let exec_of_output output =
            match String.split output ~on:',' with
            | [ line1; line2; range; count ] ->
              Ok
                { Test.Output.Exec.line1 = Int.of_string line1
                ; line2 = Int.of_string line2
                ; range = Int.of_string range
                ; count = Int.of_string count
                }
            | _ -> Or_error.error_s [%message "Unexpected output" output]
          in
          let command =
            let range = range |> List.map ~f:Int.to_string |> String.concat ~sep:"," in
            let arg =
              match arg with
              | None -> ""
              | Some arg -> Int.to_string arg
            in
            [%string "%{range}%{name}%{arg}"]
          in
          let%bind parse =
            match%map Command.Fast.parse client command with
            | Error _ as error -> error
            | Ok { range_or_count; args; _ } ->
              Ok { Test.Output.Parse.range_or_count; args }
          in
          let%bind parse_exec =
            match parse with
            | Error _ -> return None
            | Ok { range_or_count; args } ->
              let (range_or_count : Range_or_count.t option) =
                match range_or_count with
                | None -> None
                | Some (Range { start_inclusive; end_inclusive; of_ = _ }) ->
                  Some (Range { start_inclusive; end_inclusive })
                | Some (Ambiguous_count_or_singleton_range { value; of_ = _ }) ->
                  (* This ambiguous case arises from [nvim_parse_cmd] returning a
                     singleton range and no count. While we don't know whether this value
                     should be interpreted as a count (if the command uses [-range=N]) or
                     a range (if the command uses [-range] or [-range=%]), we do know that
                     it was communicated in the [range] field rather than [count] field of
                     the output from [nvim_parse_cmd], so when invoking [exec] we pass the
                     value as a range ([exec] will collapse the 2-argument range into a
                     singleton range since the values are the same). *)
                  Some (Range { start_inclusive = value; end_inclusive = value })
                | Some (Count { count; source = _; of_ = _ }) ->
                  (* When the count is specified on the command line (indicated by
                     [source = `Specified]), [nvim_parse_cmd] populates both the [range]
                     and [count] fields (the range is always a singleton with the same
                     value as the count field). However, when we pass the count to
                     [nvim_cmd], we only populate the [count] field, not the [range]
                     field. This inconsistency means this case does not roundtrip, but it
                     is necessary to avoid a problem in the [-range -count] case. Those
                     commands, some of which are built-ins such as [:delete] and [:yank],
                     when passed a count, compute an effective range that starts from the
                     last value in the range (or the default [-range] value if not
                     provided) and ends at [start + count - 1]. So, the command [:delete3]
                     would delete 3 lines, starting from the current line. This works if
                     when we invoke [nvim_cmd] we only pass the count value. However, if
                     we pass [count = 3] and [range = [3]], then it will delete 3 lines
                     starting from line 3. *)
                  Some (Count count)
              in
              Command.exec_and_capture_output ?range_or_count client name ~args
              >>| Or_error.bind ~f:exec_of_output
              >>| Option.return
          in
          let%bind native_exec =
            Command.exec_and_capture_output
              client
              "execute"
              ~args:[ [%string {|"%{command}"|}] ]
            >>| Or_error.bind ~f:exec_of_output
          in
          match parse, parse_exec, native_exec with
          | Ok _, None, _ | Error _, Some _, _ -> assert false
          | Error parse, None, native_exec ->
            print_s
              [%message
                ""
                  (input : Test.Input.t)
                  (parse : Test.Output.Vim_error.t)
                  (native_exec : Test.Output.Exec.t Test.Output.Or_vim_error.t)];
            return ()
          | Ok parse, Some (Error parse_exec), native_exec ->
            print_s
              [%message
                ""
                  (input : Test.Input.t)
                  (parse : Test.Output.Parse.t)
                  (parse_exec : Test.Output.Vim_error.t)
                  (native_exec : Test.Output.Exec.t Test.Output.Or_vim_error.t)];
            return ()
          | Ok parse, Some (Ok parse_exec), Error native_exec ->
            print_s
              [%message
                "Native failed when API succeeded - this is surprising! (probably a bug)"
                  (input : Test.Input.t)
                  (parse : Test.Output.Parse.t)
                  (parse_exec : Test.Output.Exec.t)
                  (native_exec : Test.Output.Vim_error.t)];
            return ()
          | Ok parse, Some (Ok parse_exec), Ok native_exec ->
            (match [%equal: Test.Output.Exec.t] parse_exec native_exec with
             | true ->
               print_s
                 [%message
                   (input : Test.Input.t)
                     (parse : Test.Output.Parse.t)
                     ~exec:(parse_exec : Test.Output.Exec.t)];
               return ()
             | false ->
               (* The behavior of Neovim and VCaml is inconsistent. This likely arises
                  from the known inconsistency in the specified count case (see comment
                  above), but may also be an inconsistency in Neovim. The logic that
                  follows teases out the cause. *)
               let%bind parse_exec_faithful =
                 let%tydi { range_or_count; args } = parse in
                 (* Bypass the API to sidestep VCaml's modeling of commands so we know
                    that when this result departs from [native_exec], the inconsistency is
                    in Neovim, not in VCaml. *)
                 let maybe name var conv =
                   Option.map var ~f:(fun var -> name, conv var)
                 in
                 let range, count =
                   match range_or_count with
                   | None -> None, None
                   | Some (Range { start_inclusive; end_inclusive; of_ = _ }) ->
                     Some [ start_inclusive; end_inclusive ], None
                   | Some (Count { count; source = `Default_when_omitted; of_ = _ }) ->
                     None, Some count
                   | Some (Count { count; source = `Specified; of_ = _ }) ->
                     Some [ count ], Some count
                   | Some (Ambiguous_count_or_singleton_range { value; of_ = _ }) ->
                     Some [ value ], None
                 in
                 Nvim.call_function
                   client
                   ~name:(`Viml "nvim_cmd")
                   ~type_:Nvim.Func.(Dict @-> Dict @-> return String)
                   ([ Some ("cmd", Msgpack.String name)
                    ; Some ("args", Type.to_msgpack (Array String) args)
                    ; maybe "range" range (Type.to_msgpack (Array Int))
                    ; maybe "count" count (Type.to_msgpack Int)
                    ]
                    |> List.filter_opt
                    |> String.Map.of_alist_exn)
                   (String.Map.singleton "output" (Msgpack.Bool true))
                 >>| Or_error.bind ~f:exec_of_output
               in
               (match parse_exec_faithful with
                | Error parse_exec_faithful ->
                  print_s
                    [%message
                      "Both structured and native execs succeeded but faithful exec \
                       failed - this is surprising! (probably a bug)"
                        (input : Test.Input.t)
                        (parse : Test.Output.Parse.t)
                        (parse_exec : Test.Output.Exec.t)
                        (native_exec : Test.Output.Exec.t)
                        (parse_exec_faithful : Test.Output.Vim_error.t)];
                  return ()
                | Ok parse_exec_faithful ->
                  let parse_exec =
                    match [%equal: Test.Output.Exec.t] parse_exec_faithful parse_exec with
                    | true -> None
                    | false -> Some parse_exec
                  in
                  let native_exec =
                    match
                      [%equal: Test.Output.Exec.t] parse_exec_faithful native_exec
                    with
                    | true -> None
                    | false -> Some native_exec
                  in
                  let inconsistency_in =
                    match parse_exec, native_exec with
                    | None, None -> assert false (* We know they are different. *)
                    | Some _, None -> [ "VCaml" ]
                    | None, Some _ -> [ "Neovim" ]
                    | Some _, Some _ -> [ "VCaml"; "Neovim" ]
                  in
                  print_s
                    [%message.omit_nil
                      ""
                        ~input:(Some input : Test.Input.t option)
                        (parse : Test.Output.Parse.t)
                        (parse_exec : Test.Output.Exec.t option)
                        (parse_exec_faithful : Test.Output.Exec.t)
                        (native_exec : Test.Output.Exec.t option)
                        (inconsistency_in : string list)];
                  return ())))
    in
    (* Each [test] below runs through several different kinds of invocations of a command
       defined with certain attributes ([attrs]). [input] indicates how the command was
       invoked: [range] indicates the value passed in the range position (aka. "line
       number position", and [arg] indicates the first argument passed to the command
       (commands defined with the [-count] attribute are able to take the count as the
       first argument).

       [input] is turned into a command string and parsed with [Command.Fast.parse], which
       calls [nvim_parse_cmd]. [parse] shows the parsed range/count and the arguments,
       which can be compared against the [input] values. If [parse] succeeded,
       [parse_exec] contains the result of running the parsed command with [Command.exec],
       which calls [nvim_cmd]. [native_exec] contains the result of running the command
       string directly with [:execute command_string]. If either of the two commands fail,
       we show both results so they can be compared. If they both succeed and return the
       same result, we unify the result as [exec] and display it. If they both succeed but
       return different values, we want to figure out whether the reason there's a
       disagreement is due to Neovim's behavior or VCaml's behavior. We bypass Vcaml's
       more restricted [Command.exec] API to call Neovim directly with the [nvim_cmd]. We
       call the result of this [parse_exec_faithful] (it more faithfully reflects Neovim's
       API semantics). We then display the results from [parse_exec],
       [parse_exec_faithful], and [native_exec] that are in disagreement and state the
       source of the inconsistency. *)
    let%bind () = test ~attrs:[] in
    [%expect
      {|
      ((input ()) (parse ()) (exec ((line1 1) (line2 1) (range 0) (count -1))))
      ((input ((range (2))))
       (parse "Error while parsing command line: E481: No range allowed")
       (native_exec "Vim:E481: No range allowed: 2Foo"))
      ((input ((arg 2))) (parse ((args (2))))
       (parse_exec "Wrong number of arguments")
       (native_exec "Vim:E488: Trailing characters: 2: Foo2"))
      ((input ((range (2)) (arg 5)))
       (parse "Error while parsing command line: E481: No range allowed")
       (native_exec "Vim:E481: No range allowed: 2Foo5"))
      ((input ((range (2 5))))
       (parse "Error while parsing command line: E481: No range allowed")
       (native_exec "Vim:E481: No range allowed: 2,5Foo"))
      ((input ((range (2 5)) (arg 11)))
       (parse "Error while parsing command line: E481: No range allowed")
       (native_exec "Vim:E481: No range allowed: 2,5Foo11"))
      |}];
    let%bind () = test ~attrs:[ "-range" ] in
    [%expect
      {|
      ((input ()) (parse ()) (exec ((line1 100) (line2 100) (range 0) (count -1))))
      ((input ((range (2))))
       (parse ((range_or_count (Ambiguous_count_or_singleton_range 2))))
       (exec ((line1 2) (line2 2) (range 1) (count 2))))
      ((input ((arg 2))) (parse ((args (2))))
       (parse_exec "Wrong number of arguments")
       (native_exec "Vim:E488: Trailing characters: 2: Foo2"))
      ((input ((range (2)) (arg 5)))
       (parse ((range_or_count (Ambiguous_count_or_singleton_range 2)) (args (5))))
       (parse_exec "Wrong number of arguments")
       (native_exec "Vim:E488: Trailing characters: 5: 2Foo5"))
      ((input ((range (2 5)))) (parse ((range_or_count (Range (2 5)))))
       (exec ((line1 2) (line2 5) (range 2) (count 5))))
      ((input ((range (2 5)) (arg 11)))
       (parse ((range_or_count (Range (2 5))) (args (11))))
       (parse_exec "Wrong number of arguments")
       (native_exec "Vim:E488: Trailing characters: 11: 2,5Foo11"))
      |}];
    let%bind () = test ~attrs:[ "-range=%" ] in
    [%expect
      {|
      ((input ()) (parse ()) (exec ((line1 1) (line2 200) (range 0) (count -1))))
      ((input ((range (2))))
       (parse ((range_or_count (Ambiguous_count_or_singleton_range 2))))
       (exec ((line1 2) (line2 2) (range 1) (count 2))))
      ((input ((arg 2))) (parse ((args (2))))
       (parse_exec "Wrong number of arguments")
       (native_exec "Vim:E488: Trailing characters: 2: Foo2"))
      ((input ((range (2)) (arg 5)))
       (parse ((range_or_count (Ambiguous_count_or_singleton_range 2)) (args (5))))
       (parse_exec "Wrong number of arguments")
       (native_exec "Vim:E488: Trailing characters: 5: 2Foo5"))
      ((input ((range (2 5)))) (parse ((range_or_count (Range (2 5)))))
       (exec ((line1 2) (line2 5) (range 2) (count 5))))
      ((input ((range (2 5)) (arg 11)))
       (parse ((range_or_count (Range (2 5))) (args (11))))
       (parse_exec "Wrong number of arguments")
       (native_exec "Vim:E488: Trailing characters: 11: 2,5Foo11"))
      |}];
    let%bind () = test ~attrs:[ "-range=10" ] in
    (* The [(range (2 5))] input isn't valid, but there's no logic in Neovim or in VCaml
       that checks this, and both native and structured exec have the same behavior. *)
    [%expect
      {|
      ((input ()) (parse ()) (exec ((line1 100) (line2 100) (range 0) (count 10))))
      ((input ((range (2))))
       (parse ((range_or_count (Ambiguous_count_or_singleton_range 2))))
       (exec ((line1 2) (line2 2) (range 1) (count 2))))
      ((input ((arg 2))) (parse ((args (2))))
       (parse_exec "Wrong number of arguments")
       (native_exec "Vim:E488: Trailing characters: 2: Foo2"))
      ((input ((range (2)) (arg 5)))
       (parse ((range_or_count (Ambiguous_count_or_singleton_range 2)) (args (5))))
       (parse_exec "Wrong number of arguments")
       (native_exec "Vim:E488: Trailing characters: 5: 2Foo5"))
      ((input ((range (2 5)))) (parse ((range_or_count (Range (2 5)))))
       (exec ((line1 2) (line2 5) (range 2) (count 5))))
      ((input ((range (2 5)) (arg 11)))
       (parse ((range_or_count (Range (2 5))) (args (11))))
       (parse_exec "Wrong number of arguments")
       (native_exec "Vim:E488: Trailing characters: 11: 2,5Foo11"))
      |}];
    let%bind () = test ~attrs:[ "-count=10" ] in
    (* The [(range (2 5))], [(range 2) (arg 5)], and [(range (2 5)) (arg 11)] inputs
       aren't valid, but again, there's no logic in Neovim or in VCaml that checks this.

       Now we start to see inconsistencies in the invocations. The VCaml inconsistency is
       the known case discussed earlier - [(range (2))] passed to a [-count] command is a
       count passed in the line number position, so this is the count specified on the
       command line case. Because VCaml does not fill in the [range] value in [exec],
       <line1> assumes the default [-range] value, which is the same as what happens when
       the count is passed in the first argument position (see the next case in the test).

       We omit discussion of Neovim's own inconsistencies, since they do not indicate
       problems in VCaml. *)
    [%expect
      {|
      ((input ()) (parse ((range_or_count (Count 10))))
       (parse_exec_faithful ((line1 100) (line2 10) (range 1) (count 10)))
       (native_exec ((line1 100) (line2 1) (range 0) (count 10)))
       (inconsistency_in (Neovim)))
      ((input ((range (2)))) (parse ((range_or_count (Count 2))))
       (parse_exec ((line1 100) (line2 2) (range 1) (count 2)))
       (parse_exec_faithful ((line1 2) (line2 2) (range 1) (count 2)))
       (inconsistency_in (VCaml)))
      ((input ((arg 2))) (parse ((range_or_count (Count 2))))
       (exec ((line1 100) (line2 2) (range 1) (count 2))))
      ((input ((range (2)) (arg 5))) (parse ((range_or_count (Count 5))))
       (parse_exec ((line1 100) (line2 5) (range 1) (count 5)))
       (parse_exec_faithful ((line1 5) (line2 5) (range 1) (count 5)))
       (native_exec ((line1 2) (line2 5) (range 1) (count 5)))
       (inconsistency_in (VCaml Neovim)))
      ((input ((range (2 5)))) (parse ((range_or_count (Range (2 5)))))
       (exec ((line1 2) (line2 5) (range 2) (count 5))))
      ((input ((range (2 5)) (arg 11))) (parse ((range_or_count (Range (2 11)))))
       (exec ((line1 2) (line2 11) (range 2) (count 11))))
      |}];
    let%bind () = test ~attrs:[ "-range"; "-count=10" ] in
    [%expect
      {|
      ((input ()) (parse ((range_or_count (Count 10))))
       (parse_exec_faithful ((line1 100) (line2 109) (range 1) (count 109)))
       (native_exec ((line1 100) (line2 100) (range 0) (count 10)))
       (inconsistency_in (Neovim)))
      ((input ((range (2)))) (parse ((range_or_count (Count 2))))
       (parse_exec ((line1 100) (line2 101) (range 1) (count 101)))
       (parse_exec_faithful ((line1 2) (line2 3) (range 2) (count 3)))
       (native_exec ((line1 2) (line2 2) (range 1) (count 2)))
       (inconsistency_in (VCaml Neovim)))
      ((input ((arg 2))) (parse ((range_or_count (Count 101))))
       (parse_exec ((line1 100) (line2 200) (range 1) (count 200)))
       (parse_exec_faithful ((line1 101) (line2 200) (range 2) (count 200)))
       (native_exec ((line1 100) (line2 101) (range 1) (count 101)))
       (inconsistency_in (VCaml Neovim)))
      ((input ((range (2)) (arg 5))) (parse ((range_or_count (Range (2 6)))))
       (exec ((line1 2) (line2 6) (range 2) (count 6))))
      ((input ((range (2 5)))) (parse ((range_or_count (Range (2 5)))))
       (exec ((line1 2) (line2 5) (range 2) (count 5))))
      ((input ((range (2 5)) (arg 11))) (parse ((range_or_count (Range (5 15)))))
       (parse_exec_faithful ((line1 5) (line2 15) (range 2) (count 15)))
       (native_exec ((line1 5) (line2 15) (range 3) (count 15)))
       (inconsistency_in (Neovim)))
      |}];
    let%bind () = test ~attrs:[ "-range=%"; "-count=10" ] in
    [%expect
      {|
      ((input ()) (parse ((range_or_count (Count 10))))
       (parse_exec_faithful ((line1 200) (line2 200) (range 1) (count 200)))
       (native_exec ((line1 1) (line2 200) (range 0) (count 10)))
       (inconsistency_in (Neovim)))
      ((input ((range (2)))) (parse ((range_or_count (Count 2))))
       (parse_exec ((line1 200) (line2 200) (range 1) (count 200)))
       (parse_exec_faithful ((line1 2) (line2 3) (range 2) (count 3)))
       (native_exec ((line1 2) (line2 2) (range 1) (count 2)))
       (inconsistency_in (VCaml Neovim)))
      ((input ((arg 2))) (parse ((range_or_count (Count 201))))
       (exec ((line1 200) (line2 200) (range 1) (count 200))))
      ((input ((range (2)) (arg 5))) (parse ((range_or_count (Range (2 6)))))
       (exec ((line1 2) (line2 6) (range 2) (count 6))))
      ((input ((range (2 5)))) (parse ((range_or_count (Range (2 5)))))
       (exec ((line1 2) (line2 5) (range 2) (count 5))))
      ((input ((range (2 5)) (arg 11))) (parse ((range_or_count (Range (5 15)))))
       (parse_exec_faithful ((line1 5) (line2 15) (range 2) (count 15)))
       (native_exec ((line1 5) (line2 15) (range 3) (count 15)))
       (inconsistency_in (Neovim)))
      |}];
    let%bind () = test ~attrs:[ "-count=10"; "-range" ] in
    (* The VCaml inconsistency here is the same as in the [-count=10] case. *)
    [%expect
      {|
      ((input ()) (parse ((range_or_count (Count 10))))
       (parse_exec_faithful ((line1 100) (line2 10) (range 1) (count 10)))
       (native_exec ((line1 100) (line2 1) (range 0) (count 10)))
       (inconsistency_in (Neovim)))
      ((input ((range (2)))) (parse ((range_or_count (Count 2))))
       (parse_exec ((line1 100) (line2 2) (range 1) (count 2)))
       (parse_exec_faithful ((line1 2) (line2 2) (range 1) (count 2)))
       (inconsistency_in (VCaml)))
      ((input ((arg 2))) (parse ((range_or_count (Count 2))))
       (exec ((line1 100) (line2 2) (range 1) (count 2))))
      ((input ((range (2)) (arg 5))) (parse ((range_or_count (Count 5))))
       (parse_exec ((line1 100) (line2 5) (range 1) (count 5)))
       (parse_exec_faithful ((line1 5) (line2 5) (range 1) (count 5)))
       (native_exec ((line1 2) (line2 5) (range 1) (count 5)))
       (inconsistency_in (VCaml Neovim)))
      ((input ((range (2 5)))) (parse ((range_or_count (Range (2 5)))))
       (exec ((line1 2) (line2 5) (range 2) (count 5))))
      ((input ((range (2 5)) (arg 11))) (parse ((range_or_count (Range (2 11)))))
       (exec ((line1 2) (line2 11) (range 2) (count 11))))
      |}];
    let%bind () = test ~attrs:[ "-count=10"; "-range=%" ] in
    (* The VCaml inconsistency here is the same as in the [-count=10] case. *)
    [%expect
      {|
      ((input ()) (parse ((range_or_count (Count 10))))
       (parse_exec_faithful ((line1 1) (line2 10) (range 1) (count 10)))
       (native_exec ((line1 1) (line2 200) (range 0) (count 10)))
       (inconsistency_in (Neovim)))
      ((input ((range (2)))) (parse ((range_or_count (Count 2))))
       (parse_exec ((line1 1) (line2 2) (range 1) (count 2)))
       (parse_exec_faithful ((line1 2) (line2 2) (range 1) (count 2)))
       (inconsistency_in (VCaml)))
      ((input ((arg 2))) (parse ((range_or_count (Count 2))))
       (exec ((line1 1) (line2 2) (range 1) (count 2))))
      ((input ((range (2)) (arg 5))) (parse ((range_or_count (Count 5))))
       (parse_exec ((line1 1) (line2 5) (range 1) (count 5)))
       (parse_exec_faithful ((line1 5) (line2 5) (range 1) (count 5)))
       (native_exec ((line1 2) (line2 5) (range 1) (count 5)))
       (inconsistency_in (VCaml Neovim)))
      ((input ((range (2 5)))) (parse ((range_or_count (Range (2 5)))))
       (exec ((line1 2) (line2 5) (range 2) (count 5))))
      ((input ((range (2 5)) (arg 11))) (parse ((range_or_count (Range (2 11)))))
       (exec ((line1 2) (line2 11) (range 2) (count 11))))
      |}];
    Deferred.Or_error.return ())
;;
