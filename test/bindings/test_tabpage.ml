open Core
open Async
open Vcaml
open Vcaml_test_helpers

let%expect_test "list_wins, get_win" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = Command.exec [%here] client "new" in
      let%bind current_window = Tabpage.get_win [%here] client Current in
      let%map windows = Tabpage.list_wins [%here] client Current in
      print_s [%message (current_window : Window.t) (windows : Window.t list)])
  in
  [%expect {| ((current_window 1001) (windows (1001 1000))) |}];
  return ()
;;

let%expect_test "exists, get_number" =
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let%bind tab1 = Nvim.get_current_tab [%here] client in
      let%bind () = Command.exec [%here] client "tabnew" in
      let%bind tab2 = Nvim.get_current_tab [%here] client in
      let report_statuses () =
        let status tab =
          match%bind Tabpage.exists [%here] client tab with
          | false -> return [%message (tab : Tabpage.t) ~number:("-" : string)]
          | true ->
            let%bind number = Tabpage.get_number [%here] client (Id tab) in
            return [%message (tab : Tabpage.t) (number : int)]
        in
        let%bind status1 = status tab1 in
        let%map status2 = status tab2 in
        print_s [%message "" ~_:(status1 : Sexp.t) ~_:(status2 : Sexp.t)]
      in
      let%bind () = report_statuses () in
      let%bind () =
        (* Remove [tab1] so only [tab2] remains. *)
        Command.exec [%here] client "tabonly"
      in
      report_statuses ())
  in
  [%expect
    {|
    (((tab 1) (number 1)) ((tab 2) (number 2)))
    (((tab 1) (number -)) ((tab 2) (number 1)))
    |}];
  return ()
;;

let%expect_test "set_var, get_var, delete_var" =
  Backtrace.elide := true;
  let%bind () =
    with_client (fun client ->
      let%bind () =
        Tabpage.set_var [%here] client Current "foo" ~type_:Bool ~value:true >>| ok_exn
      in
      let%bind foo = Tabpage.get_var [%here] client Current "foo" ~type_:Bool in
      print_s [%message (foo : bool Or_error.t)];
      let%bind () = Tabpage.delete_var [%here] client Current "foo" >>| ok_exn in
      let%bind foo = Tabpage.get_var [%here] client Current "foo" ~type_:Bool in
      print_s [%message (foo : bool Or_error.t)];
      Deferred.Or_error.return ())
  in
  [%expect
    {|
    (foo (Ok true))
    (foo
     (Error
      (("Vim returned error" "Key not found: foo" (error_type Validation))
       (("Called from" lib/vcaml/test/bindings/test_tabpage.ml:LINE:COL)))))
    |}];
  Backtrace.elide := false;
  return ()
;;

(* This test demonstrates that setting the value of [Cmdheight] for a tab affects new
   tabs but does not affect previous tabs. *)
let%expect_test "Option.get, Option.set" =
  with_client (fun client ->
    let%bind () = Tabpage.Option.set [%here] client Cmdheight 2 >>| ok_exn in
    let%bind value = Tabpage.Option.get [%here] client Cmdheight in
    print_s [%sexp (value : int Or_error.t)];
    [%expect {| (Ok 2) |}];
    let%bind () = Command.exec [%here] client "tabnew" >>| ok_exn in
    let%bind value = Tabpage.Option.get [%here] client Cmdheight in
    print_s [%sexp (value : int Or_error.t)];
    [%expect {| (Ok 2) |}];
    let%bind () = Tabpage.Option.set [%here] client Cmdheight 1 >>| ok_exn in
    let%bind value = Tabpage.Option.get [%here] client Cmdheight in
    print_s [%sexp (value : int Or_error.t)];
    [%expect {| (Ok 1) |}];
    let%bind () =
      Command.exec [%here] client "tabnext" ~range_or_count:(Count 1) >>| ok_exn
    in
    let%bind value = Tabpage.Option.get [%here] client Cmdheight in
    print_s [%sexp (value : int Or_error.t)];
    [%expect {| (Ok 2) |}];
    Deferred.Or_error.return ())
;;
