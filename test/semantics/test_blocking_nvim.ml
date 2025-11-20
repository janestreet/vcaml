open Core
open Async
open Vcaml
open Vcaml_test_helpers

(* Tests of VCaml's semantics for blocking Neovim. *)

let enable_slow_tests = false

let%expect_test "Client given to synchronous callback cannot be used outside callback" =
  Dynamic.set_root Backtrace.elide true;
  let%bind () =
    with_client (fun async_client ->
      let blocking_client = ref None in
      let function_name = "rpc" in
      let call_rpc ~(here : [%call_pos]) client =
        (Nvim.call_function
           ~here
           client
           ~name:(`Viml "rpcrequest")
           ~type_:Nvim.Func.(Int @-> String @-> return Nil))
          (Client.channel async_client)
          function_name
      in
      Ocaml_from_nvim.register_request_blocking
        (Connected async_client)
        ~name:function_name
        ~type_:Ocaml_from_nvim.Blocking.(return Nil)
        ~f:(fun ~run_in_background:_ ~client ->
          blocking_client := Some client;
          return (Ok ()));
      let%bind.Deferred.Or_error () = call_rpc async_client in
      let blocking_client = Option.value_exn !blocking_client in
      let%bind result = Nvim.exec_viml blocking_client "" in
      print_s [%message "Blocking client fails" (result : unit Or_error.t)];
      let%bind result = Nvim.exec_viml async_client "" in
      print_s [%message "Asynchronous client succeeds" (result : unit Or_error.t)];
      return (Ok ()))
  in
  [%expect
    {|
    ("Blocking client fails"
     (result
      (Error
       ("Called Neovim with an expired client. This probably happened because a reference to the client persisted beyond the scope of the callback. The client is not allowed to escape its context, but OCaml's type system cannot prevent it."
        (("Called from" lib/vcaml/test/semantics/test_blocking_nvim.ml:LINE:COL))))))
    ("Asynchronous client succeeds" (result (Ok ())))
    |}];
  Dynamic.set_root Backtrace.elide false;
  return ()
;;

let%expect_test "Client given to [block_nvim] cannot be used outside callback" =
  Dynamic.set_root Backtrace.elide true;
  let%bind () =
    with_client (fun async_client ->
      let blocking_client = ref None in
      let%bind.Deferred.Or_error () =
        block_nvim async_client ~f:(fun client ->
          blocking_client := Some client;
          return (Ok ()))
      in
      let blocking_client = Option.value_exn !blocking_client in
      let%bind result = Nvim.exec_viml blocking_client "" in
      print_s [%message "Blocking client fails" (result : unit Or_error.t)];
      let%bind result = Nvim.exec_viml async_client "" in
      print_s [%message "Asynchronous client succeeds" (result : unit Or_error.t)];
      return (Ok ()))
  in
  [%expect
    {|
    ("Blocking client fails"
     (result
      (Error
       ("Called Neovim with an expired client. This probably happened because a reference to the client persisted beyond the scope of the callback. The client is not allowed to escape its context, but OCaml's type system cannot prevent it."
        (("Called from" lib/vcaml/test/semantics/test_blocking_nvim.ml:LINE:COL))))))
    ("Asynchronous client succeeds" (result (Ok ())))
    |}];
  Dynamic.set_root Backtrace.elide false;
  return ()
;;

let%expect_test "Jobs started with async client or with [run_in_background] wait until \
                 after synchronous callback to run"
  =
  let%bind () =
    with_client (fun async_client ->
      let function_name = "rpc" in
      let async_client_job = ref (Deferred.never ()) in
      let background_client_job = ref (Deferred.never ()) in
      let print_job_status () =
        let async_client_job = Deferred.peek !async_client_job in
        let background_client_job = Deferred.peek !background_client_job in
        print_s
          [%message
            "Job status"
              (async_client_job : unit Or_error.t option)
              (background_client_job : unit Or_error.t option)]
      in
      let call_rpc ~(here : [%call_pos]) client =
        (Nvim.call_function
           ~here
           client
           ~name:(`Viml "rpcrequest")
           ~type_:Nvim.Func.(Int @-> String @-> return Nil))
          (Client.channel async_client)
          function_name
      in
      Ocaml_from_nvim.register_request_blocking
        (Connected async_client)
        ~name:function_name
        ~type_:Ocaml_from_nvim.Blocking.(return Nil)
        ~f:(fun ~run_in_background ~client:blocking_client ->
          let open Deferred.Or_error.Let_syntax in
          async_client_job := Nvim.exec_viml async_client "";
          run_in_background (fun client ->
            print_endline "Running in background.";
            background_client_job := Nvim.exec_viml client "";
            return ());
          let%bind () = Nvim.exec_viml blocking_client "" in
          (* At this point, since we are still in the blocking context, neither
             [async_client_job] nor [background_client_job] should be resolved. *)
          print_job_status ();
          print_endline "Done handling blocking request.";
          return ());
      let%bind.Deferred.Or_error () = call_rpc async_client in
      let%map.Deferred.Or_error () = !async_client_job
      and () = !background_client_job in
      print_job_status ())
  in
  [%expect
    {|
    ("Job status" (async_client_job ()) (background_client_job ()))
    Done handling blocking request.
    Running in background.
    ("Job status" (async_client_job ((Ok ()))) (background_client_job ((Ok ()))))
    |}];
  return ()
;;

(* 4.8K trials takes approximately 2 minutes to run. In practice it took up to 1.5K trials
   to observe the problem before it was addressed, so this should be sufficient to confirm
   the problem is resolved. *)
let%expect_test "Plugin shutdown during [run_in_background] does not interfere with RPC \
                 request"
  =
  let run () =
    Deferred.Or_error.repeat_until_finished 0 (function
      | 4800 -> Deferred.Or_error.return (`Finished ())
      | trial ->
        let%map.Deferred.Or_error () =
          Monitor.try_with_or_error (fun () ->
            with_client ~verbose:true (fun client ->
              let function_name = "rpc" in
              let%bind socket_client_channel =
                let client = Client.create ~name:"socket-client" ~on_error:`Raise in
                let%map client =
                  Private.attach_client client (Socket (`Address "socket")) >>| ok_exn
                in
                Ocaml_from_nvim.register_request_blocking
                  (Connected client)
                  ~name:function_name
                  ~type_:Ocaml_from_nvim.Blocking.(return Nil)
                  ~f:(fun ~run_in_background ~client:_ ->
                    let open Deferred.Or_error.Let_syntax in
                    run_in_background (fun client -> Client.close client |> Deferred.ok);
                    return ());
                Client.channel client
              in
              Nvim.call_function
                client
                ~name:(`Viml "rpcrequest")
                ~type_:Nvim.Func.(Int @-> String @-> return Nil)
                socket_client_channel
                function_name))
        in
        let (_ : string) = [%expect.output] in
        `Repeat (trial + 1))
    >>| ok_exn
  in
  let%bind () =
    match enable_slow_tests with
    | true -> run ()
    | false -> return ()
  in
  [%expect {| |}];
  return ()
;;

let%expect_test "Jobs started with async client wait until after [block_nvim] to run" =
  let%bind () =
    with_client (fun async_client ->
      let async_client_job = ref (Deferred.never ()) in
      let print_job_status () =
        let async_client_job = Deferred.peek !async_client_job in
        print_s [%message "Job status" (async_client_job : unit Or_error.t option)]
      in
      let%bind.Deferred.Or_error () =
        block_nvim async_client ~f:(fun blocking_client ->
          let open Deferred.Or_error.Let_syntax in
          async_client_job := Nvim.exec_viml async_client "";
          let%bind () = Nvim.exec_viml blocking_client "" in
          (* At this point, since we are still in the blocking context, [async_client_job]
             should not be resolved. *)
          print_job_status ();
          return ())
      in
      let%map.Deferred.Or_error () = !async_client_job in
      print_job_status ())
  in
  [%expect
    {|
    ("Job status" (async_client_job ()))
    ("Job status" (async_client_job ((Ok ()))))
    |}];
  return ()
;;

let%expect_test "The most recent blocking context has exclusive permission to run" =
  let result =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let factorial ~(here : [%call_pos]) client =
        (Nvim.call_function
           ~here
           client
           ~name:(`Viml "rpcrequest")
           ~type_:Nvim.Func.(Int @-> String @-> Int @-> return Int))
          (Client.channel client)
          "factorial"
      in
      let ivars = Array.init 6 ~f:(fun _ -> Ivar.create ()) in
      Ocaml_from_nvim.register_request_blocking
        (Connected client)
        ~name:"factorial"
        ~type_:Ocaml_from_nvim.Blocking.(Int @-> return Int)
        ~f:(fun ~run_in_background:_ ~client n ->
          Ivar.fill_exn ivars.(n) ();
          match n with
          | 0 -> return 1
          | _ ->
            let result = factorial client (n - 1) in
            let%bind () = Ivar.read ivars.(n - 1) |> Deferred.ok in
            let%bind () = Nvim.exec_viml client "" in
            (match Deferred.peek result with
             | Some (Ok result) -> return (n * result)
             | Some (Error _ as error) -> Deferred.return error
             | None ->
               Deferred.Or_error.error_s
                 [%message "Expected the result to be filled" ~called_from:(n : int)]));
      factorial client 5)
  in
  let%bind result = with_timeout Time_float.Span.second result in
  print_s [%sexp (result : [ `Result of int | `Timeout ])];
  [%expect {| (Result 120) |}];
  return ()
;;

let%expect_test "[block_nvim] does not run until it has permission" =
  let%bind () =
    with_client (fun client ->
      let locked = ref false in
      let block () =
        block_nvim client ~f:(fun client ->
          (* Only one [block_nvim] call runs at a time. *)
          assert (not !locked);
          locked := true;
          (* When [block_nvim] runs its client has permission to run. *)
          let%map.Deferred.Or_error () = Nvim.exec_viml client "" in
          locked := false)
      in
      List.init 10 ~f:(fun _ -> block ()) |> Deferred.Or_error.all_unit)
  in
  [%expect {| |}];
  return ()
;;

let%expect_test "Simultaneous requests are sequenced." =
  (* These constants were chosen to trigger failure fairly reliably if this case is not
     handled properly without taking too much time (~500ms) to run when it is. *)
  let num_rpcs_to_call_simultaneously = 50 in
  let num_trials = 100 in
  let%bind () =
    with_client (fun client ->
      let open Deferred.Or_error.Let_syntax in
      let calls = Queue.create () in
      let function_name = "rpc" in
      let call_rpc =
        (Nvim.call_function
           client
           ~name:(`Viml "rpcrequest")
           ~type_:Nvim.Func.(Int @-> String @-> Int @-> return Nil))
          (Client.channel client)
          function_name
      in
      Ocaml_from_nvim.register_request_blocking
        (Connected client)
        ~name:function_name
        ~type_:Ocaml_from_nvim.Blocking.(Int @-> return Nil)
        ~f:(fun ~run_in_background:_ ~client:_ id ->
          Queue.enqueue calls id;
          return ());
      Deferred.Or_error.repeat_until_finished num_trials (fun trial ->
        match trial with
        | 0 -> return (`Finished ())
        | _ ->
          let%bind () =
            List.init num_rpcs_to_call_simultaneously ~f:Fn.id
            |> Deferred.Or_error.List.iter ~how:`Parallel ~f:call_rpc
          in
          let calls =
            let lst = Queue.to_list calls in
            Queue.clear calls;
            lst
          in
          (match List.is_sorted_strictly calls ~compare with
           | true -> return (`Repeat (trial - 1))
           | false ->
             Deferred.Or_error.error_s
               [%message "Out-of-order RPC calls" (trial : int) ~_:(calls : int list)])))
  in
  [%expect {| |}];
  return ()
;;

(* This test lives here rather than in ./test_nvim_semantics.ml because returning results
   in non-LIFO order normally causes Neovim to close the connection, but VCaml has logic
   to prevent this from happening by reordering the responses. So this test is more a
   demonstration of VCaml's behavior than Neovim's. *)
let%expect_test "Nested calls that all return at the same time does not cause connection \
                 to close"
  =
  (* These constants were chosen to trigger failure fairly reliably if this case is not
     handled properly without taking too much time (~500ms) to run when it is. *)
  let num_rpcs_to_return_simultaneously = 50 in
  let num_trials = 100 in
  let succeeded = ref false in
  let%bind () =
    (* There was a run in which this test timed out and the timeout message wasn't
       sufficient to diagnose what happened. The issue may have been due to a bad timing
       setting, since the whole test was being timed instead of individual runs. We set
       [verbose] here so we can debug the failing scenario should it happen again. *)
    with_client ~verbose:true (fun client ->
      let bvar = Bvar.create () in
      let function_name = "rpc" in
      let call_rpc ~(here : [%call_pos]) client idx =
        (Nvim.call_function
           ~here
           client
           ~name:(`Viml "rpcrequest")
           ~type_:Nvim.Func.(Int @-> String @-> Int @-> return Nil))
          (Client.channel client)
          function_name
          idx
      in
      let nested_results = Queue.create () in
      Ocaml_from_nvim.register_request_blocking
        (Connected client)
        ~name:function_name
        ~type_:Ocaml_from_nvim.Blocking.(Int @-> return Nil)
        ~f:(fun ~run_in_background:_ ~client n ->
          let result = Bvar.wait bvar in
          (match n with
           | 0 -> Bvar.broadcast bvar (Ok ())
           | n -> Queue.enqueue nested_results (call_rpc client (n - 1)));
          result);
      let%bind result =
        Deferred.Or_error.repeat_until_finished num_trials (fun trial ->
          let open Deferred.Or_error.Let_syntax in
          match trial with
          | 0 -> return (`Finished ())
          | _ ->
            let result =
              let%bind () = call_rpc client num_rpcs_to_return_simultaneously in
              let%bind () =
                nested_results |> Queue.to_list |> Deferred.Or_error.all_unit
              in
              Queue.clear nested_results;
              let%bind (_ : Buffer.t) =
                (* Verify that the connection is still alive. *)
                Nvim.get_current_buf client
              in
              let (_ : string) = [%expect.output] in
              return ()
            in
            (match%bind.Deferred with_timeout Time_float.Span.second result with
             | `Result (Ok ()) -> return (`Repeat (trial - 1))
             | `Result (Error _ as error) -> Deferred.return error
             | `Timeout -> Deferred.Or_error.error_string "Timeout"))
      in
      Or_error.iter result ~f:(fun _ -> succeeded := true);
      return result)
  in
  (* There's some verbose output that follows [with_client] on connection close, and we
     want to avoid emitting that in the successful case. *)
  if !succeeded then ignore ([%expect.output] : string);
  [%expect {| |}];
  return ()
;;

(** An [Event_tag.t] tracks where an RPC request originated. If an RPC request is running
    with the tag [tag] and asks Neovim to send the plugin nested RPC requests, the first
    nested request the plugin receives will be tagged [tag @ [ 1 ]], then [tag @ [ 2 ]],
    and so on. Initially the empty tag is used, signifying a context in which Neovim is
    not blocked. *)
module Event_tag = struct
  type nonrec t = int list [@@deriving hash, compare, sexp]

  module Trie = struct
    include Trie.Of_list (Int)

    let empty = Trie.empty Keychain.keychainable
  end

  let nvim_type =
    Type.Custom
      (module struct
        type nonrec t = t

        let to_msgpack t = Msgpack.Array (List.map t ~f:(fun x -> Msgpack.Int x))

        let of_msgpack msgpack =
          let exception Failed_to_deserialize in
          match
            match (msgpack : Msgpack.t) with
            | Array ints ->
              List.map ints ~f:(function
                | Int x -> x
                | _ -> Exn.raise_without_backtrace Failed_to_deserialize)
            | _ -> Exn.raise_without_backtrace Failed_to_deserialize
          with
          | result -> Ok result
          | exception Failed_to_deserialize ->
            Or_error.error_s
              [%message
                "Failed to deserialize Msgpack as int list" ~_:(msgpack : Msgpack.t)]
        ;;
      end)
  ;;
end

(* Check where two tags diverge. E.g., tags (1 2 1 2) and (1 2 2 1) diverge at (1 2 1), (1
   2 2). If one is a prefix of the other, e.g., (1 2) and (1 2 2 1), the point of
   divergence includes the element after the prefix (1 2 2). *)
let get_point_of_divergence t1 t2 =
  let rec f t1 t2 ~prefix =
    match t1, t2 with
    | [], [] -> `Equal
    | [], y :: _ -> `First_prefixes_second (Reversed_list.rev (y :: prefix))
    | x :: _, [] -> `Second_prefixes_first (Reversed_list.rev (x :: prefix))
    | x :: xs, y :: ys ->
      (match x = y with
       | true -> f xs ys ~prefix:(x :: prefix)
       | false ->
         `Diverge_at (Reversed_list.rev (x :: prefix), Reversed_list.rev (y :: prefix)))
  in
  f t1 t2 ~prefix:[]
;;

(* Given a sequence of event tags representing calls to Neovim, return [true] iff no
   blocking RPCs were interleaved. We know an RPC was interleaved if two tags that share a
   prefix (meaning they represent commands from the same context) are interleaved by one
   or more tags that do not share that prefix. We maintain a trie that records all the
   prefixes we have expired so far (every time the prefix changes) so that if we encounter
   an expired prefix again we know there was an interleaving. *)
let is_valid_ordering ts =
  let retired_tags = ref Event_tag.Trie.empty in
  match ts with
  | [] -> true
  | hd :: tl ->
    let retire_tag tag =
      retired_tags := Trie.add_exn !retired_tags ~keychain:tag ~data:()
    in
    let check_tag tag =
      match Trie.mem !retired_tags tag with
      | true -> `Stop
      | false -> `Continue
    in
    List.fold_until
      tl
      ~init:hd
      ~finish:(fun _ -> true)
      ~f:(fun prev next ->
        match get_point_of_divergence prev next with
        | `Equal -> Continue next
        | `First_prefixes_second tag_to_check ->
          (match check_tag tag_to_check with
           | `Stop -> Stop false
           | `Continue -> Continue next)
        | `Second_prefixes_first tag_to_retire ->
          retire_tag tag_to_retire;
          Continue next
        | `Diverge_at (tag_to_retire, tag_to_check) ->
          (match check_tag tag_to_check with
           | `Stop -> Stop false
           | `Continue ->
             retire_tag tag_to_retire;
             Continue next))
;;

let%expect_test "Test the validation logic that the Quickcheck tests use" =
  let test event_tags =
    event_tags
    |> List.map ~f:Sexp.of_string
    |> List.map ~f:[%of_sexp: Event_tag.t]
    |> is_valid_ordering
    |> function
    | true -> print_endline "valid"
    | false -> print_endline "invalid"
  in
  test [];
  [%expect {| valid |}];
  test [ "(0)"; "(1 0)"; "(1 1)"; "(2)" ];
  [%expect {| valid |}];
  test [ "(0)"; "(1 1)"; "(1 0)"; "(2)" ];
  [%expect {| valid |}];
  test [ "(0)"; "()"; "(1 1)"; "(1 0)"; "()"; "()"; "(2)"; "()" ];
  [%expect {| valid |}];
  test
    [ "(0)"
    ; "(1 0)"
    ; "(1 1 0)"
    ; "(1 1 1)"
    ; "(1 1 2 0)"
    ; "(1 2 0)"
    ; "(1 2 1)"
    ; "(1 2 2 0)"
    ; "(1 2 2 1 0)"
    ; "(2)"
    ; "(3 0)"
    ];
  [%expect {| valid |}];
  test [ "(1)"; "(1 1)"; "(1)"; "(1 1)" ];
  [%expect {| invalid |}];
  test [ "(0)"; "(1 0)"; "(1 1 0)"; "(1 1 1)"; "(1 2 0)"; "(1 1 2 0)" ];
  [%expect {| invalid |}];
  test [ "(0)"; "(1 0)"; "(0)" ];
  [%expect {| invalid |}];
  test [ "(0)"; "(0 0)"; "(0 0 0)"; "(1)"; "(0 0)" ];
  [%expect {| invalid |}];
  return ()
;;

module Action = struct
  module Spec = struct
    (* Describes the abstracted semantics of a VCaml plugin. [Log] represents a call into
       Neovim to perform an action, and is used to record the [Event_tag.t] of the current
       context. [Call {await; spec}] represents a synchronous call to an RPC that will
       perform [spec], which is executed sequentially. If [await = true], the call will be
       sent as a request; if [await = false] it will be sent as a notification.
       [Parallel ts] runs [t]s in parallel. *)
    type t =
      | Log
      | Call of
          { await : bool
          ; spec : t list
          }
      | Parallel of t list
    [@@deriving quickcheck, sexp_of]

    module New = struct
      module Message_type = struct
        type t =
          | Request
          | Notification
        [@@deriving sexp_of]
      end

      (* Describes the abstracted semantics of a VCaml plugin.

         [Log] represents a request to Neovim to perform an action, and is used to record
         the [Event_tag.t] of the current context.

         [Sleep_plugin] inserts a [Clock_ns.after] for non-blocking sleep that can be used
         to delay an RPC's activity.

         [Call_neovim { name; how; specs }] represents a request or notification (given by
         [how]) to Neovim to call RPC [name], which does [specs]. Those specifications can
         be [Sleep_neovim], which puts Neovim to sleep, or [Call_plugin], which causes
         Neovim to issue a nested RPC call back to the plugin. [Sleep_neovim] has two
         varieties - blocking and non-blocking. Blocking prevents Neovim from doing
         anything else, and simulates Neovim being busy. Non-blocking invokes the `:sleep`
         command, which suspends the current job and allows others to run in the meantime.

         [Parallel plugin_specs] runs [plugin_spec]s in parallel. There isn't a natural
         equivalent on the Neovim side. *)
      type plugin_spec =
        | Log
        | Sleep_plugin of Time_ns.Span.t
        | Call_neovim of
            { name : string
            ; how : Message_type.t
            ; specs : neovim_spec list
            }
        | Parallel of plugin_spec list

      and neovim_spec =
        | Sleep_neovim of
            { milliseconds : int
            ; blocking : bool
            }
        | Call_plugin of
            { name : string
            ; how : Message_type.t
            ; specs : plugin_spec list
            }
      [@@deriving sexp_of]
    end

    let to_new plugin_spec =
      let rpc_id = ref 0 in
      let rec f = function
        | Log -> New.Log
        | Parallel plugin_specs -> Parallel (List.map plugin_specs ~f)
        | Call { await; spec } ->
          incr rpc_id;
          let neovim_rpc_name = [%string "Rpc_%{!rpc_id#Int}"] in
          let plugin_rpc_name = [%string "rpc-%{!rpc_id#Int}"] in
          Call_neovim
            { name = neovim_rpc_name
            ; how =
                (match await with
                 | true -> Request
                 | false -> Notification)
            ; specs =
                [ Call_plugin
                    { name = plugin_rpc_name; how = Request; specs = List.map spec ~f }
                ]
            }
      in
      f plugin_spec
    ;;
  end

  (* The translation of a [Spec.t] that can actually be run. The interesting
     transformation here is the [Call_neovim] constructor. For a given [Call_neovim], we:

     {v
       1. Register fresh RPCs with Neovim for each RPC in [Call_plugin]. The RPC is
          async or blocking depending on [how] it's called.
       2. For each [Call_neovim], send Neovim a request or notification (depending on the
          [how] in [Call_neovim]) asking it to call each [Call_plugin] in [specs] with
          [rpcrequest] or [rpcnotify] (depending on the [how] in [Call_plugin]).
       3. Within the handler of the RPC, we:
          i.  Add another layer of nesting in the [event_tag] for the recursive blocking
              calls.
          ii. Recursively perform all the [t] corresponding to the [Spec.t] in the
              definition of this RPC.
     v} *)
  type t =
    | Log
    | Sleep of Time_ns.Span.t
    | Call_neovim of
        { f :
            'kind.
            here:[%call_pos] -> 'kind Client.t -> Event_tag.t -> unit Deferred.Or_error.t
        }
    | Parallel of t list
  [@@deriving sexp_of]
end

let run_spec ?warn_if_neovim_exits_early ?verbose spec =
  let rpcs_finished = ref [] in
  let fresh_event_tag =
    let event_tags = ref Event_tag.Trie.empty in
    fun ~prefix ->
      let fresh_tag = Set_once.create () in
      event_tags
      := Trie.update_trie !event_tags prefix ~f:(fun trie ->
           let children = Trie.num_children trie in
           let keychain = [ children + 1 ] in
           Set_once.set_exn fresh_tag (prefix @ keychain);
           Trie.add_exn trie ~keychain ~data:());
      Set_once.get_exn fresh_tag
  in
  let rec run_action client event_tag action =
    match (action : Action.t) with
    | Log ->
      (* This was originally written as two commands - [Nvim.set_var] followed by
         [Nvim.command] - but we were able to produce cases where those two commands were
         interleaved. While we do want to test interleaving, we don't want the recording
         mechanism by which we test interleaving to be interleaved itself, because that
         can produce misleading records. Therefore, we make sure this is done in a single
         command. *)
      let event_tag =
        event_tag
        |> List.map ~f:Int.to_string
        |> String.concat ~sep:", "
        |> sprintf "[ %s ]"
      in
      Nvim.exec_viml client [%string "let g:event_log += [%{event_tag}]"]
    | Sleep span -> Clock_ns.after span |> Deferred.ok
    | Call_neovim { f } -> f client event_tag
    | Parallel actions ->
      Deferred.Or_error.List.iter actions ~how:`Parallel ~f:(fun action ->
        run_action client event_tag action)
  in
  with_client ?warn_if_neovim_exits_early ?verbose (fun client ->
    let channel = Client.channel client in
    let%bind action =
      let rec implement_neovim_spec = function
        | Action.Spec.New.Sleep_neovim { milliseconds; blocking } ->
          let command =
            match blocking with
            | false -> [%string "sleep %{milliseconds#Int}m"]
            | true ->
              let seconds = Float.of_int milliseconds /. 1000. in
              (* The [xargs] hack here works around a bug in Neovim 0.9.1 where passing a
                 float argument to [system] triggers "E806: using Float as a String". In
                 Neovim 0.10.0 we should be able to call [sleep] directly. *)
              [%string {| call system(["xargs", "sleep"], "%{seconds#Float}") |}]
          in
          return command
        | Call_plugin { name; how; specs } ->
          let%map actions =
            Deferred.List.map ~how:`Sequential specs ~f:implement_plugin_spec
          in
          let rpc_finished = Ivar.create () in
          rpcs_finished := Ivar.read rpc_finished :: !rpcs_finished;
          (match how with
           | Request ->
             Ocaml_from_nvim.register_request_blocking
               (Connected client)
               ~name
               ~type_:Ocaml_from_nvim.Blocking.(Event_tag.nvim_type @-> return Nil)
               ~f:(fun ~run_in_background:_ ~client prefix ->
                 let event_tag = fresh_event_tag ~prefix in
                 let%bind result =
                   Deferred.Or_error.List.iter ~how:`Sequential actions ~f:(fun action ->
                     run_action client event_tag action)
                 in
                 Ivar.fill_exn rpc_finished ();
                 return result);
             [%string {| call rpcrequest(%{channel#Int}, "%{name}", a:event_tag) |}]
           | Notification ->
             Ocaml_from_nvim.register_request_async
               (Connected client)
               ~name
               ~type_:Ocaml_from_nvim.Async.unit
               ~f:(fun ~client ->
                 let%bind result =
                   Deferred.Or_error.List.iter ~how:`Sequential actions ~f:(function
                     | Log ->
                       (* Print rather than raise because VCaml will catch the exception
                          and display it in Neovim. *)
                       print_s
                         [%message
                           "Encountered [Log] in an async RPC. This scenario is invalid, \
                            as async RPCs do not enjoy non-interleaving properties."];
                       Deferred.Or_error.return ()
                     | action -> run_action client [] action)
                 in
                 Ivar.fill_exn rpc_finished ();
                 return result);
             [%string {| call rpcnotify(%{channel#Int}, "%{name}") |}])
      and implement_plugin_spec = function
        | Action.Spec.New.Log -> return Action.Log
        | Sleep_plugin span -> return (Action.Sleep span)
        | Parallel specs ->
          let%map specs =
            Deferred.List.map specs ~how:`Sequential ~f:implement_plugin_spec
          in
          Action.Parallel specs
        | Call_neovim { name; how; specs } ->
          let%bind function_body =
            Deferred.List.map specs ~how:`Sequential ~f:implement_neovim_spec
          in
          let%map () =
            [ [ [%string "function %{name}(event_tag)"] ]
            ; function_body
            ; [ "return v:null"; "endfunction" ]
            ]
            |> List.concat
            |> String.concat ~sep:"\n"
            |> Nvim.exec_viml client
            >>| ok_exn
          in
          (match how with
           | Request ->
             Action.Call_neovim
               { f =
                   (fun ~(here : [%call_pos]) client event_tag ->
                     Nvim.call_function
                       ~here
                       client
                       ~name:(`Viml name)
                       ~type_:Nvim.Func.(Event_tag.nvim_type @-> return Nil)
                       event_tag)
               }
           | Notification ->
             Call_neovim
               { f =
                   (fun ~(here : [%call_pos]) client event_tag ->
                     let open Expert.Notifier in
                     notify
                       ~here
                       client
                       ~name:(`Viml name)
                       ~type_:Func.(Event_tag.nvim_type @-> unit)
                       event_tag)
               })
      in
      implement_plugin_spec spec
    in
    let result =
      let open Deferred.Or_error.Let_syntax in
      let event_log_type = Type.Array Event_tag.nvim_type in
      let%bind () = Nvim.set_var client "event_log" ~type_:event_log_type ~value:[] in
      let%bind () = run_action client [] action in
      let%bind () = Deferred.List.all_unit !rpcs_finished |> Deferred.ok in
      Nvim.get_var client "event_log" ~type_:event_log_type
    in
    match%map with_timeout (Time_float.Span.of_int_sec 3) result with
    | `Result result -> result
    | `Timeout -> Or_error.error_string "Timed out!")
;;

let run_spec_and_check_result ?warn_if_neovim_exits_early ?verbose spec =
  let%map event_tags = run_spec ?warn_if_neovim_exits_early ?verbose spec in
  match is_valid_ordering event_tags with
  | true -> ()
  | false ->
    raise_s [%message "Invalid event ordering" ~_:(event_tags : Event_tag.t list)]
;;

module For_fuzz_testing = struct
  module Scenario = Action.Spec

  let invariant ~verbose scenario =
    run_spec_and_check_result ~verbose (Scenario.to_new scenario)
  ;;
end

let%expect_test "Simple tests of running specifications" =
  let test ?verbose spec =
    let%map events = run_spec ?verbose (Action.Spec.to_new spec) in
    List.iter events ~f:(fun event -> print_s [%sexp (event : Event_tag.t)])
  in
  let call ~await spec = Action.Spec.Call { await; spec } in
  let%bind () = test Log in
  [%expect {| () |}];
  let%bind () = test (call ~await:true [ Log; Log ]) in
  [%expect
    {|
    (1)
    (1)
    |}];
  let%bind () = test (Parallel [ Log ]) in
  [%expect {| () |}];
  let%bind () =
    let rpc = call ~await:false [ Log; Log; Log ] in
    test (call ~await:true [ rpc; rpc; rpc ])
  in
  [%expect
    {|
    (1 1)
    (1 1)
    (1 1)
    (1 2)
    (1 2)
    (1 2)
    (1 3)
    (1 3)
    (1 3)
    |}];
  return ()
;;

(* This test demonstrated a bug due to a failure to reattempt flushing before invoking a
   blocking RPC. Originally we thought that the reattempts were only needed when returning
   a response, but this test showed that that was wrong.

   The plugin sends two notifications calling Rpc_1 and Rpc_2 respectively. Rpc_1 requests
   rpc-1 from the plugin. Neovim interprets the notification calling Rpc_2 as nested, and
   spends a millisecond being busy. Meanwhile, the plugin receives the request for rpc-1
   and sends a dummy request to flush Neovim's event loop. Then Neovim moves on to the
   next item in Rpc_2, which is to request rpc-2 from the plugin. The plugin receives the
   request for rpc-2 and interprets it as a nested call. It sends a dummy for rpc-2.
   Neovim receives the dummy for rpc-1 and then rpc-2 and responds to both. The plugin
   receives both dummy responses, and since rpc-2 has permission to run, it runs (doing
   nothing) and returns. The plugin flushes to see if it's safe to return the response; it
   is, and the plugin returns the response. The plugin then returns permission to run to
   rpc-1, which sends a [Log] request. Meanwhile, when Neovim received the response to
   rpc-2, it moved on to requesting rpc-3. The plugin receives the request for rpc-3,
   takes permission to run from rpc-1, and runs the [Log] for rpc-3. When rpc-3 is done,
   permission is returned again to rpc-1. Thus rpc-1 is interleaved by rpc-3.

   The purpose of keeping Neovim busy is that if Neovim immediately called rpc-2 after
   calling rpc-1, the plugin may seize permission to run from rpc-1 before sending the
   dummy for it, thereby postponing the dummy until after returning from rpc-2 and
   avoiding the problem. *)
let%expect_test "Regression test: need to re-flush before invoking RPC" =
  let%bind () =
    run_spec_and_check_result
      (Parallel
         [ Call_neovim
             { name = "Rpc_1"
             ; how = Notification
             ; specs =
                 [ Call_plugin { name = "rpc-1"; how = Request; specs = [ Log; Log ] } ]
             }
         ; Call_neovim
             { name = "Rpc_2"
             ; how = Notification
             ; specs =
                 [ Sleep_neovim { milliseconds = 1; blocking = true }
                 ; Call_plugin { name = "rpc-2"; how = Request; specs = [] }
                 ; Call_plugin { name = "rpc-3"; how = Request; specs = [ Log ] }
                 ]
             }
         ])
  in
  [%expect {| |}];
  return ()
;;

(* Demonstrate a bug that can arise due to Neovim's [sleep] defeating VCaml's flushing.
   The plugin requests Rpc_1, leading Neovim to request rpc-1 and await a response. rpc-1
   sends a notification invoking Rpc_2, which sleeps. Because it was a notification, rpc-1
   does not await a response, so it's ready to return. The plugin flushes Neovim's event
   loop with a dummy request, and that request returns successfully because sleep removed
   Rpc_2 from the loop. Then Rpc_2 wakes from sleep and rpcrequests rpc-2. Neovim returns
   the result to rpc-1, which is an out-of-order response, so Neovim closes the
   connection. *)
let%expect_test "Demonstrate disconnect induced by sleep" =
  Dynamic.set_root Backtrace.elide true;
  (* This low-level hook in the VCaml client is needed to demonstrate this scenario
     because the timing needs to be coordinated delicately. The specific sequence of
     events that needs to happen is:

     1. The dummy request sent to determine whether it's safe to return rpc-1 returns
        without any RPCs being invoked in the interim, indicating that it's safe.
     2. Neovim wakes and requests rpc-2.
     3. The plugin sends the response to rpc-1.

     If Rpc_2 sleeps for too long before sending rpc-2, Neovim may successfully process
     the response to rpc-1. If Rpc_2 sleeps for too little time before sending rpc-2,
     rpc-2 will be invoked before the dummy returns, and the plugin will determine that it
     is unsafe to respond to rpc-1. So, we need to inject a period of time between the
     dummy returning indicating it's safe to respond and actually sending the response
     during which sending the response becomes unsafe. *)
  Private.before_sending_response_hook_for_tests
  := Some (fun () -> Clock_ns.after (Time_ns.Span.of_int_ms 10));
  let output = ref "" in
  let test ?verbose spec =
    Expect_test_helpers_async.show_raise_async (fun () ->
      (* In rare cases this test will not raise, so we run it up to 100 times. It will
         likely fail within the first 2. *)
      Deferred.for_ 1 ~to_:100 ~do_:(fun _ ->
        match%map
          Monitor.try_with (fun () ->
            run_spec_and_check_result ~warn_if_neovim_exits_early:false ?verbose spec)
        with
        | Ok () -> ()
        | Error exn ->
          output := [%expect.output];
          raise exn))
  in
  let%bind () =
    test
      (Call_neovim
         { name = "Rpc_1"
         ; how = Request
         ; specs =
             [ Call_plugin
                 { name = "rpc-1"
                 ; how = Request
                 ; specs =
                     [ Call_neovim
                         { name = "Rpc_2"
                         ; how = Notification
                         ; specs =
                             [ Sleep_neovim { milliseconds = 1; blocking = false }
                             ; Call_plugin { name = "rpc-2"; how = Request; specs = [] }
                             ]
                         }
                     ]
                 }
             ]
         })
  in
  printf "\n%s\n" !output;
  [%expect
    {|
    (raised (
      ("Consumer left without responding" (
        request (
          Array (
            (Int    0)
            (Int    11)
            (String nvim_call_function)
            (Array ((String Rpc_1) (Array ((Array ())))))))))
      (("Called from" lib/vcaml/test/semantics/test_blocking_nvim.ml:LINE:COL))))

    -----  NVIM_LOG_FILE  -----
    ERR TIMESTAMP socket     chan_close_with_error:LINE: RPC: ch 1 returned a response with an unknown request id. Ensure the client is properly synchronized
    ---------------------------
    |}];
  Dynamic.set_root Backtrace.elide false;
  Private.before_sending_response_hook_for_tests := None;
  return ()
;;

(* Demonstrate a bug that can arise due to Neovim's [sleep] defeating VCaml's flushing.
   The plugin requests Rpc_1, then Rpc_2. Neovim receives Rpc_1, which tells it to
   rpcrequest rpc-1 from the plugin. Then it receives Rpc_2, which it interprets as a
   nested request. Rpc_2 puts itself to sleep, which allows Neovim to continue processing
   received messages. When the plugin receives rpc-1, it flushes Neovim's event loop with
   a dummy request, and that request returns successfully because sleep removed Rpc_2 from
   the loop. Then rpc-1 begins, and while it's running, Rpc_2 wakes from sleep and
   rpcrequests rpc-2, interleaving rpc-1. *)
let%expect_test "Demonstrate interleaving bug induced by sleep" =
  let test ?verbose spec =
    Expect_test_helpers_async.show_raise_async (fun () ->
      (* In rare cases this test will not raise, so we run it up to 100 times. It will
         likely fail within the first 2. *)
      Deferred.for_ 1 ~to_:100 ~do_:(fun _ -> run_spec_and_check_result ?verbose spec))
  in
  let%bind () =
    test
      (Parallel
         [ Call_neovim
             { name = "Rpc_1"
             ; how = Notification
             ; specs =
                 [ Call_plugin
                     { name = "rpc-1"
                     ; how = Request
                     ; specs = [ Log; Sleep_plugin (Time_ns.Span.of_int_ms 10); Log ]
                     }
                 ]
             }
         ; Call_neovim
             { name = "Rpc_2"
             ; how = Request
             ; specs =
                 [ Sleep_neovim { milliseconds = 1; blocking = false }
                 ; Call_plugin { name = "rpc-2"; how = Request; specs = [ Log ] }
                 ]
             }
         ])
  in
  [%expect
    {|
    (raised (
      "Invalid event ordering" (
        (1)
        (2)
        (1))))
    |}];
  return ()
;;

(* 10K Quickcheck trials takes approximately 5 minutes to run. *)
let%expect_test "Quickcheck tests of running specifications" =
  let run () =
    let call ~await spec = Action.Spec.Call { await; spec } in
    let latest_failure = ref None in
    Monitor.protect
      (fun () ->
        Async_quickcheck.async_test
          ~trials:10000
          ~shrinker:[%quickcheck.shrinker: Action.Spec.t]
          ~sexp_of:[%sexp_of: Action.Spec.t]
          ~examples:
            [ call ~await:false [ call ~await:false [] ]
            ; call ~await:false [ call ~await:true [] ]
            ; call ~await:true [ call ~await:false [] ]
            ; call ~await:true [ call ~await:true [] ]
            ; Parallel [ call ~await:false []; call ~await:false [] ]
            ; Parallel [ call ~await:false []; call ~await:true [] ]
            ; Parallel [ call ~await:true []; call ~await:false [] ]
            ; Parallel [ call ~await:true []; call ~await:true [] ]
            ]
          [%quickcheck.generator: Action.Spec.t]
          ~f:(fun spec ->
            match%map
              Monitor.try_with (fun () ->
                run_spec_and_check_result ~verbose:true (Action.Spec.to_new spec))
            with
            | Ok () -> ignore ([%expect.output] : string)
            | Error exn ->
              (* When an exception happens in [async_test], Quickcheck tries to shrink the
                 failing case, then ultimately fails on the most shrunken example. We only
                 want to output the verbose logs for that failure. *)
              latest_failure := Some [%expect.output];
              raise exn))
      ~finally:(fun () ->
        Option.iter !latest_failure ~f:prerr_endline;
        return ())
  in
  let%bind () =
    match enable_slow_tests with
    | true -> run ()
    | false -> return ()
  in
  [%expect {| |}];
  return ()
;;
