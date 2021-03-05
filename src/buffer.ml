open Core
open Async
include Types.Buffer

(* We can't use [Vcaml.Client.eval] because of dependency cycles

   This function is pretty fragile, but there isn't really a better way to know what
   buffer we're currently in, so we pretty much have to do this to get good filtering
   from [`Current] event listen requests.
*)
let current_buffer =
  let query : Msgpack.t Types.Api_result.t =
    { name = "nvim_eval"
    ; params = Array [ String "bufnr(\"%\")" ]
    ; witness = Types.Phantom.Object
    }
  in
  let open Api_call.Let_syntax in
  let%map result = Api_call.of_api_result query in
  Or_error.bind ~f:of_msgpack result
;;

module Event = struct
  type nonrec t =
    | Lines of
        { buffer : t
        ; changedtick : int option
        ; firstline : int
        ; lastline : int
        ; linedata : string list
        ; more : bool
        }
    | Changed_tick of
        { buffer : t
        ; changedtick : int
        }
    | Detach of t
  [@@deriving sexp_of]

  let parse { Msgpack_rpc.method_name; params } =
    let open Option.Let_syntax in
    match method_name with
    | "nvim_buf_lines_event" ->
      (match params with
       | [ (Extension _ as buffer)
         ; changedtick
         ; Integer firstline
         ; Integer lastline
         ; Array changes
         ; Boolean more
         ] ->
         let%bind linedata =
           Option.try_with (fun () ->
             List.map changes ~f:(function
               | String s -> s
               | _ -> failwith "short-circuit"))
         in
         let%bind buffer = of_msgpack buffer |> Or_error.ok in
         let%bind changedtick =
           match changedtick with
           | Nil -> Some None
           | Integer i -> Some (Some i)
           | _ -> None
         in
         Some (Lines { buffer; changedtick; firstline; lastline; linedata; more })
       | _ -> None)
    | "nvim_buf_changedtick_event" ->
      (match params with
       | [ (Extension _ as buffer); Integer changedtick ] ->
         let%bind buffer = of_msgpack buffer |> Or_error.ok in
         Some (Changed_tick { buffer; changedtick })
       | [ (Extension _ as buffer); Nil ] ->
         let%bind buffer = of_msgpack buffer |> Or_error.ok in
         Some (Changed_tick { buffer; changedtick = 0 })
       | _ -> None)
    | "nvim_buf_detach_event" ->
      (match params with
       | [ (Extension _ as buffer) ] ->
         let%bind buffer = of_msgpack buffer |> Or_error.ok in
         Some (Detach buffer)
       | _ -> None)
    | _ -> None
  ;;

  let for_buffer t = function
    | Lines { buffer; _ } | Changed_tick { buffer; _ } | Detach buffer -> t = buffer
  ;;
end

let get_name ~buffer =
  Nvim_internal.Wrappers.nvim_buf_get_name ~buffer |> Api_call.of_api_result
;;

let get_lines ~buffer ~start ~end_ ~strict_indexing =
  let open Api_call.Let_syntax in
  let%map result =
    Nvim_internal.Wrappers.nvim_buf_get_lines ~buffer ~start ~end_ ~strict_indexing
    |> Api_call.of_api_result
  in
  let open Or_error.Let_syntax in
  let%bind result = result in
  Or_error.try_with (fun () ->
    List.map result ~f:(function
      | String s -> s
      | _ -> failwith "malformed result"))
;;

let set_lines ~buffer ~start ~end_ ~strict_indexing ~replacement =
  let replacement = List.map ~f:(fun v -> Msgpack.String v) replacement in
  Nvim_internal.Wrappers.nvim_buf_set_lines
    ~buffer
    ~start
    ~end_
    ~strict_indexing
    ~replacement
  |> Api_call.of_api_result
;;

let buf_events client =
  let events = (Types.Client.Private.of_public client).events in
  let r, w = Pipe.create () in
  let s =
    Bus.subscribe_exn events [%here] ~f:(fun e ->
      match Event.parse e with
      | Some evt -> Pipe.write_without_pushback_if_open w evt
      | None -> ())
  in
  upon
    (Deferred.any [ Pipe.closed r; Pipe.closed w ])
    (fun () ->
       Pipe.close_read r;
       Pipe.close w;
       Bus.unsubscribe events s);
  r
;;

let interleave_slashes name =
  name
  |> String.to_list
  |> List.map ~f:(fun c -> [ '\\'; c ])
  |> List.join
  |> String.of_char_list
;;

let badd_api_call name =
  (* For input to :badd, you can escape any keyboard character with a slash. For example,
     \h\i is interpreted as hi. Notably, things like \n, \t, and \r get sent to n, t, and
     r instead of their usual string meanings. As a result, it makes sense to escape all
     characters, since certain characters (e.g. space) require being escaped, and
     escaping characters behaves nicely even on characters which don't require it. *)
  let escaped_name = interleave_slashes name in
  Nvim_internal.Wrappers.nvim_command ~command:(sprintf "badd %s" escaped_name)
  |> Api_call.of_api_result
;;

let bufnr_api_call name =
  let escaped_name = String.escaped name in
  Nvim_internal.Wrappers.nvim_eval ~expr:(sprintf "bufnr(\"%s\")" escaped_name)
  |> Api_call.of_api_result
;;

let add_buffer_before_searching name =
  let open Api_call.Let_syntax in
  let%map badd_or_err = badd_api_call name
  and bufnr_or_err = bufnr_api_call name in
  Or_error.both badd_or_err bufnr_or_err
  |> Or_error.bind ~f:(fun ((), buffer) -> of_msgpack buffer)
;;

let search_for_buffer name =
  let open Api_call.Let_syntax in
  let%map result = bufnr_api_call name in
  result |> Or_error.bind ~f:of_msgpack
;;

(* If we call vim's bufnr command with a string x, it may resolve to a different buffer
   y, if x is a prefix of y and x is not a prefix of any other open buffers in vim. As a
   result, we need to perform a :badd prior to calling bufnr, with the caveat that :badd
   cannot be used for the default buffer (whose name is the empty string). *)
let find_by_name_or_create ~name =
  match name with
  | "" -> search_for_buffer name
  | _ -> add_buffer_before_searching name
;;

let attach ?(opts = []) client ~(buffer : [ `Current | `Numbered of t ]) ~send_buffer =
  match Types.Client.Private.of_public client with
  | { buffers_attached; attach_sequencer; _ } ->
    let buffer_query =
      match buffer with
      | `Current -> Unsafe.of_int 0
      | `Numbered b -> b
    in
    let attach =
      Api_call.of_api_result
        (Nvim_internal.Wrappers.nvim_buf_attach ~buffer:buffer_query ~send_buffer ~opts)
    in
    let curr_bufnr =
      match buffer with
      | `Current -> current_buffer
      | `Numbered b -> Api_call.return (Ok b)
    in
    let call = Api_call.both attach curr_bufnr in
    let open Deferred.Or_error.Let_syntax in
    (* We always run the actual attach because it's possible that the user has deleted the
       buffer (and so we want to fail with an error).
    *)
    let%bind success, bufnr = Api_call.run client call in
    let run_attach () =
      let open Deferred.Or_error.Let_syntax in
      if%bind Deferred.return success
      then (
        let%bind bufnr = Deferred.return bufnr in
        Hashtbl.change buffers_attached bufnr ~f:(function
          | Some x -> Some (x + 1)
          | None -> Some 1);
        let incoming = Pipe.filter (buf_events client) ~f:(Event.for_buffer bufnr) in
        let r =
          Pipe.create_reader ~close_on_exception:false (fun w ->
            Pipe.iter incoming ~f:(function
              | Event.Detach _ as evt ->
                let open Deferred.Let_syntax in
                (* Write without pushback here because we don't want the scheduler
                   interrupting us *)
                Pipe.write_without_pushback_if_open w evt;
                Pipe.close w;
                Hashtbl.remove buffers_attached bufnr;
                return ()
              | evt -> Pipe.write_if_open w evt))
        in
        upon (Pipe.closed r) (fun () ->
          upon
            (Api_call.run_join
               client
               (Nvim_internal.Wrappers.nvim_buf_detach ~buffer:bufnr
                |> Api_call.of_api_result))
            (function
              | Ok true -> Hashtbl.remove buffers_attached bufnr
              | _ -> Log.Global.error "failed to detach from buffer, ignoring"));
        return r)
      else Deferred.Or_error.error_string "unable to connect to buffer"
    in
    Throttle.enqueue attach_sequencer run_attach
;;

let set_option ~buffer ~name ~value =
  Nvim_internal.Wrappers.nvim_buf_set_option ~buffer ~name ~value
  |> Api_call.of_api_result
;;

module Untested = struct
  let line_count ~buffer =
    Api_call.of_api_result (Nvim_internal.Wrappers.nvim_buf_line_count ~buffer)
  ;;

  let get_var ~buffer ~name =
    Nvim_internal.Wrappers.nvim_buf_get_var ~buffer ~name |> Api_call.of_api_result
  ;;

  let get_changedtick ~buffer =
    Nvim_internal.Wrappers.nvim_buf_get_changedtick ~buffer |> Api_call.of_api_result
  ;;

  let get_commands ?(opts = []) ~buffer =
    let open Api_call.Let_syntax in
    let%map result =
      Nvim_internal.Wrappers.nvim_buf_get_commands ~buffer ~opts |> Api_call.of_api_result
    in
    let open Or_error.Let_syntax in
    let%bind result = result in
    let%bind commands_with_names =
      List.map result ~f:(fun (name, command) ->
        let open Or_error.Let_syntax in
        let%bind n = Extract.string name in
        let%bind c = Nvim_command.of_msgpack command in
        return (n, c))
      |> Or_error.combine_errors
    in
    String.Map.of_alist_or_error commands_with_names
  ;;

  let set_var ~buffer ~name ~value =
    Nvim_internal.Wrappers.nvim_buf_set_var ~buffer ~name ~value |> Api_call.of_api_result
  ;;

  let del_var ~buffer ~name =
    Nvim_internal.Wrappers.nvim_buf_del_var ~buffer ~name |> Api_call.of_api_result
  ;;

  let get_option ~buffer ~name =
    Nvim_internal.Wrappers.nvim_buf_get_option ~buffer ~name |> Api_call.of_api_result
  ;;

  let set_name ~buffer ~name =
    Nvim_internal.Wrappers.nvim_buf_set_name ~buffer ~name |> Api_call.of_api_result
  ;;

  let is_valid ~buffer =
    Nvim_internal.Wrappers.nvim_buf_is_valid ~buffer |> Api_call.of_api_result
  ;;

  let get_mark ~buffer ~sym =
    let open Api_call.Let_syntax in
    let%map pos =
      Nvim_internal.Wrappers.nvim_buf_get_mark ~buffer ~name:(Char.to_string sym)
      |> Api_call.of_api_result
    in
    let open Or_error.Let_syntax in
    match%bind pos with
    | [ Integer row; Integer col ] -> Ok { Mark.sym; row; col }
    | _ -> Or_error.error_string "malformed result from [nvim_buf_get_mark]"
  ;;

  let add_highlight ~buffer ~ns_id ~hl_group ~line ~col_start ~col_end =
    Nvim_internal.Wrappers.nvim_buf_add_highlight
      ~buffer
      ~ns_id
      ~hl_group
      ~line
      ~col_start
      ~col_end
    |> Api_call.of_api_result
  ;;

  let clear_highlight ~buffer ~ns_id ~line_start ~line_end =
    Nvim_internal.Wrappers.nvim_buf_clear_highlight ~buffer ~ns_id ~line_start ~line_end
    |> Api_call.of_api_result
  ;;

  let set_scratch ~buffer =
    let open Api_call.Let_syntax in
    let%map _nofile = set_option ~buffer ~name:"buftype" ~value:(String "nofile")
    and _bufhidden = set_option ~buffer ~name:"bufhidden" ~value:(String "hide")
    and _noswapfile = set_option ~buffer ~name:"swapfile" ~value:(Boolean false)
    and _unlisted = set_option ~buffer ~name:"buflisted" ~value:(Boolean false) in
    ()
  ;;
end
