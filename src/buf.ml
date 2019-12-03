open Core
open Async

type t = Types.Buf.t

let sexp_of_t t =
  Sexp.(
    List
      [ Atom "Buffer"; Nvim_internal.Types.Buffer.to_msgpack t |> [%sexp_of: Msgpack.t] ])
;;

module Table = Types.Buf.Table

type mark =
  { row : int
  ; col : int
  }

type which_buffer =
  [ `Current
  | `Numbered of t
  ]

open Types

let to_msgpack = Types.Buf.to_msgpack
let of_msgpack = Types.Buf.of_msgpack

(* We can't use [Vcaml.Client.eval] because of dependency cycles

   This function is pretty fragile, but there isn't really a better way to know what
   buffer we're currently in, so we pretty much have to do this to get good filtering
   from [`Current] event listen requests.
*)
let current_buffer =
  let query : Msgpack.t Nvim_internal.Types.api_result =
    { name = "nvim_eval"
    ; params = Array [ String "bufnr(\"%\")" ]
    ; witness = Nvim_internal.Types.Phantom.Object
    }
  in
  let open Api_call.Let_syntax in
  let%map result = Api_call.of_api_result query in
  Or_error.bind ~f:Nvim_internal.Types.Buffer.of_msgpack result
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
       | [ (Extension _ as buf)
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
         let%bind buffer = of_msgpack buf |> Or_error.ok in
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
       | [ (Extension _ as buf); Integer changedtick ] ->
         let%bind buffer = of_msgpack buf |> Or_error.ok in
         Some (Changed_tick { buffer; changedtick })
       | [ (Extension _ as buf); Nil ] ->
         let%bind buffer = of_msgpack buf |> Or_error.ok in
         Some (Changed_tick { buffer; changedtick = 0 })
       | _ -> None)
    | "nvim_buf_detach_event" ->
      (match params with
       | [ (Extension _ as buf) ] ->
         let%bind buffer = of_msgpack buf |> Or_error.ok in
         Some (Detach buffer)
       | _ -> None)
    | _ -> None
  ;;

  let for_buffer buf =
    let equal = Nvim_internal.Types.Buffer.equal in
    function
    | Lines { buffer; _ } | Changed_tick { buffer; _ } | Detach buffer ->
      equal buffer buf
  ;;
end

open Msgpack

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
  let replacement = List.map ~f:(fun v -> String v) replacement in
  Nvim_internal.Wrappers.nvim_buf_set_lines
    ~buffer
    ~start
    ~end_
    ~strict_indexing
    ~replacement
  |> Api_call.of_api_result
;;

let buf_events { events; _ } =
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

let attach
      ?(opts = [])
      ({ attach_sequencer; buffers_attached; _ } as cli)
      ~(buffer : which_buffer)
      ~send_buffer
  =
  let buffer_query =
    match buffer with
    | `Current -> Nvim_internal.Types.Buffer.of_msgpack (Integer 0) |> Or_error.ok_exn
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
  let%bind success, bufnr = Api_call.run cli call in
  let run_attach () =
    let open Deferred.Or_error.Let_syntax in
    if%bind Deferred.return success
    then (
      let%bind bufnr = Deferred.return bufnr in
      Hashtbl.change buffers_attached bufnr ~f:(function
        | Some x -> Some (x + 1)
        | None -> Some 1);
      let incoming = Pipe.filter (buf_events cli) ~f:(Event.for_buffer bufnr) in
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
             cli
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

  let get_keymap ~buffer ~mode =
    let open Api_call.Let_syntax in
    let%map result =
      Nvim_internal.Wrappers.nvim_buf_get_keymap ~buffer ~mode |> Api_call.of_api_result
    in
    let open Or_error.Let_syntax in
    let%bind result = result in
    List.map ~f:(Keymap.Untested.of_msgpack ~to_buf:of_msgpack) result
    |> Or_error.combine_errors
  ;;

  let get_commands ?(opts = []) ~buffer =
    let open Api_call.Let_syntax in
    let%map result =
      Nvim_internal.Wrappers.nvim_buf_get_commands ~buffer ~opts
      |> Api_call.of_api_result
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
    Nvim_internal.Wrappers.nvim_buf_set_var ~buffer ~name ~value
    |> Api_call.of_api_result
  ;;

  let del_var ~buffer ~name =
    Nvim_internal.Wrappers.nvim_buf_del_var ~buffer ~name |> Api_call.of_api_result
  ;;

  let get_option ~buffer ~name =
    Nvim_internal.Wrappers.nvim_buf_get_option ~buffer ~name |> Api_call.of_api_result
  ;;

  let set_option ~buffer ~name ~value =
    Nvim_internal.Wrappers.nvim_buf_set_option ~buffer ~name ~value
    |> Api_call.of_api_result
  ;;

  let set_name ~buffer ~name =
    Nvim_internal.Wrappers.nvim_buf_set_name ~buffer ~name |> Api_call.of_api_result
  ;;

  let is_valid ~buffer =
    Nvim_internal.Wrappers.nvim_buf_is_valid ~buffer |> Api_call.of_api_result
  ;;

  let get_mark ~buffer ~name =
    let open Api_call.Let_syntax in
    let%map pos =
      Nvim_internal.Wrappers.nvim_buf_get_mark ~buffer ~name |> Api_call.of_api_result
    in
    let open Or_error.Let_syntax in
    match%bind pos with
    | [ Integer row; Integer col ] -> Ok { row; col }
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

  let find_by_name_or_create ~name =
    let open Api_call.Let_syntax in
    let%map result =
      Nvim_internal.Wrappers.nvim_eval ~expr:(sprintf "bufnr(\"%s\", 1)" name)
      |> Api_call.of_api_result
    in
    Or_error.bind ~f:Nvim_internal.Types.Buffer.of_msgpack result
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
