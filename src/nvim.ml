module Unshadow = struct
  module Buffer = Buffer
end

open Core
open Async
open Import
open Unshadow
module Type = Type

module Mouse = struct
  module Button = struct
    type t =
      | Left
      | Right
      | Middle
    [@@deriving sexp_of]

    let to_string = function
      | Left -> "left"
      | Right -> "right"
      | Middle -> "middle"
    ;;
  end

  module Action = struct
    type t =
      | Press of Button.t
      | Drag of Button.t
      | Release of Button.t
      | Move
      | Wheel_up
      | Wheel_down
      | Wheel_left
      | Wheel_right
    [@@deriving sexp_of]
  end
end

module Key_modifier = struct
  module T = struct
    type t =
      | Shift
      | Ctrl
      | Alt
      | Super
    [@@deriving compare, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)
end

let channels ~(here : [%call_pos]) client =
  let client = Type_equal.conv Client.Private.eq client in
  Client.Private.nvim_list_chans ~here client
;;

let exec_viml ~(here : [%call_pos]) client code =
  Nvim_internal.nvim_exec2 ~src:code ~opts:String.Map.empty
  |> map_witness ~f:(fun result ->
    match Map.find result "output" with
    | None -> Ok ()
    | Some output -> Or_error.error_s [%message "Unexpected output" (output : Msgpack.t)])
  |> run ~here client
;;

let exec_viml_and_capture_output ~(here : [%call_pos]) client code =
  Nvim_internal.nvim_exec2
    ~src:code
    ~opts:(String.Map.singleton "output" (Msgpack.Bool true))
  |> map_witness ~f:(fun result ->
    match Map.find result "output" with
    | Some (String output) -> Ok output
    | Some output ->
      Or_error.error_s [%message "Unexpected non-string output" (output : Msgpack.t)]
    | None ->
      Or_error.error_s
        [%message
          "Unexpectedly missing output in result" (result : Msgpack.t String.Map.t)])
  |> run ~here client
;;

let exec_lua ~(here : [%call_pos]) client code =
  Nvim_internal.nvim_exec_lua ~code ~args:[]
  |> map_witness ~f:(Type.of_msgpack Nil)
  |> run ~here client
;;

let list_bufs ~(here : [%call_pos]) client =
  Nvim_internal.nvim_list_bufs |> run ~here client
;;

let get_channel_info ~(here : [%call_pos]) client chan =
  Nvim_internal.nvim_get_chan_info ~chan
  |> map_witness ~f:Channel_info.of_msgpack_map
  |> run ~here client
;;

let eval_viml_expression ~(here : [%call_pos]) client expr ~result_type =
  Nvim_internal.nvim_eval ~expr
  |> map_witness ~f:(Type.of_msgpack result_type)
  |> run ~here client
;;

module Func = struct
  type 'fn t =
    | Nullary : 'output Type.t -> 'output Deferred.Or_error.t t
    | Cons : 'a Type.t * 'b t -> ('a -> 'b) t

  let return t = Nullary t
  let ( @-> ) typ t = Cons (typ, t)

  let rec apply_fn
    : type fn. fn t -> (Msgpack.t list -> Msgpack.t Deferred.Or_error.t) -> fn
    =
    fun t f ->
    (* Due to the fact that OCaml does not (easily) support higher-ranked polymorphism, we
       need to construct the function [to_msgpack] *after* we unpack this GADT, so it can
       have the type [i -> Msgpack.t] (which is fixed by [t] in this function). Otherwise,
       it needs the type [forall 'a . 'a witness -> 'a -> Msgpack.t], which is not that
       easily expressible. *)
    match t with
    | Nullary expected_type ->
      let%map result = f [] in
      let open Or_error.Let_syntax in
      let%bind result in
      Type.of_msgpack expected_type result |> Or_error.tag ~tag:"Wrong return type"
    | Cons (typ, rest) ->
      fun arg -> apply_fn rest (fun args -> f (Type.to_msgpack typ arg :: args))
  ;;
end

(* Changes here should probably have analogous changes in [Notifier.notify]. *)
let call_function ~(here : [%call_pos]) client ~name ~type_ =
  Func.apply_fn type_ (fun args ->
    (match name with
     | `Viml name -> Nvim_internal.nvim_call_function ~fn:name ~args
     | `Lua name ->
       Nvim_internal.nvim_exec_lua
       (* We surround [name] with parentheses to support anonymous functions. We assign it
          to [result] before returning it to ensure [name] appears in the stack trace if
          it raises an error. *)
         ~code:[%string {| local result = (%{name})(...); return result |}]
         ~args)
    |> run ~here client)
;;

let get_current_buf ~(here : [%call_pos]) client =
  Nvim_internal.nvim_get_current_buf |> run ~here client
;;

let set_current_buf ~(here : [%call_pos]) client buffer =
  Nvim_internal.nvim_set_current_buf ~buffer |> run ~here client
;;

let list_tabs ~(here : [%call_pos]) client =
  Nvim_internal.nvim_list_tabpages |> run ~here client
;;

let get_current_tab ~(here : [%call_pos]) client =
  Nvim_internal.nvim_get_current_tabpage |> run ~here client
;;

let set_current_tab ~(here : [%call_pos]) client tabpage =
  Nvim_internal.nvim_set_current_tabpage ~tabpage |> run ~here client
;;

let set_client_info ~(here : [%call_pos]) client ?version ?attributes ?client_type () =
  let client = Type_equal.conv Client.Private.eq client in
  Client.Private.nvim_set_client_info ~here client ?version ?attributes ?client_type ()
;;

let get_current_win ~(here : [%call_pos]) client =
  Nvim_internal.nvim_get_current_win |> run ~here client
;;

let set_current_win ~(here : [%call_pos]) client window =
  Nvim_internal.nvim_set_current_win ~window |> run ~here client
;;

let list_wins ~(here : [%call_pos]) client =
  Nvim_internal.nvim_list_wins |> run ~here client
;;

type keys_with_replaced_keycodes = string

let replace_termcodes ~(here : [%call_pos]) client str ~replace_keycodes =
  (* [from_part] is a legacy Vim parameter that should be [true]. Always replace <lt> when
     replacing keycodes (almost surely behavior we want, and simplifies the API). *)
  Nvim_internal.nvim_replace_termcodes
    ~str
    ~from_part:true
    ~do_lt:replace_keycodes
    ~special:replace_keycodes
  |> run ~here client
;;

let replace_termcodes_only = replace_termcodes ~replace_keycodes:false
let replace_termcodes_and_keycodes = replace_termcodes ~replace_keycodes:true

let feedkeys ~(here : [%call_pos]) client keys ~mode =
  (* The docs advise setting [escape_ks = true] (escape the K_SPECIAL byte) when passing
     an unescaped string. The K_SPECIAL byte is 0x80, and appears in multi-byte key codes.
     seandewar (Neovim dev) did not recall uses of K_SPECIAL outside of key codes, but was
     not confident. Regardless, we want [escape_ks = false] for escaped key codes because
     [replace_termcodes_and_keycodes] will have already escaped K_SPECIAL. In general,
     users should not need to think about K_SPECIAL, and we hide the details from them -
     the only important thing users should care about is that key codes are not escaped in
     [`Raw string] - they need to call [replace_termcodes_and_keycodes] and then pass the
     result to [`Keycodes]. *)
  let keys, escape_ks =
    match keys with
    | `Raw keys -> keys, true
    | `Keycodes keys -> keys, false
  in
  Nvim_internal.nvim_feedkeys ~keys ~mode ~escape_ks |> run ~here client
;;

let get_color_by_name ~(here : [%call_pos]) client name =
  Nvim_internal.nvim_get_color_by_name ~name
  |> map_witness ~f:(function
    | -1 -> Or_error.error_s [%message "Unrecognized color" (name : string)]
    | color -> Color.True_color.of_24bit_int color)
  |> run ~here client
;;

let get_color_map ~(here : [%call_pos]) client =
  Nvim_internal.nvim_get_color_map
  |> map_witness ~f:(fun color_map ->
    color_map
    |> Map.map ~f:(fun bits ->
      let%bind.Or_error bits = Type.of_msgpack Int bits in
      Color.True_color.of_24bit_int bits)
    |> Map.combine_errors)
  |> run ~here client
;;

let parse_highlight
  (type a)
  (colors : Msgpack.t String.Map.t)
  ~(color_depth : a Color.Depth.t)
  : a Color.Highlight.t Or_error.t
  =
  let open Color in
  let color_of_key key =
    let color_of_int : int -> a Or_error.t =
      match color_depth with
      | True_color -> True_color.of_24bit_int
      | Color256 -> Color256.of_8bit_int
    in
    match Map.find colors key with
    | None -> Ok None
    | Some (Int int) -> color_of_int int |> Or_error.map ~f:Option.return
    | Some value ->
      Or_error.error_s
        [%message
          "Invalid msgpack in highlight response" (key : string) (value : Msgpack.t)]
  in
  let fg_key, bg_key =
    match color_depth with
    | True_color -> "fg", "bg"
    | Color256 -> "ctermfg", "ctermbg"
  in
  let open Or_error.Let_syntax in
  let%bind fg = color_of_key fg_key in
  let%bind bg = color_of_key bg_key in
  return { Highlight.fg; bg }
;;

module Highlight_id = Int

let get_hl_id_by_name ~(here : [%call_pos]) client name =
  Nvim_internal.nvim_get_hl_id_by_name ~name |> run ~here client
;;

let get_hl_by_name
  (type a)
  ~(here : [%call_pos])
  client
  ?(ns_id = Color.Namespace.global)
  name
  ~(color_depth : a Color.Depth.t)
  =
  Nvim_internal.nvim_get_hl
    ~ns_id:(Color.Namespace.to_int ns_id)
    ~opts:(String.Map.singleton "name" (Msgpack.String name))
  |> map_witness ~f:(parse_highlight ~color_depth)
  |> run ~here client
;;

let get_hl_by_id
  (type a)
  ~(here : [%call_pos])
  client
  ?(ns_id = Color.Namespace.global)
  hl_id
  ~(color_depth : a Color.Depth.t)
  =
  Nvim_internal.nvim_get_hl
    ~ns_id:(Color.Namespace.to_int ns_id)
    ~opts:(String.Map.singleton "id" (Msgpack.Int hl_id))
  |> map_witness ~f:(parse_highlight ~color_depth)
  |> run ~here client
;;

let get_var ~(here : [%call_pos]) client name ~type_ =
  Nvim_internal.nvim_get_var ~name
  |> map_witness ~f:(Type.of_msgpack type_)
  |> run ~here client
;;

let set_var ~(here : [%call_pos]) client name ~type_ ~value =
  let value = Type.to_msgpack type_ value in
  Nvim_internal.nvim_set_var ~name ~value |> run ~here client
;;

let delete_var ~(here : [%call_pos]) client name =
  Nvim_internal.nvim_del_var ~name |> run ~here client
;;

let get_vvar ~(here : [%call_pos]) client name ~type_ =
  Nvim_internal.nvim_get_vvar ~name
  |> map_witness ~f:(Type.of_msgpack type_)
  |> run ~here client
;;

let set_vvar ~(here : [%call_pos]) client name ~type_ ~value =
  let value = Type.to_msgpack type_ value in
  Nvim_internal.nvim_set_vvar ~name ~value |> run ~here client
;;

let list_runtime_paths ~(here : [%call_pos]) client =
  Nvim_internal.nvim_list_runtime_paths |> run ~here client
;;

let out_write ~(here : [%call_pos]) client str =
  Nvim_internal.nvim_out_write ~str |> run ~here client
;;

(* For some reason this isn't a supported API function like [err_writeln]. *)
let out_writeln ~(here : [%call_pos]) client str =
  Nvim_internal.nvim_out_write ~str:(str ^ "\n") |> run ~here client
;;

let err_write ~(here : [%call_pos]) client str =
  Nvim_internal.nvim_err_write ~str |> run ~here client
;;

let err_writeln ~(here : [%call_pos]) client str =
  Nvim_internal.nvim_err_writeln ~str |> run ~here client
;;

let echo ~(here : [%call_pos]) client message ~add_to_history =
  (* [opts] is not used by this version of Neovim, but may be used in the future. If we
     expose it, we should do so in a typeful way rather than asking the user to build
     [Msgpack.t] values. *)
  Nvim_internal.nvim_echo
    ~chunks:(Highlighted_text.to_msgpack message)
    ~history:add_to_history
    ~opts:String.Map.empty
  |> run ~here client
;;

module Tabline = Statusline [@@alert "-vcaml_do_not_export"]

module Fast = struct
  let get_mode ~(here : [%call_pos]) client =
    Nvim_internal.nvim_get_mode
    |> map_witness ~f:(fun mode_info ->
      let open Or_error.Let_syntax in
      let%bind mode =
        let%bind mode =
          find_or_error_and_convert mode_info "mode" (Type.of_msgpack String)
        in
        Mode.of_mode_symbol mode
      in
      let%bind blocking =
        find_or_error_and_convert mode_info "blocking" (Type.of_msgpack Bool)
      in
      Ok { Mode.With_blocking_info.mode; blocking })
    |> run ~here client
  ;;

  let input ~(here : [%call_pos]) client keys =
    Nvim_internal.nvim_input ~keys |> run ~here client
  ;;

  let input_mouse
    ~(here : [%call_pos])
    client
    ?(modifiers = Key_modifier.Set.empty)
    ?(grid = 0)
    action
    ~row
    ~col
    =
    let action, button =
      match (action : Mouse.Action.t) with
      | Press button -> "press", Mouse.Button.to_string button
      | Drag button -> "drag", Mouse.Button.to_string button
      | Release button -> "release", Mouse.Button.to_string button
      | Move -> "move", "move"
      | Wheel_up -> "up", "wheel"
      | Wheel_down -> "down", "wheel"
      | Wheel_left -> "left", "wheel"
      | Wheel_right -> "right", "wheel"
    in
    let modifier =
      modifiers
      |> Set.to_list
      |> List.map ~f:(fun modifier ->
        match (modifier : Key_modifier.t) with
        | Shift -> "S"
        | Ctrl -> "C"
        | Alt -> "A"
        | Super -> "D")
      |> String.concat
    in
    Nvim_internal.nvim_input_mouse ~button ~action ~modifier ~grid ~row ~col
    |> run ~here client
  ;;

  let eval_tabline
    ~(here : [%call_pos])
    client
    ?max_width
    ?fill_char
    ~include_highlights
    statusline
    =
    Tabline.eval ~here client ?max_width ?fill_char ~include_highlights Tabline statusline
  ;;

  let find_runtime_file_matching ~(here : [%call_pos]) client ~pattern =
    Nvim_internal.nvim_get_runtime_file ~name:pattern ~all:false
    |> map_witness ~f:(function
      | [] -> Ok None
      | [ file ] -> Ok (Some file)
      | files ->
        Or_error.error_s
          [%message
            "[nvim_get_runtime_file] returned multiple files when [all = false] was \
             passed"
              (files : string list)])
    |> run ~here client
  ;;

  let all_runtime_files_matching ~(here : [%call_pos]) client ~pattern =
    Nvim_internal.nvim_get_runtime_file ~name:pattern ~all:true |> run ~here client
  ;;
end

let paste ~(here : [%call_pos]) client data =
  (* We set [crlf:false] here because VCaml already is UNIX-specific. If we change it in
     the future to support Windows we can expose this option, but for now it just clutters
     the API unnecessarily. *)
  let data = String.concat data ~sep:"\n" in
  Nvim_internal.nvim_paste ~data ~crlf:false ~phase:(-1)
  |> run ~here client
  |> Deferred.Or_error.ignore_m
;;

let paste_stream ~(here : [%call_pos]) client =
  let open Async in
  let flushed = Ivar.create () in
  let writer =
    Pipe.create_writer (fun reader ->
      let phase = ref 1 in
      let forced_stop = ref false in
      let force_stop () =
        Pipe.close_read reader;
        forced_stop := true;
        return ()
      in
      let%bind () =
        Pipe.iter reader ~f:(fun data ->
          match%bind
            Nvim_internal.nvim_paste ~data ~crlf:false ~phase:!phase |> run ~here client
          with
          | Ok true ->
            phase := 2;
            return ()
          | Ok false ->
            (* Documentation says we must stop pasting when we receive [false]. *)
            force_stop ()
          | Error _ as error ->
            Ivar.fill_exn flushed error;
            force_stop ())
      in
      let%bind () =
        match !forced_stop with
        | true -> return ()
        | false ->
          Nvim_internal.nvim_paste ~data:"" ~crlf:false ~phase:3
          |> run ~here client
          >>| Or_error.ignore_m
          >>| (function
           | Ok () -> ()
           | Error _ as error -> Ivar.fill_exn flushed error)
      in
      Ivar.fill_if_empty flushed (Ok ());
      return ())
  in
  writer, Ivar.read flushed
;;

module Text_mode = struct
  type t =
    | Charwise
    | Linewise
    | Blockwise of { width : int }
end

let put ~(here : [%call_pos]) client ?text_mode lines ~where ~place_cursor =
  let type_ =
    match (text_mode : Text_mode.t option) with
    | None -> "" (* Inferred from contents. *)
    | Some Charwise -> "c"
    | Some Linewise -> "l"
    | Some (Blockwise { width }) -> [%string "b%{width#Int}"]
  in
  let after =
    match where with
    | `Before_cursor -> false
    | `After_cursor -> true
  in
  let follow =
    match place_cursor with
    | `At_start_of_text -> false
    | `At_end_of_text -> true
  in
  Nvim_internal.nvim_put ~lines ~type_ ~after ~follow |> run ~here client
;;

(* The <CR> is buffered and then used to reply to the prompt. The side-effect of this
   dance is that the prompt is echoed to the screen. The <C-u> is used to clear pending
   input that the user might have actually typed (e.g., from mashing the keyboard while
   waiting). *)
let echo_in_rpcrequest ~(here : [%call_pos]) client message =
  let message = String.substr_replace_all message ~pattern:"'" ~with_:"'.\"'\".'" in
  [ T (Nvim_internal.nvim_input ~keys:"<C-u><CR>")
  ; T (Nvim_internal.nvim_eval ~expr:(sprintf "input('%s')" message))
  ]
  |> Atomic.run ~here client
  |> Deferred.Result.map_error ~f:Atomic.Error.to_error
  |> Deferred.Or_error.ignore_m
;;

module Log_level = struct
  (* See `:h log_levels` *)
  type t =
    | Trace
    | Debug
    | Info
    | Warn
    | Error
  [@@deriving compare, enumerate, sexp_of]

  let to_int = function
    | Trace -> 0
    | Debug -> 1
    | Info -> 2
    | Warn -> 3
    | Error -> 4
  ;;
end

let notify ~(here : [%call_pos]) client log_level message =
  (* [opts] is not used by this version of Neovim, but may be used in the future. If we
     expose it, we should do so in a typeful way rather than asking the user to build
     [Msgpack.t] values. *)
  Nvim_internal.nvim_notify
    ~msg:message
    ~log_level:(Log_level.to_int log_level)
    ~opts:String.Map.empty
  |> map_witness ~f:(Type.of_msgpack Nil)
  |> run ~here client
;;

let send_to_channel ~(here : [%call_pos]) client ~channel data =
  Nvim_internal.nvim_chan_send ~chan:channel ~data |> run ~here client
;;

let get_current_line ~(here : [%call_pos]) client =
  Nvim_internal.nvim_get_current_line
  |> map_witness ~f:(fun line -> Ok (String.Utf8.of_string_unchecked line))
  |> run ~here client
;;

let set_current_line ~(here : [%call_pos]) client line =
  Nvim_internal.nvim_set_current_line ~line |> run ~here client
;;

let delete_current_line ~(here : [%call_pos]) client =
  Nvim_internal.nvim_del_current_line |> run ~here client
;;

module Context_type = struct
  module T = struct
    type t =
      | Jumplist
      | Registers
      | Buffer_list
      | Global_variables
      | Script_local_functions
      | Global_and_script_local_functions
    [@@deriving compare, enumerate, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)

  let to_string = function
    | Jumplist -> "jumps"
    | Registers -> "regs"
    | Buffer_list -> "bufs"
    | Global_variables -> "gvars"
    | Script_local_functions -> "sfuncs"
    | Global_and_script_local_functions -> "funcs"
  ;;

  let of_string = function
    | "jumps" -> Jumplist
    | "regs" -> Registers
    | "bufs" -> Buffer_list
    | "gvars" -> Global_variables
    | "sfuncs" -> Script_local_functions
    | "funcs" -> Global_and_script_local_functions
    | context_type -> raise_s [%message "Unknown context type" context_type] [@nontail]
  ;;
end

let get_context ~(here : [%call_pos]) client context_types =
  let opts =
    let context_types =
      context_types
      |> Set.to_list
      |> List.map ~f:(fun context_type ->
        Msgpack.String (Context_type.to_string context_type))
    in
    [ "types", Msgpack.Array context_types ] |> String.Map.of_alist_exn
  in
  Nvim_internal.nvim_get_context ~opts
  |> map_witness ~f:(fun map ->
    Or_error.try_with (fun () ->
      Map.map_keys_exn (module Context_type) map ~f:Context_type.of_string))
  |> run ~here client
;;

let load_context ~(here : [%call_pos]) client context =
  let context = Map.map_keys_exn (module String) context ~f:Context_type.to_string in
  Nvim_internal.nvim_load_context ~dict:context
  |> map_witness ~f:(Type.of_msgpack Nil)
  |> run ~here client
;;

let get_mark ~(here : [%call_pos]) client sym =
  (* [opts] is not used by this version of Neovim, but may be used in the future. If we
     expose it, we should do so in a typeful way rather than asking the user to build
     [Msgpack.t] values. *)
  Nvim_internal.nvim_get_mark ~name:(Char.to_string sym) ~opts:String.Map.empty
  |> map_witness ~f:(function
    | [ Msgpack.Int 0; Int 0; Int 0; String "" ] -> Ok None
    | [ Msgpack.Int row; Int col; buffer; String _buffer_name ] ->
      (match Buffer.of_msgpack buffer with
       | Error _ as error -> error
       | Ok buffer ->
         let mark = { Mark.sym; pos = { row; col } } in
         Ok (Some (buffer, mark)))
    | _ -> failwith "Malformed result from [nvim_get_mark]")
  |> run ~here client
;;

let delete_mark ~(here : [%call_pos]) client sym =
  Nvim_internal.nvim_del_mark ~name:(Char.to_string sym)
  |> map_witness ~f:(function
    | true -> Ok ()
    | false -> Or_error.error_s [%message "Failed to delete mark" (sym : char)])
  |> run ~here client
;;

let get_display_width_of_text ~(here : [%call_pos]) client text =
  Nvim_internal.nvim_strwidth ~text |> run ~here client
;;

let set_current_dir ~(here : [%call_pos]) client dir =
  Nvim_internal.nvim_set_current_dir ~dir |> run ~here client
;;

module Option = struct
  (*$ Vcaml_cinaps.generate_options_impl ~scope:Global *)
  type 'a t =
    | Allowrevins : bool t
    | Ambiwidth : string t
    | Arabicshape : bool t
    | Autochdir : bool t
    | Autowrite : bool t
    | Autowriteall : bool t
    | Background : string t
    | Backspace : string list t
    | Backup : bool t
    | Backupdir : string list t
    | Backupext : string t
    | Backupskip : string list t
    | Belloff : string list t
    | Breakat : char list t
    | Browsedir : string t
    | Casemap : string list t
    | Cdhome : bool t
    | Cdpath : string list t
    | Cedit : string t
    | Charconvert : string t
    | Clipboard : string list t
    | Cmdwinheight : int t
    | Columns : int t
    | Compatible : bool t
    | Completeopt : string list t
    | Completeslash : string t
    | Confirm : bool t
    | Cpoptions : char list t
    | Debug : string t
    | Delcombine : bool t
    | Diffexpr : string t
    | Diffopt : string list t
    | Digraph : bool t
    | Directory : string list t
    | Display : string list t
    | Eadirection : string t
    | Emoji : bool t
    | Equalalways : bool t
    | Errorbells : bool t
    | Errorfile : string t
    | Eventignore : string list t
    | Exrc : bool t
    | Fileencodings : string list t
    | Fileformats : string list t
    | Fileignorecase : bool t
    | Foldclose : string list t
    | Foldlevelstart : int t
    | Foldopen : string list t
    | Fsync : bool t
    | Grepformat : string list t
    | Guicursor : string list t
    | Guifont : string list t
    | Guifontwide : string list t
    | Guioptions : char list t
    | Guitablabel : string t
    | Guitabtooltip : string t
    | Helpfile : string t
    | Helpheight : int t
    | Helplang : string list t
    | Hidden : bool t
    | History : int t
    | Hlsearch : bool t
    | Icon : bool t
    | Iconstring : string t
    | Ignorecase : bool t
    | Imcmdline : bool t
    | Imdisable : bool t
    | Inccommand : string t
    | Incsearch : bool t
    | Isfname : string list t
    | Isident : string list t
    | Isprint : string list t
    | Joinspaces : bool t
    | Jumpoptions : string list t
    | Keymodel : string list t
    | Langmap : string list t
    | Langmenu : string t
    | Langremap : bool t
    | Laststatus : int t
    | Lazyredraw : bool t
    | Lines : int t
    | Linespace : int t
    | Loadplugins : bool t
    | Magic : bool t
    | Makeef : string t
    | Matchtime : int t
    | Maxfuncdepth : int t
    | Maxmapdepth : int t
    | Maxmempattern : int t
    | Menuitems : int t
    | Mkspellmem : string t
    | Modelineexpr : bool t
    | Modelines : int t
    | More : bool t
    | Mouse : char list t
    | Mousefocus : bool t
    | Mousehide : bool t
    | Mousemodel : string t
    | Mousemoveevent : bool t
    | Mousescroll : string list t
    | Mouseshape : string list t
    | Mousetime : int t
    | Opendevice : bool t
    | Operatorfunc : string t
    | Packpath : string list t
    | Paragraphs : string t
    | Patchexpr : string t
    | Patchmode : string t
    | Previewheight : int t
    | Pumblend : int t
    | Pumheight : int t
    | Pumwidth : int t
    | Pyxversion : int t
    | Quickfixtextfunc : string t
    | Redrawdebug : string list t
    | Redrawtime : int t
    | Regexpengine : int t
    | Report : int t
    | Revins : bool t
    | Ruler : bool t
    | Rulerformat : string t
    | Runtimepath : string list t
    | Scrolljump : int t
    | Scrollopt : string list t
    | Sections : string t
    | Selection : string t
    | Selectmode : string list t
    | Shada : string list t
    | Shadafile : string list t
    | Shell : string t
    | Shellcmdflag : string t
    | Shellpipe : string t
    | Shellquote : string t
    | Shellredir : string t
    | Shellslash : bool t
    | Shelltemp : bool t
    | Shellxescape : string t
    | Shellxquote : string t
    | Shiftround : bool t
    | Shortmess : char list t
    | Showcmd : bool t
    | Showcmdloc : string t
    | Showfulltag : bool t
    | Showmatch : bool t
    | Showmode : bool t
    | Showtabline : int t
    | Sidescroll : int t
    | Smartcase : bool t
    | Smarttab : bool t
    | Spellsuggest : string list t
    | Splitbelow : bool t
    | Splitkeep : string t
    | Splitright : bool t
    | Startofline : bool t
    | Suffixes : string list t
    | Switchbuf : string list t
    | Tabline : string t
    | Tabpagemax : int t
    | Tagbsearch : bool t
    | Taglength : int t
    | Tagrelative : bool t
    | Tagstack : bool t
    | Termbidi : bool t
    | Termguicolors : bool t
    | Termpastefilter : string list t
    | Tildeop : bool t
    | Timeout : bool t
    | Timeoutlen : int t
    | Title : bool t
    | Titlelen : int t
    | Titleold : string t
    | Titlestring : string t
    | Ttimeout : bool t
    | Ttimeoutlen : int t
    | Undodir : string list t
    | Undoreload : int t
    | Updatecount : int t
    | Updatetime : int t
    | Verbose : int t
    | Verbosefile : string t
    | Viewdir : string t
    | Viminfo : string t
    | Viminfofile : string t
    | Visualbell : bool t
    | Warn : bool t
    | Whichwrap : char list t
    | Wildchar : int t
    | Wildcharm : int t
    | Wildignore : string list t
    | Wildignorecase : bool t
    | Wildmenu : bool t
    | Wildmode : string list t
    | Wildoptions : string list t
    | Winaltkeys : string t
    | Window : int t
    | Winheight : int t
    | Winminheight : int t
    | Winminwidth : int t
    | Winwidth : int t
    | Wrapscan : bool t
    | Write : bool t
    | Writeany : bool t
    | Writebackup : bool t
    | Writedelay : int t
  [@@deriving sexp_of]

  let to_string (type a) : a t -> string = function
    | Allowrevins -> "allowrevins"
    | Ambiwidth -> "ambiwidth"
    | Arabicshape -> "arabicshape"
    | Autochdir -> "autochdir"
    | Autowrite -> "autowrite"
    | Autowriteall -> "autowriteall"
    | Background -> "background"
    | Backspace -> "backspace"
    | Backup -> "backup"
    | Backupdir -> "backupdir"
    | Backupext -> "backupext"
    | Backupskip -> "backupskip"
    | Belloff -> "belloff"
    | Breakat -> "breakat"
    | Browsedir -> "browsedir"
    | Casemap -> "casemap"
    | Cdhome -> "cdhome"
    | Cdpath -> "cdpath"
    | Cedit -> "cedit"
    | Charconvert -> "charconvert"
    | Clipboard -> "clipboard"
    | Cmdwinheight -> "cmdwinheight"
    | Columns -> "columns"
    | Compatible -> "compatible"
    | Completeopt -> "completeopt"
    | Completeslash -> "completeslash"
    | Confirm -> "confirm"
    | Cpoptions -> "cpoptions"
    | Debug -> "debug"
    | Delcombine -> "delcombine"
    | Diffexpr -> "diffexpr"
    | Diffopt -> "diffopt"
    | Digraph -> "digraph"
    | Directory -> "directory"
    | Display -> "display"
    | Eadirection -> "eadirection"
    | Emoji -> "emoji"
    | Equalalways -> "equalalways"
    | Errorbells -> "errorbells"
    | Errorfile -> "errorfile"
    | Eventignore -> "eventignore"
    | Exrc -> "exrc"
    | Fileencodings -> "fileencodings"
    | Fileformats -> "fileformats"
    | Fileignorecase -> "fileignorecase"
    | Foldclose -> "foldclose"
    | Foldlevelstart -> "foldlevelstart"
    | Foldopen -> "foldopen"
    | Fsync -> "fsync"
    | Grepformat -> "grepformat"
    | Guicursor -> "guicursor"
    | Guifont -> "guifont"
    | Guifontwide -> "guifontwide"
    | Guioptions -> "guioptions"
    | Guitablabel -> "guitablabel"
    | Guitabtooltip -> "guitabtooltip"
    | Helpfile -> "helpfile"
    | Helpheight -> "helpheight"
    | Helplang -> "helplang"
    | Hidden -> "hidden"
    | History -> "history"
    | Hlsearch -> "hlsearch"
    | Icon -> "icon"
    | Iconstring -> "iconstring"
    | Ignorecase -> "ignorecase"
    | Imcmdline -> "imcmdline"
    | Imdisable -> "imdisable"
    | Inccommand -> "inccommand"
    | Incsearch -> "incsearch"
    | Isfname -> "isfname"
    | Isident -> "isident"
    | Isprint -> "isprint"
    | Joinspaces -> "joinspaces"
    | Jumpoptions -> "jumpoptions"
    | Keymodel -> "keymodel"
    | Langmap -> "langmap"
    | Langmenu -> "langmenu"
    | Langremap -> "langremap"
    | Laststatus -> "laststatus"
    | Lazyredraw -> "lazyredraw"
    | Lines -> "lines"
    | Linespace -> "linespace"
    | Loadplugins -> "loadplugins"
    | Magic -> "magic"
    | Makeef -> "makeef"
    | Matchtime -> "matchtime"
    | Maxfuncdepth -> "maxfuncdepth"
    | Maxmapdepth -> "maxmapdepth"
    | Maxmempattern -> "maxmempattern"
    | Menuitems -> "menuitems"
    | Mkspellmem -> "mkspellmem"
    | Modelineexpr -> "modelineexpr"
    | Modelines -> "modelines"
    | More -> "more"
    | Mouse -> "mouse"
    | Mousefocus -> "mousefocus"
    | Mousehide -> "mousehide"
    | Mousemodel -> "mousemodel"
    | Mousemoveevent -> "mousemoveevent"
    | Mousescroll -> "mousescroll"
    | Mouseshape -> "mouseshape"
    | Mousetime -> "mousetime"
    | Opendevice -> "opendevice"
    | Operatorfunc -> "operatorfunc"
    | Packpath -> "packpath"
    | Paragraphs -> "paragraphs"
    | Patchexpr -> "patchexpr"
    | Patchmode -> "patchmode"
    | Previewheight -> "previewheight"
    | Pumblend -> "pumblend"
    | Pumheight -> "pumheight"
    | Pumwidth -> "pumwidth"
    | Pyxversion -> "pyxversion"
    | Quickfixtextfunc -> "quickfixtextfunc"
    | Redrawdebug -> "redrawdebug"
    | Redrawtime -> "redrawtime"
    | Regexpengine -> "regexpengine"
    | Report -> "report"
    | Revins -> "revins"
    | Ruler -> "ruler"
    | Rulerformat -> "rulerformat"
    | Runtimepath -> "runtimepath"
    | Scrolljump -> "scrolljump"
    | Scrollopt -> "scrollopt"
    | Sections -> "sections"
    | Selection -> "selection"
    | Selectmode -> "selectmode"
    | Shada -> "shada"
    | Shadafile -> "shadafile"
    | Shell -> "shell"
    | Shellcmdflag -> "shellcmdflag"
    | Shellpipe -> "shellpipe"
    | Shellquote -> "shellquote"
    | Shellredir -> "shellredir"
    | Shellslash -> "shellslash"
    | Shelltemp -> "shelltemp"
    | Shellxescape -> "shellxescape"
    | Shellxquote -> "shellxquote"
    | Shiftround -> "shiftround"
    | Shortmess -> "shortmess"
    | Showcmd -> "showcmd"
    | Showcmdloc -> "showcmdloc"
    | Showfulltag -> "showfulltag"
    | Showmatch -> "showmatch"
    | Showmode -> "showmode"
    | Showtabline -> "showtabline"
    | Sidescroll -> "sidescroll"
    | Smartcase -> "smartcase"
    | Smarttab -> "smarttab"
    | Spellsuggest -> "spellsuggest"
    | Splitbelow -> "splitbelow"
    | Splitkeep -> "splitkeep"
    | Splitright -> "splitright"
    | Startofline -> "startofline"
    | Suffixes -> "suffixes"
    | Switchbuf -> "switchbuf"
    | Tabline -> "tabline"
    | Tabpagemax -> "tabpagemax"
    | Tagbsearch -> "tagbsearch"
    | Taglength -> "taglength"
    | Tagrelative -> "tagrelative"
    | Tagstack -> "tagstack"
    | Termbidi -> "termbidi"
    | Termguicolors -> "termguicolors"
    | Termpastefilter -> "termpastefilter"
    | Tildeop -> "tildeop"
    | Timeout -> "timeout"
    | Timeoutlen -> "timeoutlen"
    | Title -> "title"
    | Titlelen -> "titlelen"
    | Titleold -> "titleold"
    | Titlestring -> "titlestring"
    | Ttimeout -> "ttimeout"
    | Ttimeoutlen -> "ttimeoutlen"
    | Undodir -> "undodir"
    | Undoreload -> "undoreload"
    | Updatecount -> "updatecount"
    | Updatetime -> "updatetime"
    | Verbose -> "verbose"
    | Verbosefile -> "verbosefile"
    | Viewdir -> "viewdir"
    | Viminfo -> "viminfo"
    | Viminfofile -> "viminfofile"
    | Visualbell -> "visualbell"
    | Warn -> "warn"
    | Whichwrap -> "whichwrap"
    | Wildchar -> "wildchar"
    | Wildcharm -> "wildcharm"
    | Wildignore -> "wildignore"
    | Wildignorecase -> "wildignorecase"
    | Wildmenu -> "wildmenu"
    | Wildmode -> "wildmode"
    | Wildoptions -> "wildoptions"
    | Winaltkeys -> "winaltkeys"
    | Window -> "window"
    | Winheight -> "winheight"
    | Winminheight -> "winminheight"
    | Winminwidth -> "winminwidth"
    | Winwidth -> "winwidth"
    | Wrapscan -> "wrapscan"
    | Write -> "write"
    | Writeany -> "writeany"
    | Writebackup -> "writebackup"
    | Writedelay -> "writedelay"
  ;;

  let[@warning "-33"] of_msgpack (type a) (t : a t) msgpack : a Or_error.t =
    let open Option_helpers in
    match t with
    | Allowrevins -> Type.of_msgpack Bool msgpack
    | Ambiwidth -> Type.of_msgpack String msgpack
    | Arabicshape -> Type.of_msgpack Bool msgpack
    | Autochdir -> Type.of_msgpack Bool msgpack
    | Autowrite -> Type.of_msgpack Bool msgpack
    | Autowriteall -> Type.of_msgpack Bool msgpack
    | Background -> Type.of_msgpack String msgpack
    | Backspace -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Backup -> Type.of_msgpack Bool msgpack
    | Backupdir -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Backupext -> Type.of_msgpack String msgpack
    | Backupskip -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Belloff -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Breakat -> Type.of_msgpack (Custom (module Char_list)) msgpack
    | Browsedir -> Type.of_msgpack String msgpack
    | Casemap -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Cdhome -> Type.of_msgpack Bool msgpack
    | Cdpath -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Cedit -> Type.of_msgpack String msgpack
    | Charconvert -> Type.of_msgpack String msgpack
    | Clipboard -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Cmdwinheight -> Type.of_msgpack Int msgpack
    | Columns -> Type.of_msgpack Int msgpack
    | Compatible -> Type.of_msgpack Bool msgpack
    | Completeopt -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Completeslash -> Type.of_msgpack String msgpack
    | Confirm -> Type.of_msgpack Bool msgpack
    | Cpoptions -> Type.of_msgpack (Custom (module Char_list)) msgpack
    | Debug -> Type.of_msgpack String msgpack
    | Delcombine -> Type.of_msgpack Bool msgpack
    | Diffexpr -> Type.of_msgpack String msgpack
    | Diffopt -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Digraph -> Type.of_msgpack Bool msgpack
    | Directory -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Display -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Eadirection -> Type.of_msgpack String msgpack
    | Emoji -> Type.of_msgpack Bool msgpack
    | Equalalways -> Type.of_msgpack Bool msgpack
    | Errorbells -> Type.of_msgpack Bool msgpack
    | Errorfile -> Type.of_msgpack String msgpack
    | Eventignore -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Exrc -> Type.of_msgpack Bool msgpack
    | Fileencodings -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Fileformats -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Fileignorecase -> Type.of_msgpack Bool msgpack
    | Foldclose -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Foldlevelstart -> Type.of_msgpack Int msgpack
    | Foldopen -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Fsync -> Type.of_msgpack Bool msgpack
    | Grepformat -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Guicursor -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Guifont -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Guifontwide -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Guioptions -> Type.of_msgpack (Custom (module Char_list)) msgpack
    | Guitablabel -> Type.of_msgpack String msgpack
    | Guitabtooltip -> Type.of_msgpack String msgpack
    | Helpfile -> Type.of_msgpack String msgpack
    | Helpheight -> Type.of_msgpack Int msgpack
    | Helplang -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Hidden -> Type.of_msgpack Bool msgpack
    | History -> Type.of_msgpack Int msgpack
    | Hlsearch -> Type.of_msgpack Bool msgpack
    | Icon -> Type.of_msgpack Bool msgpack
    | Iconstring -> Type.of_msgpack String msgpack
    | Ignorecase -> Type.of_msgpack Bool msgpack
    | Imcmdline -> Type.of_msgpack Bool msgpack
    | Imdisable -> Type.of_msgpack Bool msgpack
    | Inccommand -> Type.of_msgpack String msgpack
    | Incsearch -> Type.of_msgpack Bool msgpack
    | Isfname -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Isident -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Isprint -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Joinspaces -> Type.of_msgpack Bool msgpack
    | Jumpoptions -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Keymodel -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Langmap -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Langmenu -> Type.of_msgpack String msgpack
    | Langremap -> Type.of_msgpack Bool msgpack
    | Laststatus -> Type.of_msgpack Int msgpack
    | Lazyredraw -> Type.of_msgpack Bool msgpack
    | Lines -> Type.of_msgpack Int msgpack
    | Linespace -> Type.of_msgpack Int msgpack
    | Loadplugins -> Type.of_msgpack Bool msgpack
    | Magic -> Type.of_msgpack Bool msgpack
    | Makeef -> Type.of_msgpack String msgpack
    | Matchtime -> Type.of_msgpack Int msgpack
    | Maxfuncdepth -> Type.of_msgpack Int msgpack
    | Maxmapdepth -> Type.of_msgpack Int msgpack
    | Maxmempattern -> Type.of_msgpack Int msgpack
    | Menuitems -> Type.of_msgpack Int msgpack
    | Mkspellmem -> Type.of_msgpack String msgpack
    | Modelineexpr -> Type.of_msgpack Bool msgpack
    | Modelines -> Type.of_msgpack Int msgpack
    | More -> Type.of_msgpack Bool msgpack
    | Mouse -> Type.of_msgpack (Custom (module Char_list)) msgpack
    | Mousefocus -> Type.of_msgpack Bool msgpack
    | Mousehide -> Type.of_msgpack Bool msgpack
    | Mousemodel -> Type.of_msgpack String msgpack
    | Mousemoveevent -> Type.of_msgpack Bool msgpack
    | Mousescroll -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Mouseshape -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Mousetime -> Type.of_msgpack Int msgpack
    | Opendevice -> Type.of_msgpack Bool msgpack
    | Operatorfunc -> Type.of_msgpack String msgpack
    | Packpath -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Paragraphs -> Type.of_msgpack String msgpack
    | Patchexpr -> Type.of_msgpack String msgpack
    | Patchmode -> Type.of_msgpack String msgpack
    | Previewheight -> Type.of_msgpack Int msgpack
    | Pumblend -> Type.of_msgpack Int msgpack
    | Pumheight -> Type.of_msgpack Int msgpack
    | Pumwidth -> Type.of_msgpack Int msgpack
    | Pyxversion -> Type.of_msgpack Int msgpack
    | Quickfixtextfunc -> Type.of_msgpack String msgpack
    | Redrawdebug -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Redrawtime -> Type.of_msgpack Int msgpack
    | Regexpengine -> Type.of_msgpack Int msgpack
    | Report -> Type.of_msgpack Int msgpack
    | Revins -> Type.of_msgpack Bool msgpack
    | Ruler -> Type.of_msgpack Bool msgpack
    | Rulerformat -> Type.of_msgpack String msgpack
    | Runtimepath -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Scrolljump -> Type.of_msgpack Int msgpack
    | Scrollopt -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Sections -> Type.of_msgpack String msgpack
    | Selection -> Type.of_msgpack String msgpack
    | Selectmode -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Shada -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Shadafile -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Shell -> Type.of_msgpack String msgpack
    | Shellcmdflag -> Type.of_msgpack String msgpack
    | Shellpipe -> Type.of_msgpack String msgpack
    | Shellquote -> Type.of_msgpack String msgpack
    | Shellredir -> Type.of_msgpack String msgpack
    | Shellslash -> Type.of_msgpack Bool msgpack
    | Shelltemp -> Type.of_msgpack Bool msgpack
    | Shellxescape -> Type.of_msgpack String msgpack
    | Shellxquote -> Type.of_msgpack String msgpack
    | Shiftround -> Type.of_msgpack Bool msgpack
    | Shortmess -> Type.of_msgpack (Custom (module Char_list)) msgpack
    | Showcmd -> Type.of_msgpack Bool msgpack
    | Showcmdloc -> Type.of_msgpack String msgpack
    | Showfulltag -> Type.of_msgpack Bool msgpack
    | Showmatch -> Type.of_msgpack Bool msgpack
    | Showmode -> Type.of_msgpack Bool msgpack
    | Showtabline -> Type.of_msgpack Int msgpack
    | Sidescroll -> Type.of_msgpack Int msgpack
    | Smartcase -> Type.of_msgpack Bool msgpack
    | Smarttab -> Type.of_msgpack Bool msgpack
    | Spellsuggest -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Splitbelow -> Type.of_msgpack Bool msgpack
    | Splitkeep -> Type.of_msgpack String msgpack
    | Splitright -> Type.of_msgpack Bool msgpack
    | Startofline -> Type.of_msgpack Bool msgpack
    | Suffixes -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Switchbuf -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Tabline -> Type.of_msgpack String msgpack
    | Tabpagemax -> Type.of_msgpack Int msgpack
    | Tagbsearch -> Type.of_msgpack Bool msgpack
    | Taglength -> Type.of_msgpack Int msgpack
    | Tagrelative -> Type.of_msgpack Bool msgpack
    | Tagstack -> Type.of_msgpack Bool msgpack
    | Termbidi -> Type.of_msgpack Bool msgpack
    | Termguicolors -> Type.of_msgpack Bool msgpack
    | Termpastefilter -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Tildeop -> Type.of_msgpack Bool msgpack
    | Timeout -> Type.of_msgpack Bool msgpack
    | Timeoutlen -> Type.of_msgpack Int msgpack
    | Title -> Type.of_msgpack Bool msgpack
    | Titlelen -> Type.of_msgpack Int msgpack
    | Titleold -> Type.of_msgpack String msgpack
    | Titlestring -> Type.of_msgpack String msgpack
    | Ttimeout -> Type.of_msgpack Bool msgpack
    | Ttimeoutlen -> Type.of_msgpack Int msgpack
    | Undodir -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Undoreload -> Type.of_msgpack Int msgpack
    | Updatecount -> Type.of_msgpack Int msgpack
    | Updatetime -> Type.of_msgpack Int msgpack
    | Verbose -> Type.of_msgpack Int msgpack
    | Verbosefile -> Type.of_msgpack String msgpack
    | Viewdir -> Type.of_msgpack String msgpack
    | Viminfo -> Type.of_msgpack String msgpack
    | Viminfofile -> Type.of_msgpack String msgpack
    | Visualbell -> Type.of_msgpack Bool msgpack
    | Warn -> Type.of_msgpack Bool msgpack
    | Whichwrap -> Type.of_msgpack (Custom (module Char_list.Comma_separated)) msgpack
    | Wildchar -> Type.of_msgpack Int msgpack
    | Wildcharm -> Type.of_msgpack Int msgpack
    | Wildignore -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Wildignorecase -> Type.of_msgpack Bool msgpack
    | Wildmenu -> Type.of_msgpack Bool msgpack
    | Wildmode -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Wildoptions -> Type.of_msgpack (Custom (module String_list)) msgpack
    | Winaltkeys -> Type.of_msgpack String msgpack
    | Window -> Type.of_msgpack Int msgpack
    | Winheight -> Type.of_msgpack Int msgpack
    | Winminheight -> Type.of_msgpack Int msgpack
    | Winminwidth -> Type.of_msgpack Int msgpack
    | Winwidth -> Type.of_msgpack Int msgpack
    | Wrapscan -> Type.of_msgpack Bool msgpack
    | Write -> Type.of_msgpack Bool msgpack
    | Writeany -> Type.of_msgpack Bool msgpack
    | Writebackup -> Type.of_msgpack Bool msgpack
    | Writedelay -> Type.of_msgpack Int msgpack
  ;;

  let[@warning "-33"] to_msgpack (type a) (t : a t) (value : a) =
    let open Option_helpers in
    match t with
    | Allowrevins -> Type.to_msgpack Bool value
    | Ambiwidth -> Type.to_msgpack String value
    | Arabicshape -> Type.to_msgpack Bool value
    | Autochdir -> Type.to_msgpack Bool value
    | Autowrite -> Type.to_msgpack Bool value
    | Autowriteall -> Type.to_msgpack Bool value
    | Background -> Type.to_msgpack String value
    | Backspace -> Type.to_msgpack (Custom (module String_list)) value
    | Backup -> Type.to_msgpack Bool value
    | Backupdir -> Type.to_msgpack (Custom (module String_list)) value
    | Backupext -> Type.to_msgpack String value
    | Backupskip -> Type.to_msgpack (Custom (module String_list)) value
    | Belloff -> Type.to_msgpack (Custom (module String_list)) value
    | Breakat -> Type.to_msgpack (Custom (module Char_list)) value
    | Browsedir -> Type.to_msgpack String value
    | Casemap -> Type.to_msgpack (Custom (module String_list)) value
    | Cdhome -> Type.to_msgpack Bool value
    | Cdpath -> Type.to_msgpack (Custom (module String_list)) value
    | Cedit -> Type.to_msgpack String value
    | Charconvert -> Type.to_msgpack String value
    | Clipboard -> Type.to_msgpack (Custom (module String_list)) value
    | Cmdwinheight -> Type.to_msgpack Int value
    | Columns -> Type.to_msgpack Int value
    | Compatible -> Type.to_msgpack Bool value
    | Completeopt -> Type.to_msgpack (Custom (module String_list)) value
    | Completeslash -> Type.to_msgpack String value
    | Confirm -> Type.to_msgpack Bool value
    | Cpoptions -> Type.to_msgpack (Custom (module Char_list)) value
    | Debug -> Type.to_msgpack String value
    | Delcombine -> Type.to_msgpack Bool value
    | Diffexpr -> Type.to_msgpack String value
    | Diffopt -> Type.to_msgpack (Custom (module String_list)) value
    | Digraph -> Type.to_msgpack Bool value
    | Directory -> Type.to_msgpack (Custom (module String_list)) value
    | Display -> Type.to_msgpack (Custom (module String_list)) value
    | Eadirection -> Type.to_msgpack String value
    | Emoji -> Type.to_msgpack Bool value
    | Equalalways -> Type.to_msgpack Bool value
    | Errorbells -> Type.to_msgpack Bool value
    | Errorfile -> Type.to_msgpack String value
    | Eventignore -> Type.to_msgpack (Custom (module String_list)) value
    | Exrc -> Type.to_msgpack Bool value
    | Fileencodings -> Type.to_msgpack (Custom (module String_list)) value
    | Fileformats -> Type.to_msgpack (Custom (module String_list)) value
    | Fileignorecase -> Type.to_msgpack Bool value
    | Foldclose -> Type.to_msgpack (Custom (module String_list)) value
    | Foldlevelstart -> Type.to_msgpack Int value
    | Foldopen -> Type.to_msgpack (Custom (module String_list)) value
    | Fsync -> Type.to_msgpack Bool value
    | Grepformat -> Type.to_msgpack (Custom (module String_list)) value
    | Guicursor -> Type.to_msgpack (Custom (module String_list)) value
    | Guifont -> Type.to_msgpack (Custom (module String_list)) value
    | Guifontwide -> Type.to_msgpack (Custom (module String_list)) value
    | Guioptions -> Type.to_msgpack (Custom (module Char_list)) value
    | Guitablabel -> Type.to_msgpack String value
    | Guitabtooltip -> Type.to_msgpack String value
    | Helpfile -> Type.to_msgpack String value
    | Helpheight -> Type.to_msgpack Int value
    | Helplang -> Type.to_msgpack (Custom (module String_list)) value
    | Hidden -> Type.to_msgpack Bool value
    | History -> Type.to_msgpack Int value
    | Hlsearch -> Type.to_msgpack Bool value
    | Icon -> Type.to_msgpack Bool value
    | Iconstring -> Type.to_msgpack String value
    | Ignorecase -> Type.to_msgpack Bool value
    | Imcmdline -> Type.to_msgpack Bool value
    | Imdisable -> Type.to_msgpack Bool value
    | Inccommand -> Type.to_msgpack String value
    | Incsearch -> Type.to_msgpack Bool value
    | Isfname -> Type.to_msgpack (Custom (module String_list)) value
    | Isident -> Type.to_msgpack (Custom (module String_list)) value
    | Isprint -> Type.to_msgpack (Custom (module String_list)) value
    | Joinspaces -> Type.to_msgpack Bool value
    | Jumpoptions -> Type.to_msgpack (Custom (module String_list)) value
    | Keymodel -> Type.to_msgpack (Custom (module String_list)) value
    | Langmap -> Type.to_msgpack (Custom (module String_list)) value
    | Langmenu -> Type.to_msgpack String value
    | Langremap -> Type.to_msgpack Bool value
    | Laststatus -> Type.to_msgpack Int value
    | Lazyredraw -> Type.to_msgpack Bool value
    | Lines -> Type.to_msgpack Int value
    | Linespace -> Type.to_msgpack Int value
    | Loadplugins -> Type.to_msgpack Bool value
    | Magic -> Type.to_msgpack Bool value
    | Makeef -> Type.to_msgpack String value
    | Matchtime -> Type.to_msgpack Int value
    | Maxfuncdepth -> Type.to_msgpack Int value
    | Maxmapdepth -> Type.to_msgpack Int value
    | Maxmempattern -> Type.to_msgpack Int value
    | Menuitems -> Type.to_msgpack Int value
    | Mkspellmem -> Type.to_msgpack String value
    | Modelineexpr -> Type.to_msgpack Bool value
    | Modelines -> Type.to_msgpack Int value
    | More -> Type.to_msgpack Bool value
    | Mouse -> Type.to_msgpack (Custom (module Char_list)) value
    | Mousefocus -> Type.to_msgpack Bool value
    | Mousehide -> Type.to_msgpack Bool value
    | Mousemodel -> Type.to_msgpack String value
    | Mousemoveevent -> Type.to_msgpack Bool value
    | Mousescroll -> Type.to_msgpack (Custom (module String_list)) value
    | Mouseshape -> Type.to_msgpack (Custom (module String_list)) value
    | Mousetime -> Type.to_msgpack Int value
    | Opendevice -> Type.to_msgpack Bool value
    | Operatorfunc -> Type.to_msgpack String value
    | Packpath -> Type.to_msgpack (Custom (module String_list)) value
    | Paragraphs -> Type.to_msgpack String value
    | Patchexpr -> Type.to_msgpack String value
    | Patchmode -> Type.to_msgpack String value
    | Previewheight -> Type.to_msgpack Int value
    | Pumblend -> Type.to_msgpack Int value
    | Pumheight -> Type.to_msgpack Int value
    | Pumwidth -> Type.to_msgpack Int value
    | Pyxversion -> Type.to_msgpack Int value
    | Quickfixtextfunc -> Type.to_msgpack String value
    | Redrawdebug -> Type.to_msgpack (Custom (module String_list)) value
    | Redrawtime -> Type.to_msgpack Int value
    | Regexpengine -> Type.to_msgpack Int value
    | Report -> Type.to_msgpack Int value
    | Revins -> Type.to_msgpack Bool value
    | Ruler -> Type.to_msgpack Bool value
    | Rulerformat -> Type.to_msgpack String value
    | Runtimepath -> Type.to_msgpack (Custom (module String_list)) value
    | Scrolljump -> Type.to_msgpack Int value
    | Scrollopt -> Type.to_msgpack (Custom (module String_list)) value
    | Sections -> Type.to_msgpack String value
    | Selection -> Type.to_msgpack String value
    | Selectmode -> Type.to_msgpack (Custom (module String_list)) value
    | Shada -> Type.to_msgpack (Custom (module String_list)) value
    | Shadafile -> Type.to_msgpack (Custom (module String_list)) value
    | Shell -> Type.to_msgpack String value
    | Shellcmdflag -> Type.to_msgpack String value
    | Shellpipe -> Type.to_msgpack String value
    | Shellquote -> Type.to_msgpack String value
    | Shellredir -> Type.to_msgpack String value
    | Shellslash -> Type.to_msgpack Bool value
    | Shelltemp -> Type.to_msgpack Bool value
    | Shellxescape -> Type.to_msgpack String value
    | Shellxquote -> Type.to_msgpack String value
    | Shiftround -> Type.to_msgpack Bool value
    | Shortmess -> Type.to_msgpack (Custom (module Char_list)) value
    | Showcmd -> Type.to_msgpack Bool value
    | Showcmdloc -> Type.to_msgpack String value
    | Showfulltag -> Type.to_msgpack Bool value
    | Showmatch -> Type.to_msgpack Bool value
    | Showmode -> Type.to_msgpack Bool value
    | Showtabline -> Type.to_msgpack Int value
    | Sidescroll -> Type.to_msgpack Int value
    | Smartcase -> Type.to_msgpack Bool value
    | Smarttab -> Type.to_msgpack Bool value
    | Spellsuggest -> Type.to_msgpack (Custom (module String_list)) value
    | Splitbelow -> Type.to_msgpack Bool value
    | Splitkeep -> Type.to_msgpack String value
    | Splitright -> Type.to_msgpack Bool value
    | Startofline -> Type.to_msgpack Bool value
    | Suffixes -> Type.to_msgpack (Custom (module String_list)) value
    | Switchbuf -> Type.to_msgpack (Custom (module String_list)) value
    | Tabline -> Type.to_msgpack String value
    | Tabpagemax -> Type.to_msgpack Int value
    | Tagbsearch -> Type.to_msgpack Bool value
    | Taglength -> Type.to_msgpack Int value
    | Tagrelative -> Type.to_msgpack Bool value
    | Tagstack -> Type.to_msgpack Bool value
    | Termbidi -> Type.to_msgpack Bool value
    | Termguicolors -> Type.to_msgpack Bool value
    | Termpastefilter -> Type.to_msgpack (Custom (module String_list)) value
    | Tildeop -> Type.to_msgpack Bool value
    | Timeout -> Type.to_msgpack Bool value
    | Timeoutlen -> Type.to_msgpack Int value
    | Title -> Type.to_msgpack Bool value
    | Titlelen -> Type.to_msgpack Int value
    | Titleold -> Type.to_msgpack String value
    | Titlestring -> Type.to_msgpack String value
    | Ttimeout -> Type.to_msgpack Bool value
    | Ttimeoutlen -> Type.to_msgpack Int value
    | Undodir -> Type.to_msgpack (Custom (module String_list)) value
    | Undoreload -> Type.to_msgpack Int value
    | Updatecount -> Type.to_msgpack Int value
    | Updatetime -> Type.to_msgpack Int value
    | Verbose -> Type.to_msgpack Int value
    | Verbosefile -> Type.to_msgpack String value
    | Viewdir -> Type.to_msgpack String value
    | Viminfo -> Type.to_msgpack String value
    | Viminfofile -> Type.to_msgpack String value
    | Visualbell -> Type.to_msgpack Bool value
    | Warn -> Type.to_msgpack Bool value
    | Whichwrap -> Type.to_msgpack (Custom (module Char_list.Comma_separated)) value
    | Wildchar -> Type.to_msgpack Int value
    | Wildcharm -> Type.to_msgpack Int value
    | Wildignore -> Type.to_msgpack (Custom (module String_list)) value
    | Wildignorecase -> Type.to_msgpack Bool value
    | Wildmenu -> Type.to_msgpack Bool value
    | Wildmode -> Type.to_msgpack (Custom (module String_list)) value
    | Wildoptions -> Type.to_msgpack (Custom (module String_list)) value
    | Winaltkeys -> Type.to_msgpack String value
    | Window -> Type.to_msgpack Int value
    | Winheight -> Type.to_msgpack Int value
    | Winminheight -> Type.to_msgpack Int value
    | Winminwidth -> Type.to_msgpack Int value
    | Winwidth -> Type.to_msgpack Int value
    | Wrapscan -> Type.to_msgpack Bool value
    | Write -> Type.to_msgpack Bool value
    | Writeany -> Type.to_msgpack Bool value
    | Writebackup -> Type.to_msgpack Bool value
    | Writedelay -> Type.to_msgpack Int value
  ;;

  (*$*)

  let get (type a) ~(here : [%call_pos]) client (t : a t) : a Deferred.Or_error.t =
    Nvim_internal.nvim_get_option_value ~name:(to_string t) ~opts:String.Map.empty
    |> map_witness ~f:(of_msgpack t)
    |> run ~here client
  ;;

  let set (type a) ~(here : [%call_pos]) client (t : a t) (value : a)
    : unit Deferred.Or_error.t
    =
    Nvim_internal.nvim_set_option_value
      ~name:(to_string t)
      ~value:(to_msgpack t value)
      ~opts:String.Map.empty
    |> run ~here client
  ;;

  let get_dynamic_info (type a) ~(here : [%call_pos]) client (t : a t) =
    Nvim_internal.nvim_get_option_info ~name:(to_string t)
    |> map_witness
         ~f:(Dynamic_option_info.of_msgpack_map ~default_of_msgpack:(of_msgpack t))
    |> run ~here client
  ;;
end

(* These functions are part of the Neovim API but are not exposed in VCaml. *)
module _ = struct
  (* If we are going to expose this function we should only do it in a typesafe way
     similar to the way we expose [nvim_call_function] via [call_function] above. I'm not
     sure there's actually much value in exposing this function since OCaml plugins likely
     will not be using VimL OOP. *)
  let (_ : _) = Nvim_internal.nvim_call_dict_function

  (* We get the API info at codegen time, and we currently don't support any compatibility
     checking. VCaml models parts of Neovim that aren't strictly exposed in the Msgpack
     API, so this compatibility check wouldn't be especially helpful. *)
  let (_ : _) = Nvim_internal.nvim_get_api_info

  (* Since the goal of VCaml is to move away from writing any VimL, I would be surprised
     if we actually wanted to do analysis on a VimL AST (aside from Tree-sitter). *)
  let (_ : _) = Nvim_internal.nvim_parse_expression

  (* These functions are not related to Neovim and shouldn't be part of the API. *)
  let (_ : _) = Nvim_internal.nvim_get_proc
  let (_ : _) = Nvim_internal.nvim_get_proc_children

  (* These functions are not exposed in their current form because the semantics of [set]
     are a bit tricky to reason about properly. In the future we will have a better
     option-setting API that may use these behind the scenes. If needed on a sooner
     timeline we can provide them in the [Expert] module. *)
  let (_ : _) = Nvim_internal.nvim_get_option_value
  let (_ : _) = Nvim_internal.nvim_set_option_value

  module _ = struct
    let _ = Nvim_internal.nvim_select_popupmenu_item
    let _ = Nvim_internal.nvim_set_hl
    let _ = Nvim_internal.nvim_set_hl_ns (* used with [nvim_set_hl] *)
    let _ = Nvim_internal.nvim_win_set_hl_ns (* used with [nvim_set_hl] *)
  end
end
