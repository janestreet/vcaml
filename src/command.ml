module Unshadow = struct
  module Buffer = Buffer
end

open Core
open Async
open Import
open Unshadow

(* To understand the logic around the serialization and deserialization of ranges and
   counts in this module, read https://github.com/neovim/neovim/issues/24081. The test
   cases may also be a helpful reference to understand the serialization and
   deserialization logic. *)

module Number_of_arguments = struct
  type t =
    | Zero
    | One
    | Any
    | At_most_one
    | At_least_one
  [@@deriving sexp_of]

  let of_msgpack = function
    | Msgpack.Nil | Int 0 | String "0" -> Ok Zero
    | Int 1 | String "1" -> Ok One
    | String "*" -> Ok Any
    | String "?" -> Ok At_most_one
    | String "+" -> Ok At_least_one
    | nargs -> Or_error.error_s [%message "Malformed [nargs]" (nargs : Msgpack.t)]
  ;;

  let to_msgpack = function
    | Zero -> Msgpack.Int 0
    | One -> Int 1
    | Any -> String "*"
    | At_most_one -> String "?"
    | At_least_one -> String "+"
  ;;
end

module Completion = struct
  type t =
    | Arglist
    | Augroup
    | Buffer
    | Behave
    | Color
    | Command
    | Compiler
    | Cscope
    | Dir
    | Environment
    | Event
    | Expression
    | File
    | File_in_path
    | Filetype
    | Function
    | Help
    | Highlight
    | History
    | Locale
    | Lua
    | Mapclear
    | Mapping
    | Menu
    | Messages
    | Option
    | Packadd
    | Shellcmd
    | Sign
    | Syntax
    | Syntime
    | Tag
    | Tag_listfiles
    | User
    | Var
    | Custom of { f : string }
    | Customlist of { f : string }
  [@@deriving sexp_of]

  let of_msgpack_map map =
    let open Or_error.Let_syntax in
    match%bind Map.find_or_error map "complete" with
    | Msgpack.Nil -> Ok None
    | String "arglist" -> Ok (Some Arglist)
    | String "augroup" -> Ok (Some Augroup)
    | String "buffer" -> Ok (Some Buffer)
    | String "behave" -> Ok (Some Behave)
    | String "color" -> Ok (Some Color)
    | String "command" -> Ok (Some Command)
    | String "compiler" -> Ok (Some Compiler)
    | String "cscope" -> Ok (Some Cscope)
    | String "dir" -> Ok (Some Dir)
    | String "environment" -> Ok (Some Environment)
    | String "event" -> Ok (Some Event)
    | String "expression" -> Ok (Some Expression)
    | String "file" -> Ok (Some File)
    | String "file_in_path" -> Ok (Some File_in_path)
    | String "filetype" -> Ok (Some Filetype)
    | String "function" -> Ok (Some Function)
    | String "help" -> Ok (Some Help)
    | String "highlight" -> Ok (Some Highlight)
    | String "history" -> Ok (Some History)
    | String "locale" -> Ok (Some Locale)
    | String "lua" -> Ok (Some Lua)
    | String "mapclear" -> Ok (Some Mapclear)
    | String "mapping" -> Ok (Some Mapping)
    | String "menu" -> Ok (Some Menu)
    | String "messages" -> Ok (Some Messages)
    | String "option" -> Ok (Some Option)
    | String "packadd" -> Ok (Some Packadd)
    | String "shellcmd" -> Ok (Some Shellcmd)
    | String "sign" -> Ok (Some Sign)
    | String "syntax" -> Ok (Some Syntax)
    | String "syntime" -> Ok (Some Syntime)
    | String "tag" -> Ok (Some Tag)
    | String "tag_listfiles" -> Ok (Some Tag_listfiles)
    | String "user" -> Ok (Some User)
    | String "var" -> Ok (Some Var)
    | String "custom" ->
      let%map f = find_or_error_and_convert map "complete_arg" (Type.of_msgpack String) in
      Some (Custom { f })
    | String "customlist" ->
      let%map f = find_or_error_and_convert map "complete_arg" (Type.of_msgpack String) in
      Some (Customlist { f })
    | complete ->
      Or_error.error_s [%message "Malformed [complete]" (complete : Msgpack.t)]
  ;;

  let to_msgpack = function
    | Arglist -> Msgpack.String "arglist"
    | Augroup -> String "augroup"
    | Buffer -> String "buffer"
    | Behave -> String "behave"
    | Color -> String "color"
    | Command -> String "command"
    | Compiler -> String "compiler"
    | Cscope -> String "cscope"
    | Dir -> String "dir"
    | Environment -> String "environment"
    | Event -> String "event"
    | Expression -> String "expression"
    | File -> String "file"
    | File_in_path -> String "file_in_path"
    | Filetype -> String "filetype"
    | Function -> String "function"
    | Help -> String "help"
    | Highlight -> String "highlight"
    | History -> String "history"
    | Locale -> String "locale"
    | Lua -> String "lua"
    | Mapclear -> String "mapclear"
    | Mapping -> String "mapping"
    | Menu -> String "menu"
    | Messages -> String "messages"
    | Option -> String "option"
    | Packadd -> String "packadd"
    | Shellcmd -> String "shellcmd"
    | Sign -> String "sign"
    | Syntax -> String "syntax"
    | Syntime -> String "syntime"
    | Tag -> String "tag"
    | Tag_listfiles -> String "tag_listfiles"
    | User -> String "user"
    | Var -> String "var"
    | Custom { f } -> String ("custom," ^ f)
    | Customlist { f } -> String ("customlist," ^ f)
  ;;
end

module Modifiers = struct
  module Filter = struct
    type t =
      | Only of string
      | Excluding of string
    [@@deriving sexp_of]
  end

  module Silent = struct
    type t =
      | No
      | Yes of { silence_errors : bool }
    [@@deriving sexp_of]
  end

  module Split = struct
    type t =
      | Above_left
      | Below_right
      | Top_left
      | Bottom_right
    [@@deriving sexp_of]
  end

  type t =
    { filter : Filter.t option
    ; silent : Silent.t
    ; unsilent : bool
    ; sandbox : bool
    ; noautocmd : bool
    ; browse : bool
    ; confirm : bool
    ; hide : bool
    ; horizontal : bool
    ; vertical : bool
    ; split : Split.t option
    ; keepalt : bool
    ; keepjumps : bool
    ; keepmarks : bool
    ; keeppatterns : bool
    ; lockmarks : bool
    ; noswapfile : bool
    ; tab : int option
    ; verbose : int option
    }
  [@@deriving sexp_of]
end

module Range_or_count = struct
  module Of = struct
    type t =
      | Lines
      | Args
      | Buffers
      | Loaded_buffers
      | Windows
      | Tabs
      | Quickfix_entries
      | Other
    [@@deriving sexp_of]
  end

  module Spec = struct
    module Default_range = struct
      type t =
        | Current
        | All
      [@@deriving sexp_of]
    end

    module How_count_can_be_passed = struct
      type t =
        | Only_in_line_number_position
        | In_line_number_position_or_as_first_argument
      [@@deriving sexp_of]
    end

    type t =
      | Range of
          { default : Default_range.t
          ; of_ : Of.t
          }
      | Count of
          { default : int
          ; can_be_passed : How_count_can_be_passed.t
          ; of_ : Of.t
          }
    [@@deriving sexp_of]

    let of_msgpack_map map =
      let open Or_error.Let_syntax in
      let%bind of_ =
        match%bind Map.find_or_error map "addr" with
        | Msgpack.Nil | String "lines" -> Ok Of.Lines
        | String "arguments" -> Ok Args
        | String "buffers" -> Ok Buffers
        | String "loaded_buffers" -> Ok Loaded_buffers
        | String "windows" -> Ok Windows
        | String "tabs" -> Ok Tabs
        | String "quickfix" -> Ok Quickfix_entries
        | String "other" -> Ok Other
        | addr -> Or_error.error_s [%message "Malformed [addr]" (addr : Msgpack.t)]
      in
      let%bind range =
        match%bind Map.find_or_error map "range" with
        | Nil -> Ok None
        | Int count -> Ok (Some (`Count count))
        | String count when String.for_all count ~f:Char.is_digit ->
          Ok (Some (`Count (Int.of_string count)))
        | String "." -> Ok (Some `Current)
        | String "%" -> Ok (Some `All)
        | range -> Or_error.error_s [%message "Malformed [range]" (range : Msgpack.t)]
      in
      let%bind count =
        match%bind Map.find_or_error map "count" with
        | Nil -> Ok None
        | Int count -> Ok (Some count)
        | String count when String.for_all count ~f:Char.is_digit ->
          Ok (Some (Int.of_string count))
        | count -> Or_error.error_s [%message "Malformed [count]" (count : Msgpack.t)]
      in
      match count with
      | Some default ->
        Ok
          (Some
             (Count
                { default
                ; can_be_passed = In_line_number_position_or_as_first_argument
                ; of_
                }))
      | None ->
        (match range with
         | None -> Ok None
         | Some (`Count default) ->
           Ok
             (Some (Count { default; can_be_passed = Only_in_line_number_position; of_ }))
         | Some `Current -> Ok (Some (Range { default = Current; of_ }))
         | Some `All -> Ok (Some (Range { default = All; of_ })))
    ;;

    let to_msgpack_map range_or_count =
      let of_, range_or_count =
        match range_or_count with
        | Range { default = Current; of_ } -> of_, ("range", Msgpack.Bool true)
        | Range { default = All; of_ } -> of_, ("range", String "%")
        | Count { default; can_be_passed = Only_in_line_number_position; of_ } ->
          of_, ("range", Int default)
        | Count
            { default; can_be_passed = In_line_number_position_or_as_first_argument; of_ }
          -> of_, ("count", Int default)
      in
      let (addr : Msgpack.t) =
        match of_ with
        | Lines -> String "lines"
        | Args -> String "arguments"
        | Buffers -> String "buffers"
        | Loaded_buffers -> String "loaded_buffers"
        | Windows -> String "windows"
        | Tabs -> String "tabs"
        | Quickfix_entries -> String "quickfix"
        | Other -> String "other"
      in
      [ "addr", addr; range_or_count ] |> String.Map.of_alist_exn
    ;;
  end

  type t =
    | Range of
        { start_inclusive : int
        ; end_inclusive : int
        }
    | Count of int
  [@@deriving sexp_of]
end

let create
  ~(here : [%call_pos])
  client
  ?keepscript
  ?bang
  ?bar
  ?register
  ?nargs
  ?range_or_count
  ?completion
  ?fail_if_exists
  ()
  ~name
  ~scope
  command
  =
  let%bind.Deferred.Or_error scope =
    match scope with
    | `Global | `Buffer_local (Buffer.Or_current.Id _) -> Deferred.Or_error.return scope
    | `Buffer_local Current ->
      let%map.Deferred.Or_error buffer = Nvim.get_current_buf ~here client in
      `Buffer_local (Buffer.Or_current.Id buffer)
  in
  let%bind command =
    match (command : unit Ocaml_from_nvim.Callback.t) with
    | Viml command -> return command
    | Rpc rpc ->
      let name =
        Ocaml_from_nvim.Private.register_callback ~here client ~return_type:Nil rpc
      in
      let channel = Client.channel client in
      let%map () =
        match scope with
        | `Global -> return ()
        | `Buffer_local buffer ->
          Autocmd.create
            ~here
            client
            ~description:[%string "Unregister %{name}"]
            ~once:true
            ~patterns_or_buffer:(Buffer buffer)
            ~group:(Autocmd.Private.vcaml_internal_group client)
            ~events:[ BufDelete; BufWipeout ]
            (Viml
               [%string
                 {|
if !empty(nvim_get_chan_info(%{channel#Int}))
  call rpcnotify(%{channel#Int}, "%{Client.Private.unregister_blocking_rpc}", "%{name}")
endif |}])
          |> Deferred.ignore_m
      in
      [%string {|call rpcrequest(%{channel#Int}, "%{name}")|}]
  in
  let opts =
    let map =
      let maybe name var conv = Option.map var ~f:(fun var -> name, conv var) in
      [ maybe "bang" bang (Type.to_msgpack Bool)
      ; maybe "bar" bar (Type.to_msgpack Bool)
      ; maybe "register" register (Type.to_msgpack Bool)
      ; maybe "keepscript" keepscript (Type.to_msgpack Bool)
      ; maybe "nargs" nargs Number_of_arguments.to_msgpack
      ; maybe "complete" completion Completion.to_msgpack
      ; maybe "force" fail_if_exists (* [force] defaults to [true]. *) (fun fail ->
          Type.to_msgpack Bool (not fail))
      ]
      |> List.filter_opt
      |> String.Map.of_alist_exn
    in
    match range_or_count with
    | None -> map
    | Some range_or_count ->
      Core.Map.merge_skewed
        map
        (Range_or_count.Spec.to_msgpack_map range_or_count)
        ~combine:(fun ~key _ _ ->
          raise_s [%message "BUG: Key appears twice in command specification" key])
  in
  let query =
    match scope with
    | `Global -> Nvim_internal.nvim_create_user_command
    | `Buffer_local buffer -> Nvim_internal.nvim_buf_create_user_command ~buffer
  in
  query ~name ~command:(String command) ~opts |> run ~here client
;;

let delete ~(here : [%call_pos]) client name ~scope =
  let query =
    match scope with
    | `Global -> Nvim_internal.nvim_del_user_command
    | `Buffer_local buffer -> Nvim_internal.nvim_buf_del_user_command ~buffer
  in
  query ~name |> run ~here client
;;

let exec_internal
  ~output
  ~map_witness_f
  here
  client
  ?range_or_count
  ?bang
  ?register
  ?args
  ?expand_args
  ?filter
  ?silent
  ?unsilent
  ?sandbox
  ?noautocmd
  ?browse
  ?confirm
  ?hide
  ?horizontal
  ?vertical
  ?split
  ?keepalt
  ?keepjumps
  ?keepmarks
  ?keeppatterns
  ?lockmarks
  ?noswapfile
  ?tab
  ?verbose
  name
  =
  let maybe name var conv = Option.map var ~f:(fun var -> name, conv var) in
  let modifiers =
    let silent, silence_errors =
      match silent with
      | None -> None, None
      | Some Modifiers.Silent.No -> Some false, Some false
      | Some (Yes { silence_errors }) -> Some true, Some silence_errors
    in
    [ maybe "filter" filter (fun filter ->
        let pattern, force =
          match (filter : Modifiers.Filter.t) with
          | Only pattern -> pattern, false
          | Excluding pattern -> pattern, true
        in
        [ "pattern", Msgpack.String pattern; "force", Bool force ]
        |> String.Map.of_alist_exn
        |> Type.to_msgpack Dict)
    ; maybe "silent" silent (Type.to_msgpack Bool)
    ; maybe "emsg_silent" silence_errors (Type.to_msgpack Bool)
    ; maybe "unsilent" unsilent (Type.to_msgpack Bool)
    ; maybe "sandbox" sandbox (Type.to_msgpack Bool)
    ; maybe "noautocmd" noautocmd (Type.to_msgpack Bool)
    ; maybe "browse" browse (Type.to_msgpack Bool)
    ; maybe "confirm" confirm (Type.to_msgpack Bool)
    ; maybe "hide" hide (Type.to_msgpack Bool)
    ; maybe "horizontal" horizontal (Type.to_msgpack Bool)
    ; maybe "vertical" vertical (Type.to_msgpack Bool)
    ; maybe "split" split (function
        | Modifiers.Split.Above_left -> Msgpack.String "aboveleft"
        | Below_right -> String "belowright"
        | Top_left -> String "topleft"
        | Bottom_right -> String "botright")
    ; maybe "keepalt" keepalt (Type.to_msgpack Bool)
    ; maybe "keepjumps" keepjumps (Type.to_msgpack Bool)
    ; maybe "keepmarks" keepmarks (Type.to_msgpack Bool)
    ; maybe "keeppatterns" keeppatterns (Type.to_msgpack Bool)
    ; maybe "lockmarks" lockmarks (Type.to_msgpack Bool)
    ; maybe "noswapfile" noswapfile (Type.to_msgpack Bool)
    ; maybe "tab" tab (Type.to_msgpack Int)
    ; maybe "verbose" verbose (Type.to_msgpack Int)
    ]
    |> List.filter_opt
    |> function
    | [] -> []
    | modifiers -> [ "mods", Type.to_msgpack Dict (String.Map.of_alist_exn modifiers) ]
  in
  let cmd =
    ([ Some ("cmd", Msgpack.String name)
     ; Option.map
         (range_or_count : Range_or_count.t option)
         ~f:(function
           | Range { start_inclusive; end_inclusive } ->
             let range =
               match start_inclusive = end_inclusive with
               | true -> [ start_inclusive ]
               | false -> [ start_inclusive; end_inclusive ]
             in
             "range", Msgpack.Array (List.map range ~f:(fun x -> Msgpack.Int x))
           | Count count -> "count", Int count)
     ; maybe "bang" bang (Type.to_msgpack Bool)
     ; maybe "reg" register (fun char -> Msgpack.String (Char.to_string char))
     ; maybe "args" args (Type.to_msgpack (Array String))
     ; maybe "magic" expand_args (fun expand ->
         Msgpack.Map [ String "file", Bool expand ])
     ]
     |> List.filter_opt)
    @ modifiers
    |> String.Map.of_alist_exn
  in
  let opts = [ "output", Msgpack.Bool output ] |> String.Map.of_alist_exn in
  match%bind
    Nvim_internal.nvim_cmd ~cmd ~opts |> map_witness ~f:map_witness_f |> run ~here client
  with
  | Ok _ as ok -> return ok
  | Error error ->
    (* This is a brittle attempt to rescue a failure to invoke [nvim_cmd] because [count]
       was passed to a command that was defined with [-range=N], which, though it is a
       count in function, needs to be passed in the [range] field (analogous to being
       passed in the line number position). A test case exercises this scenario. *)
    (match range_or_count with
     | Some (Count count) ->
       (match Error.sexp_of_t error with
        | List
            [ List
                [ Atom "Vim returned error"
                ; Atom msg
                ; List [ Atom "error_type"; Atom "Validation" ]
                ]
            ; _
            ]
          when String.equal msg [%string "Command cannot accept count: %{name}"] ->
          let cmd =
            Map.remove cmd "count"
            |> Map.set ~key:"range" ~data:(Msgpack.Array [ Int count ])
          in
          Nvim_internal.nvim_cmd ~cmd ~opts
          |> map_witness ~f:map_witness_f
          |> run ~here client
          >>| (function
           | Ok _ as ok -> ok
           | Error _ -> Error error)
        | _ -> return (Error error))
     | None | Some (Range _) -> return (Error error))
;;

let exec ~(here : [%call_pos]) client =
  exec_internal
    ~output:false
    ~map_witness_f:(function
      | "" -> Ok ()
      | output ->
        Or_error.error_s [%message "Bug: [exec] produced unexpected output" output])
    here
    client
;;

let exec_and_capture_output ~(here : [%call_pos]) client =
  exec_internal ~output:true ~map_witness_f:Or_error.return here client
;;

module Definition = struct
  type t =
    { name : string
    ; definition : string
    ; script_id : int
    ; keepscript : bool
    ; bang : bool
    ; bar : bool
    ; register : bool
    ; nargs : Number_of_arguments.t
    ; range_or_count : Range_or_count.Spec.t option
    ; completion : Completion.t option
    }
  [@@deriving sexp_of]

  let of_msgpack msgpack =
    let open Or_error.Let_syntax in
    let%bind map = Type.of_msgpack Dict msgpack in
    let find_key = find_or_error_and_convert in
    let%bind name = find_key map "name" (Type.of_msgpack String) in
    let%bind definition = find_key map "definition" (Type.of_msgpack String) in
    let%bind script_id = find_key map "script_id" (Type.of_msgpack Int) in
    let%bind keepscript = find_key map "keepscript" (Type.of_msgpack Bool) in
    let%bind bang = find_key map "bang" (Type.of_msgpack Bool) in
    let%bind bar = find_key map "bar" (Type.of_msgpack Bool) in
    let%bind register = find_key map "register" (Type.of_msgpack Bool) in
    let%bind nargs = find_key map "nargs" Number_of_arguments.of_msgpack in
    let%bind range_or_count = Range_or_count.Spec.of_msgpack_map map in
    let%bind completion = Completion.of_msgpack_map map in
    return
      { name
      ; definition
      ; script_id
      ; keepscript
      ; bang
      ; bar
      ; register
      ; nargs
      ; range_or_count
      ; completion
      }
  ;;
end

let user_defined_commands ~(here : [%call_pos]) client ~scope =
  let query =
    match scope with
    | `Global -> Nvim_internal.nvim_get_commands
    | `Buffer_local buffer -> Nvim_internal.nvim_buf_get_commands ~buffer
  in
  (* [opts] is not used by this version of Neovim, but may be used in the future. If we
     expose it, we should do so in a typeful way rather than asking the user to build
     [Msgpack.t] values. *)
  query ~opts:String.Map.empty
  |> map_witness ~f:(Fn.compose Or_error.return (Map.map ~f:Definition.of_msgpack))
  |> run ~here client
;;

module Parse_result = struct
  module Range_or_count = struct
    module Of = Range_or_count.Of

    type t =
      | Range of
          { start_inclusive : int
          ; end_inclusive : int
          ; of_ : Of.t
          }
      | Count of
          { count : int
          ; of_ : Of.t
          ; source : [ `Default_when_omitted | `Specified ]
          }
      | Ambiguous_count_or_singleton_range of
          { value : int
          ; of_ : Of.t
          }
    [@@deriving sexp_of]
  end

  type t =
    { name : string
    ; range_or_count : Range_or_count.t option
    ; bang : bool
    ; bar : bool
    ; register : [ `This of char | `Not_provided | `Not_applicable ]
    ; args : string list
    ; expand_args : bool
    ; nargs : Number_of_arguments.t
    ; nextcmd : string option
    ; modifiers : Modifiers.t
    }
  [@@deriving sexp_of]

  let of_msgpack_map map =
    let open Or_error.Let_syntax in
    let find_key = find_or_error_and_convert in
    let%bind name = find_key map "cmd" (Type.of_msgpack String) in
    let%bind range_or_count =
      let%bind of_ =
        match%bind find_key map "addr" (Type.of_msgpack String) with
        | "none" -> Ok None
        | "line" -> Ok (Some Range_or_count.Of.Lines)
        | "arg" -> Ok (Some Args)
        | "buf" -> Ok (Some Buffers)
        | "load" -> Ok (Some Loaded_buffers)
        | "win" -> Ok (Some Windows)
        | "tab" -> Ok (Some Tabs)
        | "qf" -> Ok (Some Quickfix_entries)
        | "?" -> Ok (Some Other)
        | addr -> Or_error.error_s [%message "Malformed [addr]" addr]
      in
      match of_ with
      | None -> return None
      | Some of_ ->
        let%bind count = find_and_convert map "count" (Type.of_msgpack Int) in
        let%bind range = find_and_convert map "range" (Type.of_msgpack (Array Int)) in
        (match count, range with
         | None, (None | Some []) -> Ok None
         | _, Some (_ :: _ :: _ :: _ as range) ->
           Or_error.error_s
             [%message "Range provided with >2 elements" (range : int list)]
         | None, Some [ value ] ->
           Ok (Some (Range_or_count.Ambiguous_count_or_singleton_range { value; of_ }))
         | None, Some [ start_inclusive; end_inclusive ] ->
           Ok (Some (Range_or_count.Range { start_inclusive; end_inclusive; of_ }))
         | Some count, (None | Some []) ->
           Ok (Some (Range_or_count.Count { count; of_; source = `Default_when_omitted }))
         | Some count, Some [ range ] ->
           (match count = range with
            | true -> Ok (Some (Range_or_count.Count { count; of_; source = `Specified }))
            | false ->
              (* If we hit this case, there's likely something broken in the way we are
                 modeling ranges and counts. *)
              Or_error.error_s
                [%message
                  "Default values for [range] and [count] differ"
                    (count : int)
                    (range : int)])
         | Some _, Some [ start_inclusive; end_inclusive ] ->
           (* The behavior for this case is ill-defined: a 2-argument range is being
              passed to a command that accepts a count. We get the most similar results to
              native behavior by just using the range [nvim_parse_cmd] returns and
              dropping the count. *)
           Ok (Some (Range { start_inclusive; end_inclusive; of_ })))
    in
    let%bind register =
      match%map find_and_convert map "reg" (Type.of_msgpack String) with
      | None -> `Not_applicable
      | Some "" -> `Not_provided
      | Some register -> `This (Char.of_string register)
    in
    let%bind bang = find_key map "bang" (Type.of_msgpack Bool) in
    let%bind args = find_key map "args" (Type.of_msgpack (Array String)) in
    let%bind nargs = find_key map "nargs" Number_of_arguments.of_msgpack in
    let%bind nextcmd =
      match%map find_key map "nextcmd" (Type.of_msgpack String) with
      | "" -> None
      | nextcmd -> Some nextcmd
    in
    let%bind expand_args, bar =
      let%bind map = find_key map "magic" (Type.of_msgpack Dict) in
      let%bind expand_args = find_key map "file" (Type.of_msgpack Bool) in
      let%bind bar = find_key map "bar" (Type.of_msgpack Bool) in
      return (expand_args, bar)
    in
    let%bind modifiers =
      let%bind map = find_key map "mods" (Type.of_msgpack Dict) in
      let%bind filter =
        let%bind map = find_key map "filter" (Type.of_msgpack Dict) in
        match%bind find_key map "pattern" (Type.of_msgpack String) with
        | "" -> return None
        | pattern ->
          (match%map find_key map "force" (Type.of_msgpack Bool) with
           | false -> Some (Modifiers.Filter.Only pattern)
           | true -> Some (Excluding pattern))
      in
      let%bind silent =
        match%bind find_key map "emsg_silent" (Type.of_msgpack Bool) with
        | true -> return (Modifiers.Silent.Yes { silence_errors = true })
        | false ->
          (match%map find_key map "silent" (Type.of_msgpack Bool) with
           | true -> Modifiers.Silent.Yes { silence_errors = false }
           | false -> No)
      in
      let%bind unsilent = find_key map "unsilent" (Type.of_msgpack Bool) in
      let%bind sandbox = find_key map "sandbox" (Type.of_msgpack Bool) in
      let%bind noautocmd = find_key map "noautocmd" (Type.of_msgpack Bool) in
      let%bind browse = find_key map "browse" (Type.of_msgpack Bool) in
      let%bind confirm = find_key map "confirm" (Type.of_msgpack Bool) in
      let%bind hide = find_key map "hide" (Type.of_msgpack Bool) in
      let%bind horizontal = find_key map "horizontal" (Type.of_msgpack Bool) in
      let%bind vertical = find_key map "vertical" (Type.of_msgpack Bool) in
      let%bind keepalt = find_key map "keepalt" (Type.of_msgpack Bool) in
      let%bind keepjumps = find_key map "keepjumps" (Type.of_msgpack Bool) in
      let%bind keepmarks = find_key map "keepmarks" (Type.of_msgpack Bool) in
      let%bind keeppatterns = find_key map "keeppatterns" (Type.of_msgpack Bool) in
      let%bind lockmarks = find_key map "lockmarks" (Type.of_msgpack Bool) in
      let%bind noswapfile = find_key map "noswapfile" (Type.of_msgpack Bool) in
      let%bind tab =
        match%map find_key map "tab" (Type.of_msgpack Int) with
        | -1 -> None
        | tab -> Some tab
      in
      let%bind verbose =
        match%map find_key map "verbose" (Type.of_msgpack Int) with
        | -1 -> None
        | verbose -> Some verbose
      in
      let%bind split =
        match%bind find_key map "split" (Type.of_msgpack String) with
        | "" -> Ok None
        | "aboveleft" -> Ok (Some Modifiers.Split.Above_left)
        | "belowright" -> Ok (Some Below_right)
        | "topleft" -> Ok (Some Top_left)
        | "botright" -> Ok (Some Bottom_right)
        | split -> Or_error.error_s [%message "Unrecognized split modifier" split]
      in
      { Modifiers.filter
      ; silent
      ; unsilent
      ; sandbox
      ; noautocmd
      ; browse
      ; confirm
      ; hide
      ; horizontal
      ; vertical
      ; split
      ; keepalt
      ; keepjumps
      ; keepmarks
      ; keeppatterns
      ; lockmarks
      ; noswapfile
      ; tab
      ; verbose
      }
      |> return
    in
    return
      { name
      ; range_or_count
      ; bang
      ; bar
      ; register
      ; args
      ; expand_args
      ; nargs
      ; nextcmd
      ; modifiers
      }
  ;;
end

module Fast = struct
  let parse ~(here : [%call_pos]) client str =
    (* [opts] is not used by this version of Neovim, but may be used in the future. If we
       expose it, we should do so in a typeful way rather than asking the user to build
       [Msgpack.t] values. *)
    Nvim_internal.nvim_parse_cmd ~str ~opts:String.Map.empty
    |> map_witness ~f:Parse_result.of_msgpack_map
    |> run ~here client
  ;;
end
