open Core

type scope =
  | Global
  | Buffer
  | Window
  | Window_per_buffer
  | Tabpage

(* These lists of special cases were produced from source-spelunking + sanity checking.
   They were confirmed by vim devs here: https://github.com/vim/vim/pull/12643/files *)

let window_local_options_with_ignored_global_setting =
  [ "previewwindow"; "scroll"; "winfixheight"; "winfixwidth" ] |> String.Set.of_list
;;

let buffer_local_options_with_ignored_global_setting =
  [ "bufhidden"; "buftype"; "channel"; "filetype"; "modified"; "readonly"; "syntax" ]
  |> String.Set.of_list
;;

(* Although this is a "global or local to tabpage" option, it is marked
   [global_local = false] in the data. This is likely an oversight due to it being a
   singleton category. *)
let global_or_local_to_tabpage = [ "cmdheight" ] |> String.Set.of_list

let options_for_scope =
  let global_or_local_to_tabpage_options, global_options =
    Nvim_internal.Options.Global.options
    |> List.partition_tf ~f:(fun { name; _ } -> Set.mem global_or_local_to_tabpage name)
  in
  let window_options, window_per_buffer_options =
    Nvim_internal.Options.Window.options
    |> List.partition_tf ~f:(fun { name; _ } ->
      Set.mem window_local_options_with_ignored_global_setting name)
  in
  function
  | Global -> global_options
  | Buffer -> Nvim_internal.Options.Buffer.options
  | Window -> window_options
  | Window_per_buffer -> window_per_buffer_options
  | Tabpage -> global_or_local_to_tabpage_options
;;

let generate_options_type ~scope =
  let options = options_for_scope scope in
  let gadt_params =
    match scope with
    | Global | Window | Tabpage -> "'a"
    | Buffer | Window_per_buffer -> "('a, 'global)"
  in
  let gadt =
    options
    |> List.map ~f:(fun { name; global_local; type_ } ->
      let type_phantom =
        match type_ with
        | String -> "string"
        | Int -> "int"
        | Bool -> "bool"
        | Char_list _ -> "char list"
        | String_list -> "string list"
      in
      let scope_phantoms =
        match scope, global_local with
        | Global, true ->
          raise_s [%message "Globally scoped option is marked global-local" name]
        | Global, false -> []
        | Buffer, true -> [ "[`global]" ]
        | Buffer, false ->
          (match Set.mem buffer_local_options_with_ignored_global_setting name with
           | true -> [ "[`none]" ]
           | false -> [ "[`copied]" ])
        | Window, true ->
          raise_s [%message "Special window-local option was marked global-local" name]
        | Window, false -> []
        | Window_per_buffer, true -> [ "[`global]" ]
        | Window_per_buffer, false -> [ "[`copied]" ]
        | Tabpage, _ -> []
      in
      let phantom = String.concat (type_phantom :: scope_phantoms) ~sep:"," in
      [%string "| %{String.capitalize name} : (%{phantom}) t"])
    |> String.concat ~sep:"\n"
  in
  [%string {| type %{gadt_params} t = %{gadt} [@@deriving sexp_of] |}] |> print_endline
;;

let generate_options_to_string ~scope =
  let options = options_for_scope scope in
  let cases =
    options
    |> List.map ~f:(fun { name; _ } ->
      [%string {| | %{String.capitalize name} -> "%{name}" |}])
    |> String.concat ~sep:"\n"
  in
  (match scope with
   | Global | Window | Tabpage ->
     [%string {| let to_string (type a) : a t -> string = function %{cases} |}]
   | Buffer | Window_per_buffer ->
     [%string {| let to_string (type a g) : (a, g) t -> string = function %{cases} |}])
  |> print_endline
;;

let generate_options_of_msgpack ~scope =
  let options = options_for_scope scope in
  let cases =
    options
    |> List.map ~f:(fun { name; type_; _ } ->
      let phantom =
        match type_ with
        | String -> "String"
        | Int -> "Int"
        | Bool -> "Bool"
        | Char_list { commalist = false } -> "Custom (module Char_list)"
        | Char_list { commalist = true } -> "Custom (module Char_list.Comma_separated)"
        | String_list -> "Custom (module String_list)"
      in
      [%string {| | %{String.capitalize name} -> Type.of_msgpack (%{phantom}) msgpack |}])
    |> String.concat ~sep:"\n"
  in
  (match scope with
   | Global | Window | Tabpage ->
     [%string
       {|
       let[@warning "-33"] of_msgpack (type a) (t : a t) msgpack : a Or_error.t =
         let open Option_helpers in
         match t with %{cases} |}]
   | Buffer | Window_per_buffer ->
     [%string
       {|
       let of_msgpack (type a g) (t : (a, g) t) msgpack : a Or_error.t =
         let open Option_helpers in
         match t with %{cases} |}])
  |> print_endline
;;

let generate_options_to_msgpack ~scope =
  let options = options_for_scope scope in
  let cases =
    options
    |> List.map ~f:(fun { name; type_; _ } ->
      let phantom =
        match type_ with
        | String -> "String"
        | Int -> "Int"
        | Bool -> "Bool"
        | Char_list { commalist = false } -> "Custom (module Char_list)"
        | Char_list { commalist = true } -> "Custom (module Char_list.Comma_separated)"
        | String_list -> "Custom (module String_list)"
      in
      [%string {| | %{String.capitalize name} -> Type.to_msgpack (%{phantom}) value |}])
    |> String.concat ~sep:"\n"
  in
  (match scope with
   | Global | Window | Tabpage ->
     [%string
       {|
       let[@warning "-33"] to_msgpack (type a) (t : a t) (value : a) =
         let open Option_helpers in
         match t with %{cases} |}]
   | Buffer | Window_per_buffer ->
     [%string
       {|
       let to_msgpack (type a g) (t : (a, g) t) (value : a) =
         let open Option_helpers in
         match t with %{cases} |}])
  |> print_endline
;;

let generate_buffer_options_kind () =
  let options = Nvim_internal.Options.Buffer.options in
  let cases =
    options
    |> List.map ~f:(fun { name; global_local; _ } ->
      let kind =
        match global_local with
        | true -> "Global"
        | false ->
          (match Set.mem buffer_local_options_with_ignored_global_setting name with
           | true -> "None"
           | false -> "Copied")
      in
      [%string {| | %{String.capitalize name} -> %{kind} |}])
    |> String.concat ~sep:"\n"
  in
  [%string
    {|
    module Global = struct
      type 'a t =
        | Copied : [ `copied ] t
        | Global : [ `global ] t
        | None : [ `none ] t
    end

    let kind (type a g) : (a, g) t -> g Global.t = function
    %{cases} |}]
  |> print_endline
;;

let generate_options_impl ~scope =
  generate_options_type ~scope;
  generate_options_to_string ~scope;
  generate_options_of_msgpack ~scope;
  generate_options_to_msgpack ~scope;
  (* [kind] is currently needed for [Buffer.Option.get] due to the Lua workaround (see the
     module for details). *)
  match scope with
  | Global | Window | Window_per_buffer | Tabpage -> ()
  | Buffer -> generate_buffer_options_kind ()
;;

let generate_options_intf ~scope =
  let () =
    match scope with
    | Global | Window | Tabpage -> ()
    | Buffer ->
      print_endline
        {|
  (** Buffer-specific Neovim options. The ['global] phantom type represents the notion of
      a "global" value for the option. [`none] means setting the global value has no
      effect. [`global] means there is a global value that can be locally overridden.
      [`copied] means the global value is copied to the local value on buffer creation,
      so setting it will only affect new buffers. *) |}
    | Window_per_buffer ->
      print_endline
        {|
  (** Buffer-specific Neovim window options. These operate like buffer options, but the
      "global scope" is per-window. The ['global] phantom type represents the notion of
      a "global" value for the option. [`global] means there is a global value for each
      window that can be locally overridden for any given buffer in the window.
      [`copied] means the (per-window) global value is copied to the (per-buffer,window)
      local value when a new buffer is opened in the window. The details of when and how
      these settings are copied are somewhat arcane, so it's best not to rely on this
      behavior. *) |}
  in
  generate_options_type ~scope
;;
