open Core
open Async
include Import0

let run
  (* this function shouldn't be called by users, so it's not [call_pos] in order to make
     sure that we thread the correct positions through *)
  ~(here : Source_code_position.t)
  client
  api_result
  =
  let client = Type_equal.conv Client.Private.eq client in
  client.call_nvim_api_fn ~here Request api_result
;;

module Atomic = struct
  type t = T : 'a Api_result.t -> t [@@unboxed]

  module Error = struct
    type t_without_position =
      | Msgpack_rpc_error of Error.t
      | Unexpected_format of Msgpack.t
      | Call_failed of
          { partial_results : Msgpack.t list
          ; index_of_failure : int
          ; error_type : Error_type.t
          ; message : string
          }

    type t = t_without_position * Source_code_position.t

    let to_error ((t_without_position, here) : t) =
      let sexp =
        match t_without_position with
        | Msgpack_rpc_error error -> [%sexp (error : Error.t)]
        | Unexpected_format msgpack ->
          [%message
            "nvim_call_atomic returned OK result with unexpected format"
              (msgpack : Msgpack.t)]
        | Call_failed { partial_results; index_of_failure; error_type; message } ->
          [%message.omit_nil
            "One of the calls in the nvim_call_atomic batch failed"
              (partial_results : Msgpack.t list)
              (index_of_failure : int)
              (error_type : Error_type.t)
              message]
      in
      let here = [%sexp (here : Source_code_position.t)] in
      Error.create_s (List [ sexp; List [ List [ Atom "Called from"; here ] ] ])
    ;;
  end

  let run ~(here : [%call_pos]) client calls =
    let calls =
      List.map calls ~f:(fun (T { name; params; witness = _ }) ->
        Msgpack.Array [ String name; Array params ])
    in
    let nvim_call_atomic =
      (* Since we already expect a particular output format below, we want to report any
         failures due to unexpected format using [Unexpected_format]. *)
      let api_result = Nvim_internal.nvim_call_atomic ~calls in
      { api_result with witness = Object }
    in
    match%map run ~here client nvim_call_atomic with
    | Error error -> Error (Error.Msgpack_rpc_error error, here)
    | Ok (Array [ Array results; Nil ]) -> Ok results
    | Ok
        (Array
          [ Array partial_results
          ; Array [ Int index_of_failure; Int error_type; String message ]
          ]) ->
      let error_type = Error_type.of_int error_type in
      Error (Call_failed { partial_results; index_of_failure; error_type; message }, here)
    | Ok response -> Error (Unexpected_format response, here)
  ;;
end

let run2
  (type a b)
  ~(here : [%call_pos])
  client
  (api_result1 : a Api_result.t)
  (api_result2 : b Api_result.t)
  =
  match%map Atomic.run ~here client [ T api_result1; T api_result2 ] with
  | Error error -> Error (Atomic.Error.to_error error)
  | Ok [ result1; result2 ] ->
    let open Or_error.Let_syntax in
    let%bind result1 = Type.of_msgpack api_result1.witness result1 in
    let%bind result2 = Type.of_msgpack api_result2.witness result2 in
    Ok (result1, result2)
  | Ok results ->
    let message =
      let num_results = List.length results in
      [%string "Unexpected number of results - expected 2 but got %{num_results#Int}"]
    in
    Or_error.error_s [%message message (results : Msgpack.t list)]
;;

module Statusline = struct
  type t =
    { text : string
    ; display_width : int
    ; highlights : Highlighted_text.t option
    }
  [@@deriving sexp_of]

  module Kind = struct
    type t =
      | Statusline
      | Tabline
      | Winbar
      | Statuscol of { one_indexed_row : int }
  end

  let eval
    ~(here : [%call_pos])
    client
    ?window
    ?max_width
    ?fill_char
    ~include_highlights
    kind
    str
    =
    let opts =
      let kind =
        match (kind : Kind.t) with
        | Statusline -> None
        | Tabline -> Some ("use_tabline", Msgpack.Bool true)
        | Winbar -> Some ("use_winbar", Bool true)
        | Statuscol { one_indexed_row } -> Some ("use_statuscol_lnum", Int one_indexed_row)
      in
      [ Option.map window ~f:(fun window -> "winid", Type.to_msgpack Window window)
      ; Option.map max_width ~f:(fun max_width -> "maxwidth", Msgpack.Int max_width)
      ; Option.map fill_char ~f:(fun fill_char ->
          "fillchar", Msgpack.String (Char.to_string fill_char))
      ; Some ("highlights", Bool include_highlights)
      ; kind
      ]
      |> List.filter_opt
      |> String.Map.of_alist_exn
    in
    Nvim_internal.nvim_eval_statusline ~str ~opts
    |> map_witness ~f:(fun map ->
      let open Or_error.Let_syntax in
      let%bind text = find_or_error_and_convert map "str" (Type.of_msgpack String) in
      let%bind display_width =
        find_or_error_and_convert map "width" (Type.of_msgpack Int)
      in
      let%bind highlights =
        match%bind find_and_convert map "highlights" (Type.of_msgpack (Array Dict)) with
        | None -> Ok None
        | Some highlights ->
          let%bind highlights =
            highlights
            |> List.map ~f:(fun map ->
              let%bind start =
                find_or_error_and_convert map "start" (Type.of_msgpack Int)
              in
              let%bind group =
                find_or_error_and_convert map "group" (Type.of_msgpack String)
              in
              Ok (start, group))
            |> Or_error.combine_errors
          in
          (match highlights with
           | [] ->
             (* Confirmed in testing that the array should always be non-empty, even when
                the statusline string is empty. *)
             Or_error.error_s [%message "Empty highlights list"]
           | initial_highlight :: highlights ->
             let (last_highlight_start, last_highlight_group), chunks =
               List.fold
                 ~init:(initial_highlight, Reversed_list.[])
                 highlights
                 ~f:(fun ((start, group), chunks) ((end_, _) as next_highlight) ->
                   let chunk =
                     { Highlighted_text.Chunk.text = String.slice text start end_
                     ; hl_group = Some group
                     }
                   in
                   next_highlight, chunk :: chunks)
             in
             let highlighted_text =
               { Highlighted_text.Chunk.text =
                   String.slice text last_highlight_start (String.length text)
               ; hl_group = Some last_highlight_group
               }
               :: chunks
               |> Reversed_list.rev
             in
             Ok (Some highlighted_text))
      in
      return { text; display_width; highlights })
    |> run ~here client
  ;;
end
(* Prevent accidental export as [module Statusline = Statusline]. *)
[@@alert
  vcaml_do_not_export "This module is internal to VCaml and should not be exported."]

module Option_helpers = struct
  module String_list = struct
    type t = string list

    let of_msgpack : Msgpack.t -> t Or_error.t = function
      | String s -> Ok (String.split s ~on:',')
      | msgpack -> Or_error.error_s [%message "String expected" (msgpack : Msgpack.t)]
    ;;

    let to_msgpack t = Msgpack.String (String.concat t ~sep:",")
  end

  module Char_list = struct
    type t = char list

    let of_msgpack : Msgpack.t -> t Or_error.t = function
      | String s -> Ok (String.to_list s)
      | msgpack -> Or_error.error_s [%message "String expected" (msgpack : Msgpack.t)]
    ;;

    let to_msgpack t = Msgpack.String (String.of_list t)

    module Comma_separated = struct
      type nonrec t = t

      let of_msgpack : Msgpack.t -> t Or_error.t = function
        | String flags ->
          String.split flags ~on:','
          |> List.map ~f:(fun flag ->
            match String.length flag with
            | 1 -> Ok flag.[0]
            | _ ->
              Or_error.error_s [%message "Character flag expected" flag (flags : string)])
          |> Or_error.combine_errors
        | msgpack -> Or_error.error_s [%message "String expected" (msgpack : Msgpack.t)]
      ;;

      let to_msgpack t = Msgpack.String (t |> List.intersperse ~sep:',' |> String.of_list)
    end
  end
end
