open Core

module True_color = struct
  type t =
    { red : char
    ; green : char
    ; blue : char
    }

  let to_string { red; green; blue } =
    sprintf "#%02x%02x%02x" (Char.to_int red) (Char.to_int green) (Char.to_int blue)
  ;;

  let sexp_of_t t = Sexp.of_string (to_string t)

  let of_24bit_int x =
    match x land lnot 0xFFFFFF = 0 with
    | true ->
      let red = x lsr 16 |> Char.of_int_exn in
      let green = (x land 0xFFFF) lsr 8 |> Char.of_int_exn in
      let blue = x land 0xFF |> Char.of_int_exn in
      Ok { red; green; blue }
    | false ->
      Or_error.error_s
        [%message "Color.True_color.of_24bit_int: Expected 24-bit integer" (x : int)]
  ;;
end

module Color256 = struct
  type t = char

  let of_8bit_int x =
    match Char.of_int x with
    | Some x -> Ok x
    | None ->
      Or_error.error_s
        [%message "Color.Color256.of_8bit_int: Expected 8-bit integer" (x : int)]
  ;;

  let to_string t = t |> Char.to_int |> Int.to_string
  let sexp_of_t t = t |> Char.to_int |> Int.sexp_of_t
end

module Depth = struct
  type 'a t =
    | True_color : True_color.t t
    | Color256 : Color256.t t
  [@@deriving sexp_of]
end

module Highlight = struct
  type 'a t =
    { fg : 'a option
    ; bg : 'a option
    }
  [@@deriving sexp_of]
end

module Namespace = struct
  include Int

  let global = 0
end

type 'a t =
  | True_color : True_color.t -> True_color.t t
  | Color256 : Color256.t -> Color256.t t
[@@deriving sexp_of]

type packed = T : 'a t -> packed [@@unboxed]

let to_string (type a) (t : a t) =
  match t with
  | True_color color -> True_color.to_string color
  | Color256 color -> Color256.to_string color
;;

let sexp_of_t (type a) _sexp_of_a (t : a t) =
  match t with
  | True_color color -> True_color.sexp_of_t color
  | Color256 color -> Color256.sexp_of_t color
;;

let of_24bit_int x =
  let%map.Or_error color = True_color.of_24bit_int x in
  True_color color
;;

let of_8bit_int x =
  let%map.Or_error color = Color256.of_8bit_int x in
  Color256 color
;;
