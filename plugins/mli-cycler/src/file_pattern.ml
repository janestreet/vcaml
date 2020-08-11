open! Core
open! Async
module Re_jane = Re_jane.Easy_mode

type t =
  | Ml of string
  | Mli of string
  | Intf of string
  | Number of int * t
[@@deriving compare, equal]

let regex_endings =
  [ (Re_jane.create_exn "_intf\\.ml", fun x -> Intf x)
  ; (Re_jane.create_exn "\\.ml", fun x -> Ml x)
  ; (Re_jane.create_exn "\\.mli", fun x -> Mli x)
  ]
;;

let extract_before_first_group st ~regex =
  match Re_jane.split regex st with
  | [ x; "" ] -> Some x
  | _ -> None
;;

let get_root_with_digits_and_constructor ~filename =
  List.map regex_endings ~f:(fun (regex, constructor) ->
    let%map.Option root = extract_before_first_group filename ~regex in
    root, constructor)
  (* Find first Some *)
  |> List.fold ~init:None ~f:Option.first_some
;;

let split_root_and_digits st =
  let root = String.rstrip ~drop:Char.is_digit st in
  let digits = String.slice st (String.length root) (String.length st) in
  root, digits
;;

let of_filename filename =
  let%map.Option root_with_digits, constructor =
    get_root_with_digits_and_constructor ~filename
  in
  let root, digits = split_root_and_digits root_with_digits in
  match digits with
  | "" -> constructor root
  | x -> Number (Int.of_string x, constructor root)
;;

let rec get_root = function
  | Ml root -> root
  | Mli root -> root
  | Intf root -> root
  | Number (_, t) -> get_root t
;;

let get_middle = function
  | Ml _ | Mli _ | Intf _ -> ""
  | Number (x, _) -> Int.to_string x
;;

let rec get_ending = function
  | Ml _ -> ".ml"
  | Mli _ -> ".mli"
  | Intf _ -> "_intf.ml"
  | Number (_, t) -> get_ending t
;;

let to_filename t = get_root t ^ get_middle t ^ get_ending t
let to_short_filename t = Filename.basename (to_filename t)

let to_intf_name t =
  Printf.sprintf "%s%s_intf" (Filename.basename (get_root t)) (get_middle t)
;;

let dirname t = Filename.dirname (to_filename t)

let is_include_line ~intf_name line =
  let include_prefix = Printf.sprintf "include %s" (String.capitalize intf_name) in
  String.is_prefix line ~prefix:include_prefix
;;

let is_redundant_line_in_mli ~intf_name line =
  let open_prefix = "open" in
  let comment_prefix = "(*" in
  List.exists
    ~f:(fun prefix -> String.is_prefix line ~prefix)
    [ open_prefix; comment_prefix ]
  || is_include_line ~intf_name line
;;

let rec is_redundant_mli t =
  match t with
  | Ml _ | Intf _ -> return false
  | Number (_, t') -> is_redundant_mli t'
  | Mli _ as mli ->
    let intf_name = to_intf_name mli in
    let%map.Deferred lines = Reader.file_lines (to_filename mli) in
    List.for_all ~f:(is_redundant_line_in_mli ~intf_name) lines
    && List.exists ~f:(is_include_line ~intf_name) lines
;;

let list filename =
  match of_filename filename with
  | None -> return []
  | Some t ->
    let current_dir = dirname t in
    let%bind.Deferred file_list = Sys.ls_dir current_dir in
    List.map file_list ~f:(fun name -> of_filename (Filename.concat current_dir name))
    |> List.filter_opt
    |> List.filter ~f:(fun t' -> String.equal (get_root t) (get_root t'))
    |> List.sort ~compare
    |> Deferred.List.filter ~f:(fun t -> is_redundant_mli t >>| not)
;;

let find_file_pattern_at_offset ~offset ~current_file_pattern ~file_patterns =
  let open Option.Let_syntax in
  let%bind current_file_pattern = current_file_pattern in
  let%bind ind, (_ : t) =
    List.findi
      ~f:(fun (_ : int) file_pattern -> equal file_pattern current_file_pattern)
      file_patterns
  in
  List.nth file_patterns ((ind + offset) % List.length file_patterns)
;;

let find_next_file_pattern_in_list = find_file_pattern_at_offset ~offset:1
let find_prev_file_pattern_in_list = find_file_pattern_at_offset ~offset:(-1)

let get_file_pattern_with_fn ~current_file_pattern ~file_patterns ~file_pattern_fn =
  [ file_pattern_fn ~current_file_pattern ~file_patterns; List.nth file_patterns 0 ]
  |> List.fold ~init:None ~f:Option.first_some
;;

let next ~current_file_pattern ~file_patterns =
  get_file_pattern_with_fn
    ~current_file_pattern
    ~file_patterns
    ~file_pattern_fn:find_next_file_pattern_in_list
;;

let prev ~current_file_pattern ~file_patterns =
  get_file_pattern_with_fn
    ~current_file_pattern
    ~file_patterns
    ~file_pattern_fn:find_prev_file_pattern_in_list
;;
