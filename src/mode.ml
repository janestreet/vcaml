open Core

module T = struct
  type t =
    | Command_line_editing
    | Confirm_dialog
    | External_command
    | Hit_enter_prompt
    | Insert
    | Insert_mode_completion
    | Insert_mode_x_completion
    | More_prompt
    | Normal
    | Normal_ex_mode
    | Operator_pending
    | Replace
    | Replace_mode_completion
    | Replace_mode_x_completion
    | Select_blockwise
    | Select_by_character
    | Select_by_line
    | Terminal
    | Vim_ex_mode
    | Virtual_replace
    | Visual_blockwise
    | Visual_by_character
    | Visual_by_line
  [@@deriving compare, enumerate, sexp_of]
end

include T
include Comparable.Make_plain (T)

let of_mode_symbol = function
  | "n" -> Ok Normal
  | "no" -> Ok Operator_pending
  | "v" -> Ok Visual_by_character
  | "V" -> Ok Visual_by_line
  | "\022" -> Ok Visual_blockwise
  | "s" -> Ok Select_by_character
  | "S" -> Ok Select_by_line
  | "\019" -> Ok Select_blockwise
  | "i" -> Ok Insert
  | "ic" -> Ok Insert_mode_completion
  | "ix" -> Ok Insert_mode_x_completion
  | "R" -> Ok Replace
  | "Rc" -> Ok Replace_mode_completion
  | "Rv" -> Ok Virtual_replace
  | "Rx" -> Ok Replace_mode_x_completion
  | "c" -> Ok Command_line_editing
  | "cv" -> Ok Vim_ex_mode
  | "ce" -> Ok Normal_ex_mode
  | "r" -> Ok Hit_enter_prompt
  | "rm" -> Ok More_prompt
  | "r?" -> Ok Confirm_dialog
  | "!" -> Ok External_command
  | "t" -> Ok Terminal
  | sym -> Or_error.error_s [%message "Unrecognized mode symbol" (sym : string)]
;;

module With_blocking_info = struct
  type nonrec t =
    { mode : t
    ; blocking : bool
    }
  [@@deriving sexp_of]
end
