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
    | Normal_in_terminal_emulator
    | Normal_using_i_ctrl_o_in_insert_mode
    | Normal_using_i_ctrl_o_in_replace_mode
    | Normal_using_i_ctrl_o_in_virtual_replace_mode
    | Normal_using_t_ctrl_slash_ctrl_o_in_terminal_mode
    | Operator_pending
    | Operator_pending_forced_blockwise
    | Operator_pending_forced_characterwise
    | Operator_pending_forced_linewise
    | Replace
    | Replace_mode_completion
    | Replace_mode_i_ctrl_x_completion
    | Select_blockwise
    | Select_by_character
    | Select_by_line
    | Terminal
    | Vim_ex_mode
    | Virtual_replace
    | Virtual_replace_mode_completion
    | Virtual_replace_mode_i_ctrl_x_completion
    | Visual_blockwise
    | Visual_blockwise_using_v_ctrl_o_in_select_mode
    | Visual_by_character
    | Visual_by_character_using_v_ctrl_o_in_select_mode
    | Visual_by_line
    | Visual_by_line_using_v_ctrl_o_in_select_mode
  [@@deriving compare, enumerate, sexp_of]
end

include T
include Comparable.Make_plain (T)

let of_mode_symbol = function
  | "n" -> Ok Normal
  | "no" -> Ok Operator_pending
  | "nov" -> Ok Operator_pending_forced_characterwise
  | "noV" -> Ok Operator_pending_forced_linewise
  | "no\022" -> Ok Operator_pending_forced_blockwise
  | "niI" -> Ok Normal_using_i_ctrl_o_in_insert_mode
  | "niR" -> Ok Normal_using_i_ctrl_o_in_replace_mode
  | "niV" -> Ok Normal_using_i_ctrl_o_in_virtual_replace_mode
  | "ntT" -> Ok Normal_using_t_ctrl_slash_ctrl_o_in_terminal_mode
  | "nt" -> Ok Normal_in_terminal_emulator
  | "v" -> Ok Visual_by_character
  | "vs" -> Ok Visual_by_character_using_v_ctrl_o_in_select_mode
  | "V" -> Ok Visual_by_line
  | "Vs" -> Ok Visual_by_line_using_v_ctrl_o_in_select_mode
  | "\022" -> Ok Visual_blockwise
  | "\022s" -> Ok Visual_blockwise_using_v_ctrl_o_in_select_mode
  | "s" -> Ok Select_by_character
  | "S" -> Ok Select_by_line
  | "\019" -> Ok Select_blockwise
  | "i" -> Ok Insert
  | "ic" -> Ok Insert_mode_completion
  | "ix" -> Ok Insert_mode_x_completion
  | "R" -> Ok Replace
  | "Rc" -> Ok Replace_mode_completion
  | "Rx" -> Ok Replace_mode_i_ctrl_x_completion
  | "Rv" -> Ok Virtual_replace
  | "Rvc" -> Ok Virtual_replace_mode_completion
  | "Rvx" -> Ok Virtual_replace_mode_i_ctrl_x_completion
  | "c" -> Ok Command_line_editing
  | "cv" -> Ok Vim_ex_mode
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
