open Core

(** See `:h mode()` for descriptions of these modes. *)
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
  | Normal_using_i_ctrl_o_in_insert_mode
  | Normal_using_i_ctrl_o_in_replace_mode
  | Normal_using_i_ctrl_o_in_virtual_replace_mode
  | Normal_ex_mode
  | Operator_pending
  | Operator_pending_forced_blockwise
  | Operator_pending_forced_characterwise
  | Operator_pending_forced_linewise
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
[@@deriving enumerate, sexp_of]

include Comparable.S_plain with type t := t

val of_mode_symbol : string -> t Or_error.t

module With_blocking_info : sig
  type nonrec t =
    { mode : t
    ; blocking : bool
    }
  [@@deriving sexp_of]
end
