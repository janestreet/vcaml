open Async
open Import

include module type of struct
  include Nvim_internal.Window
end

(** Get the buffer currently in the window. *)
val get_buf
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> Nvim_internal.Buffer.t Deferred.Or_error.t

(** Set the buffer for the window without triggering autocommands that relate to cursor
    movements, such as [WinEnter] or [BufEnter]. Note that there is an outstanding issue
    where autocommands that should still be triggered are not being triggered. For
    details, see https://github.com/neovim/neovim/issues/10070. *)
val set_buf
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> buffer:Nvim_internal.Buffer.t
  -> unit Deferred.Or_error.t

module When_this_is_the_buffer's_last_window : sig
  (** When [Unload { if_modified = `Abort_if_hiding_is_disabled }] is specified, [close]
      will fail when all of the following hold:

      1. The buffer is modified.
      2. 'hidden' is off (it is on by default in Neovim).
      3. 'bufhidden' is empty for the buffer.
      4. 'confirm' is off (otherwise a dialog will appear asking the user to confirm
         whether to save before closing with yes/no/cancel options). *)
  type t =
    | Hide
    | Unload of { if_modified : [ `Hide | `Abort_if_hiding_is_disabled ] }
end

(** Close the given window. If this is not the current window, no WinLeave event will be
    triggered because closing it does not require changing windows. *)
val close
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> when_this_is_the_buffer's_last_window:When_this_is_the_buffer's_last_window.t
  -> unit Deferred.Or_error.t

val exists : here:[%call_pos] -> _ Client.t -> t -> bool Deferred.Or_error.t
val get_height : here:[%call_pos] -> _ Client.t -> Or_current.t -> int Deferred.Or_error.t

val set_height
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> height:int
  -> unit Deferred.Or_error.t

val get_width : here:[%call_pos] -> _ Client.t -> Or_current.t -> int Deferred.Or_error.t

val set_width
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> width:int
  -> unit Deferred.Or_error.t

(** Get the cursor position for the given window. Every window in Neovim has a cursor
    position - for inactive windows, this is the position the cursor will assume when the
    window becomes active. *)
val get_cursor
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> Position.One_indexed_row.t Deferred.Or_error.t

(** Set the cursor position for the given window. This will not move the cursor to the
    window, but the window will still scroll to ensure the cursor position is visible. *)
val set_cursor
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> Position.One_indexed_row.t
  -> unit Deferred.Or_error.t

(** Get a window variable (see `:h w:`). *)
val get_var
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> string
  -> type_:'a Type.t
  -> 'a Deferred.Or_error.t

(** Set a window variable (see `:h w:`). Before using this, note that users have the
    freedom to change the values of these variables. If that would be undesirable, keep
    your state management inside your plugin. *)
val set_var
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> string
  -> type_:'a Type.t
  -> value:'a
  -> unit Deferred.Or_error.t

(** Delete a window variable (see `:h w:`). *)
val delete_var
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> string
  -> unit Deferred.Or_error.t

val get_tab
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> Nvim_internal.Tabpage.t Deferred.Or_error.t

(** The window number is a relative identifier of the window among the other windows in
    the tabpage. This is different from the window ID (represented by [t]), which is
    unique per window and is never reused. *)
val get_number : here:[%call_pos] -> _ Client.t -> Or_current.t -> int Deferred.Or_error.t

(** Get the display position of the window. *)
val get_position
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> Position.t Deferred.Or_error.t

(** This module represents the configuration of an external or floating window. An
    external window exists outside Neovim; a floating window is drawn in Neovim on top of
    normal windows. *)
module Config : sig
  module Border : sig
    type t =
      | Single_line
      | Double_line
      | Single_line_rounded_corners
      | Whitespace
      | Shadow
      | Custom of Highlighted_text.t
  end

  module Title : sig
    type pos =
      | Left
      | Center
      | Right

    type t =
      { pos : pos
      ; text : Highlighted_text.t
      }
  end

  module Floating : sig
    module Corner : sig
      type t =
        | Top_left
        | Top_right
        | Bottom_left
        | Bottom_right

      module Position : sig
        type nonrec t =
          | Relative_to_editor of { pos : Position.t }
          | Relative_to_window of
              { window : Or_current.t
              ; pos : Position.t
              }
          | Relative_to_text_in_window of
              { window : Or_current.t
              ; text_pos : Position.t
              ; pos : Position.t option (** This position is relative to [text_pos]. *)
              }
          | Relative_to_cursor_in_current_window of { pos : Position.t }
          | Relative_to_mouse of { pos : Position.t }
          (** Note that mouse updates will be infrequent unless the 'mousemoveevent'
              option is set. This option has overhead, and when it is enabled mouse
              movement can abort pending mappings. *)
      end
    end

    type t =
      { width : int
      ; height : int
      ; corner : Corner.t
      ; corner_pos : Corner.Position.t
      ; zindex : int option
      ; focusable : bool
      ; border : Border.t option
      ; title : Title.t option
      }
    [@@deriving sexp_of]
  end

  module External : sig
    type t =
      { width : int
      ; height : int
      ; focusable : bool
      ; border : Border.t option
      ; title : Title.t option
      }
    [@@deriving sexp_of]
  end

  type t =
    | Floating of Floating.t
    | External of External.t
  [@@deriving sexp_of]
end

(** Open a floating window. [enter] specifies whether the cursor should be placed in the
    window after opening it. Most use cases will want to set [minimal_style = true]. Note
    that if the given [buffer] is not loaded, opening it in the floating window may not
    trigger certain autocommands you may expect to trigger on file load, e.g., FileType
    autocommands. For details, see
    https://github.com/neovim/neovim/issues/10070#issuecomment-537854497. *)
val open_floating
  :  here:[%call_pos]
  -> _ Client.t
  -> ?noautocmd:bool
  -> unit
  -> buffer:Nvim_internal.Buffer.Or_current.t
  -> enter:bool
  -> config:Config.Floating.t
  -> minimal_style:bool
  -> t Deferred.Or_error.t

(** Open an external window (must be supported by the attached UIs). *)
val open_external
  :  here:[%call_pos]
  -> _ Client.t
  -> ?noautocmd:bool
  -> unit
  -> buffer:Nvim_internal.Buffer.Or_current.t
  -> enter:bool
  -> config:Config.External.t
  -> minimal_style:bool
  -> t Deferred.Or_error.t

(** Returns [Some config] for floating and external windows and returns [None] for normal
    windows. *)
val get_config
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> Config.t option Deferred.Or_error.t

(** This API obscures a limitation inherent in [nvim_win_set_config]'s interface: because
    absent config keys will not be changed and there is no support for removing keys, if
    [Config.Floating.corner_pos = Relative_to_text_in_window { ... }], the type of corner
    positioning cannot be changed because it sets the "bufpos" key, which no other
    positioning schemes override. *)
val set_config
  :  here:[%call_pos]
  -> _ Client.t
  -> Or_current.t
  -> Config.t
  -> unit Deferred.Or_error.t

module Statusline : sig
  type t =
    { text : string
    ; display_width : int
    ; highlights : Highlighted_text.t option
    }
  [@@deriving sexp_of]
end

module Winbar : sig
  type t =
    { text : string
    ; display_width : int
    ; highlights : Highlighted_text.t option
    }
  [@@deriving sexp_of]
end

module Statuscolumn : sig
  type t =
    { text : string
    ; display_width : int
    ; highlights : Highlighted_text.t option
    }
  [@@deriving sexp_of]
end

(** These API functions are served immediately without waiting in the input queue. *)
module Fast : sig
  (** Evaluates a stutusline-formatted string (see `:h statusline`). To evaluate a string
      for the tabline, see [Nvim.eval_tabline]. *)
  val eval_statusline
    :  here:[%call_pos]
    -> _ Client.t
    -> Or_current.t
    -> ?max_width:int
    -> ?fill_char:char
    -> include_highlights:bool
    -> string
    -> Statusline.t Deferred.Or_error.t

  (** Evaluates a statuscolumn-formatted string (see `:h statuscolumn). *)
  val eval_statuscolumn
    :  here:[%call_pos]
    -> _ Client.t
    -> Or_current.t
    -> ?max_width:int
    -> ?fill_char:char
    -> include_highlights:bool
    -> one_indexed_row:int
    -> string
    -> Statuscolumn.t Deferred.Or_error.t

  (** Evaluates a winbar-formatted string (see `:h winbar). To evaluate a string for the
      tabline, see [Nvim.eval_tabline]. *)
  val eval_winbar
    :  here:[%call_pos]
    -> _ Client.t
    -> Or_current.t
    -> ?max_width:int
    -> ?fill_char:char
    -> include_highlights:bool
    -> string
    -> Winbar.t Deferred.Or_error.t
end

module Option : sig
  (*$ Vcaml_cinaps.generate_options_intf ~scope:Window *)
  type 'a t =
    | Previewwindow : bool t
    | Scroll : int t
    | Winfixheight : bool t
    | Winfixwidth : bool t
  [@@deriving sexp_of]
  (*$*)

  (** Get the effective value of the window option for the current window. *)
  val get
    :  here:[%call_pos]
    -> _ Client.t
    -> Or_current.t
    -> 'a t
    -> 'a Deferred.Or_error.t

  val set
    :  here:[%call_pos]
    -> _ Client.t
    -> Or_current.t
    -> 'a t
    -> 'a
    -> unit Deferred.Or_error.t

  val get_dynamic_info
    :  here:[%call_pos]
    -> _ Client.t
    -> 'a t
    -> 'a Dynamic_option_info.t Deferred.Or_error.t

  module Per_buffer : sig
    (*$ Vcaml_cinaps.generate_options_intf ~scope:Window_per_buffer *)
    (** Buffer-specific Neovim window options. These operate like buffer options, but the
        "global scope" is per-window. The ['global] phantom type represents the notion of
        a "global" value for the option. [`global] means there is a global value for each
        window that can be locally overridden for any given buffer in the window.
        [`copied] means the (per-window) global value is copied to the (per-buffer,window)
        local value when a new buffer is opened in the window. The details of when and how
        these settings are copied are somewhat arcane, so it's best not to rely on this
        behavior. *)
    type ('a, 'global) t =
      | Arabic : (bool, [ `copied ]) t
      | Breakindent : (bool, [ `copied ]) t
      | Breakindentopt : (string list, [ `copied ]) t
      | Colorcolumn : (string list, [ `copied ]) t
      | Concealcursor : (string, [ `copied ]) t
      | Conceallevel : (int, [ `copied ]) t
      | Cursorbind : (bool, [ `copied ]) t
      | Cursorcolumn : (bool, [ `copied ]) t
      | Cursorline : (bool, [ `copied ]) t
      | Cursorlineopt : (string list, [ `copied ]) t
      | Diff : (bool, [ `copied ]) t
      | Fillchars : (string list, [ `global ]) t
      | Foldcolumn : (string, [ `copied ]) t
      | Foldenable : (bool, [ `copied ]) t
      | Foldexpr : (string, [ `copied ]) t
      | Foldignore : (string, [ `copied ]) t
      | Foldlevel : (int, [ `copied ]) t
      | Foldmarker : (string list, [ `copied ]) t
      | Foldmethod : (string, [ `copied ]) t
      | Foldminlines : (int, [ `copied ]) t
      | Foldnestmax : (int, [ `copied ]) t
      | Foldtext : (string, [ `copied ]) t
      | Linebreak : (bool, [ `copied ]) t
      | List : (bool, [ `copied ]) t
      | Listchars : (string list, [ `global ]) t
      | Number : (bool, [ `copied ]) t
      | Numberwidth : (int, [ `copied ]) t
      | Relativenumber : (bool, [ `copied ]) t
      | Rightleft : (bool, [ `copied ]) t
      | Rightleftcmd : (string, [ `copied ]) t
      | Scrollbind : (bool, [ `copied ]) t
      | Scrolloff : (int, [ `global ]) t
      | Showbreak : (string, [ `global ]) t
      | Sidescrolloff : (int, [ `global ]) t
      | Signcolumn : (string, [ `copied ]) t
      | Spell : (bool, [ `copied ]) t
      | Statuscolumn : (string, [ `copied ]) t
      | Statusline : (string, [ `global ]) t
      | Virtualedit : (string list, [ `global ]) t
      | Winbar : (string, [ `global ]) t
      | Winblend : (int, [ `copied ]) t
      | Winhighlight : (string list, [ `copied ]) t
      | Wrap : (bool, [ `copied ]) t
    [@@deriving sexp_of]
    (*$*)

    val get_dynamic_info
      :  here:[%call_pos]
      -> _ Client.t
      -> ('a, _) t
      -> 'a Dynamic_option_info.t Deferred.Or_error.t
  end

  (** Get the effective value of the window option for the current buffer in the window. *)
  val get_for_current_buffer_in_window
    :  here:[%call_pos]
    -> _ Client.t
    -> Or_current.t
    -> ('a, _) Per_buffer.t
    -> 'a Deferred.Or_error.t

  (** Set the option for the current buffer in the window. *)
  val set_for_current_buffer_in_window
    :  here:[%call_pos]
    -> _ Client.t
    -> Or_current.t
    -> ('a, _) Per_buffer.t
    -> 'a
    -> unit Deferred.Or_error.t

  (** Get the global value of the option used by all buffers/windows without local
      overrides. *)
  val get_default
    :  here:[%call_pos]
    -> _ Client.t
    -> ('a, [ `global ]) Per_buffer.t
    -> 'a Deferred.Or_error.t

  (** Set the option for all buffers/windows without local overrides. *)
  val set_default
    :  here:[%call_pos]
    -> _ Client.t
    -> ('a, [ `global ]) Per_buffer.t
    -> 'a
    -> unit Deferred.Or_error.t

  (** Get the value that buffers opened in the window will inherit for this option. The
      details of this mechanic are arcane and relying on this is likely a mistake. *)
  val get_for_new_buffers_opened_in_window
    :  here:[%call_pos]
    -> _ Client.t
    -> Or_current.t
    -> ('a, [ `copied ]) Per_buffer.t
    -> 'a Deferred.Or_error.t

  (** Set the option for buffers opened in the window in the future. The details of this
      mechanic are arcane and relying on this is likely a mistake. *)
  val set_for_new_buffers_opened_in_window
    :  here:[%call_pos]
    -> _ Client.t
    -> Or_current.t
    -> ('a, [ `copied ]) Per_buffer.t
    -> 'a
    -> unit Deferred.Or_error.t
end
