module Unshadow_buffer := Buffer
open Core
open Async
open Import
module Buffer := Unshadow_buffer

(** Neovim API functions that are not bound to a particular Neovim object. *)

val get_current_buf : here:[%call_pos] -> _ Client.t -> Buffer.t Deferred.Or_error.t

val set_current_buf
  :  here:[%call_pos]
  -> _ Client.t
  -> Buffer.t
  -> unit Deferred.Or_error.t

val get_current_win : here:[%call_pos] -> _ Client.t -> Window.t Deferred.Or_error.t

val set_current_win
  :  here:[%call_pos]
  -> _ Client.t
  -> Window.t
  -> unit Deferred.Or_error.t

val get_current_tab : here:[%call_pos] -> _ Client.t -> Tabpage.t Deferred.Or_error.t

val set_current_tab
  :  here:[%call_pos]
  -> _ Client.t
  -> Tabpage.t
  -> unit Deferred.Or_error.t

(** List all valid buffers, irrespective of whether they are 'buflisted' (and irrespective
    of whether they are loaded). *)
val list_bufs : here:[%call_pos] -> _ Client.t -> Buffer.t list Deferred.Or_error.t

val list_wins : here:[%call_pos] -> _ Client.t -> Window.t list Deferred.Or_error.t
val list_tabs : here:[%call_pos] -> _ Client.t -> Tabpage.t list Deferred.Or_error.t

(** Get a global variable (see `:h g:`). *)
val get_var
  :  here:[%call_pos]
  -> _ Client.t
  -> string
  -> type_:'a Type.t
  -> 'a Deferred.Or_error.t

(** Set a global variable (see `:h g:`). Before using this, note that users have the
    freedom to change the values of these variables. If that would be undesirable, keep
    your state management inside your plugin. *)
val set_var
  :  here:[%call_pos]
  -> _ Client.t
  -> string
  -> type_:'a Type.t
  -> value:'a
  -> unit Deferred.Or_error.t

(** Delete a global variable (see `:h g:`). *)
val delete_var : here:[%call_pos] -> _ Client.t -> string -> unit Deferred.Or_error.t

(** vvar refers to a special vim variable (see `:h vim-variable`). *)
val get_vvar
  :  here:[%call_pos]
  -> _ Client.t
  -> string
  -> type_:'a Type.t
  -> 'a Deferred.Or_error.t

(** Set a special vim variable (see `:h vim-variable`). Not all variables can be set by
    the user - see documentation of each variable to see if [set_vvar] is appropriate. *)
val set_vvar
  :  here:[%call_pos]
  -> _ Client.t
  -> string
  -> type_:'a Type.t
  -> value:'a
  -> unit Deferred.Or_error.t

(** Evaluate a single VimL (Vimscript) expression. To just call a function, use
    [call_function] instead. *)
val eval_viml_expression
  :  here:[%call_pos]
  -> _ Client.t
  -> string
  -> result_type:'a Type.t
  -> 'a Deferred.Or_error.t

module Func : sig
  (** [t] represents the type of a VimL (Vimscript) or Lua function. When constructing a
      [t] bear the following in mind:

      1. If you are modeling a function that takes no arguments, just use [return T]. Do
         not use [Nil @-> return T].

      2. If you are modeling a native (non-API) VimL function that does not have an
         explicit return statement, its implicit return is [Integer 0], not [Nil].

      3. VimL and Lua functions may return values of different types. When modeling a
         function that does this you will need to use [return Object] and parse the
         [Msgpack.t] yourself. *)
  type 'fn t

  val return : 'a Type.t -> 'a Deferred.Or_error.t t
  val ( @-> ) : 'a Type.t -> 'b t -> ('a -> 'b) t
end

(** Given the name and type of a VimL or Lua function, return a type-checked function for
    calling it as though it were part of the API. If you are using this to call a native
    Neovim function, first look at the available API functions to see if any meet your
    needs.

    Example:
    {[
      let sqrt =
        call_function ~name:(`Viml "sqrt") ~type_:Nvim.Func.(Float @-> return Float)
      in
      sqrt [%here] client 0.5
    ]} *)
val call_function
  :  here:[%call_pos]
  -> _ Client.t
  -> name:[ `Viml of string | `Lua of string ]
  -> type_:'fn Func.t
  -> 'fn

(** Run a series of Vim commands, like [:source]-ing an anonymous VimL file. *)
val exec_viml : here:[%call_pos] -> _ Client.t -> string -> unit Deferred.Or_error.t

(** When capturing output, the `:silent` modifier is implicitly in effect. To force
    messages to appear on the screen during execution, use `:unsilent`. See `:h :silent`
    and `:h :unsilent` for more information. *)
val exec_viml_and_capture_output
  :  here:[%call_pos]
  -> _ Client.t
  -> string
  -> string Deferred.Or_error.t

(** Run a series of Lua commands, like [:source]-ing an anonymous Lua file. *)
val exec_lua : here:[%call_pos] -> _ Client.t -> string -> unit Deferred.Or_error.t

val out_write : here:[%call_pos] -> _ Client.t -> string -> unit Deferred.Or_error.t
val out_writeln : here:[%call_pos] -> _ Client.t -> string -> unit Deferred.Or_error.t
val err_write : here:[%call_pos] -> _ Client.t -> string -> unit Deferred.Or_error.t
val err_writeln : here:[%call_pos] -> _ Client.t -> string -> unit Deferred.Or_error.t

val echo
  :  here:[%call_pos]
  -> _ Client.t
  -> Highlighted_text.t
  -> add_to_history:bool
  -> unit Deferred.Or_error.t

(** As of this writing messages echoed during an [rpcrequest] are not displayed until the
    request completes. This function hacks around that limitation. For more details about
    this echoing limitation, see https://github.com/neovim/neovim/issues/14449. *)
val echo_in_rpcrequest
  :  here:[%call_pos]
  -> _ Client.t
  -> string
  -> unit Deferred.Or_error.t

module Log_level : sig
  type t =
    | Trace
    | Debug
    | Info
    | Warn
    | Error
  [@@deriving compare, enumerate, sexp_of]
end

(** Send a message to the notification handler. By default, the message is written to the
    screen and saved in message history (same as [out_writeln]), but the behavior can be
    customized by setting [vim.notify]. Customizing the behavior of [vim.notify] should be
    left to the user or to plugins that are specifically intending to globally affect how
    notifications are handled.

    This function should not be confused with [Notifier.notify], which is used to send RPC
    notifications to Neovim. *)
val notify
  :  here:[%call_pos]
  -> _ Client.t
  -> Log_level.t
  -> string
  -> unit Deferred.Or_error.t

type keys_with_replaced_keycodes = private string

val replace_termcodes_only
  :  here:[%call_pos]
  -> _ Client.t
  -> string
  -> string Deferred.Or_error.t

val replace_termcodes_and_keycodes
  :  here:[%call_pos]
  -> _ Client.t
  -> string
  -> keys_with_replaced_keycodes Deferred.Or_error.t

(** Send keystrokes to Neovim (see `:h feedkeys`) *)
val feedkeys
  :  here:[%call_pos]
  -> _ Client.t
  -> [ `Raw of string
       (** This string is not escaped. To escape key codes, first call
           [replace_termcodes_and_keycodes], then pass the result with [`Keycodes]. *)
     | `Keycodes of keys_with_replaced_keycodes
     ]
  -> mode:string
  -> unit Deferred.Or_error.t

val get_color_map
  :  here:[%call_pos]
  -> _ Client.t
  -> Color.True_color.t String.Map.t Deferred.Or_error.t

val get_color_by_name
  :  here:[%call_pos]
  -> _ Client.t
  -> string
  -> Color.True_color.t Deferred.Or_error.t

val get_hl_by_name
  :  here:[%call_pos]
  -> _ Client.t
  -> ?ns_id:Color.Namespace.t
  -> string
  -> color_depth:'a Color.Depth.t
  -> 'a Color.Highlight.t Deferred.Or_error.t

module Highlight_id : sig
  type t [@@deriving compare, sexp_of]
end

val get_hl_id_by_name
  :  here:[%call_pos]
  -> _ Client.t
  -> string
  -> Highlight_id.t Deferred.Or_error.t

val get_hl_by_id
  :  here:[%call_pos]
  -> _ Client.t
  -> ?ns_id:Color.Namespace.t
  -> Highlight_id.t
  -> color_depth:'a Color.Depth.t
  -> 'a Color.Highlight.t Deferred.Or_error.t

val list_runtime_paths : here:[%call_pos] -> _ Client.t -> string list Deferred.Or_error.t

module Mouse : sig
  module Button : sig
    type t =
      | Left
      | Right
      | Middle
    [@@deriving sexp_of]
  end

  module Action : sig
    type t =
      | Press of Button.t
      | Drag of Button.t
      | Release of Button.t
      | Move (** Unlike the API, we model mouse movement as an action, not a button. *)
      | Wheel_up
      | Wheel_down
      | Wheel_left
      | Wheel_right
    [@@deriving sexp_of]
  end
end

module Key_modifier : sig
  type t =
    | Shift
    | Ctrl
    | Alt
    | Super
  [@@deriving compare, sexp_of]

  include Comparable.S_plain with type t := t
end

module Tabline : sig
  type t =
    { text : string
    ; display_width : int
    ; highlights : Highlighted_text.t option
    }
  [@@deriving sexp_of]
end

(** These API functions are served immediately without waiting in the input queue. *)
module Fast : sig
  (** There is a Neovim bug where calling [get_mode] during an [rpcrequest] will hang. See
      https://github.com/neovim/neovim/issues/14451. *)
  val get_mode
    :  here:[%call_pos]
    -> _ Client.t
    -> Mode.With_blocking_info.t Deferred.Or_error.t

  (** Queue raw user input. *)
  val input : here:[%call_pos] -> _ Client.t -> string -> int Deferred.Or_error.t

  (** Send a mouse event from the GUI. *)
  val input_mouse
    :  here:[%call_pos]
    -> _ Client.t
    -> ?modifiers:Key_modifier.Set.t
    -> ?grid:int
    -> Mouse.Action.t
    -> row:int
    -> col:int
    -> unit Deferred.Or_error.t

  (** Evaluates a tabline-formatted string (see `:h setting-tabline`). To evaluate a
      string for a window's statusline, see [Window.eval_statusline]. *)
  val eval_tabline
    :  here:[%call_pos]
    -> _ Client.t
    -> ?max_width:int
    -> ?fill_char:char
    -> include_highlights:bool
    -> string
    -> Tabline.t Deferred.Or_error.t

  val find_runtime_file_matching
    :  here:[%call_pos]
    -> _ Client.t
    -> pattern:string
    -> string option Deferred.Or_error.t

  val all_runtime_files_matching
    :  here:[%call_pos]
    -> _ Client.t
    -> pattern:string
    -> string list Deferred.Or_error.t
end

(** Paste text at the cursor. This is the "paste" in "copy-paste." *)
val paste : here:[%call_pos] -> _ Client.t -> string list -> unit Deferred.Or_error.t

(** Paste a stream of text at the cursor. Only one paste stream should be open for a
    client at a time. The returned deferred is filled after the writer is closed and the
    paste stream has flushed to Neovim. *)
val paste_stream
  :  here:[%call_pos]
  -> _ Client.t
  -> string Pipe.Writer.t * unit Deferred.Or_error.t

module Text_mode : sig
  type t =
    | Charwise
    | Linewise
    | Blockwise of { width : int }
end

(** Put text at the cursor. This is a more targeted operation than [paste], though if your
    edit is not best expressed relative to the cursor, [Buffer.set_lines] or
    [Buffer.set_text] will likely be better fits. *)
val put
  :  here:[%call_pos]
  -> _ Client.t
  -> ?text_mode:Text_mode.t
  -> string list
  -> where:[ `Before_cursor | `After_cursor ]
  -> place_cursor:[ `At_start_of_text | `At_end_of_text ]
  -> unit Deferred.Or_error.t

(** Get information about a given channel (see `:h channel`). *)
val get_channel_info
  :  here:[%call_pos]
  -> _ Client.t
  -> int
  -> Channel_info.t Deferred.Or_error.t

(** List all channels connected to Neovim (see `:h channel`). *)
val channels : here:[%call_pos] -> _ Client.t -> Channel_info.t list Deferred.Or_error.t

(** Set client information for your plugin. Some bits of information are already handled
    for you by VCaml, such as the client name and registered methods. *)
val set_client_info
  :  here:[%call_pos]
  -> _ Client.t
  -> ?version:Client_info.Version.t
  -> ?attributes:string String.Map.t
  -> ?client_type:Client_info.Client_type.t
  -> unit
  -> unit Deferred.Or_error.t

(** Send raw bytes to channel. If the channel is a terminal and you want to display
    multiple lines you will need to follow your newlines with carriage returns. See also
    [Buffer.Untested.open_term]. *)
val send_to_channel
  :  here:[%call_pos]
  -> _ Client.t
  -> channel:int
  -> string
  -> unit Deferred.Or_error.t

(** If you want to access lines other than the current line, use [Buffer.get_lines]. *)
val get_current_line : here:[%call_pos] -> _ Client.t -> String.Utf8.t Deferred.Or_error.t

(** If you want to modify lines other than the current line, use [Buffer.set_lines]. The
    character encoding should be UTF-8 (this function takes a [string] rather than a
    [String.Utf8.t] for ergonomic reasons; validity can be tested with
    [String.Utf8.is_valid]). *)
val set_current_line
  :  here:[%call_pos]
  -> _ Client.t
  -> string
  -> unit Deferred.Or_error.t

(** If you want to delete lines other than the current line, use [Buffer.set_lines]. *)
val delete_current_line : here:[%call_pos] -> _ Client.t -> unit Deferred.Or_error.t

module Context_type : sig
  type t =
    | Jumplist
    | Registers
    | Buffer_list
    | Global_variables
    | Script_local_functions
    | Global_and_script_local_functions
  [@@deriving compare, enumerate, sexp_of]

  include Comparable.S_plain with type t := t
end

(** Although the context for each key is made available as a [Msgpack.t] instead of being
    represented as an abstract type, inspecting the context is unlikely to be helpful. It
    should be treated as an opaque value for passing to [load_context]. *)
val get_context
  :  here:[%call_pos]
  -> _ Client.t
  -> Context_type.Set.t
  -> Msgpack.t Context_type.Map.t Deferred.Or_error.t

(** Load context that was retrieved by [get_context] to restore editor state. *)
val load_context
  :  here:[%call_pos]
  -> _ Client.t
  -> Msgpack.t Context_type.Map.t
  -> unit Deferred.Or_error.t

(** Get a native uppercase mark (see `:h mark-motions`). Use [Buffer.get_mark] to get a
    lowercase mark. *)
val get_mark
  :  here:[%call_pos]
  -> _ Client.t
  -> char
  -> (Buffer.t * Mark.t) option Deferred.Or_error.t

(** Delete a native uppercase mark (see `:h mark-motions`). *)
val delete_mark : here:[%call_pos] -> _ Client.t -> char -> unit Or_error.t Deferred.t

val get_display_width_of_text
  :  here:[%call_pos]
  -> _ Client.t
  -> string
  -> int Deferred.Or_error.t

(** This function has the same semantics as [:cd] in that it will clear the window-local
    and tab-local directories. Setting the working directory in Neovim is tricky, and this
    may not be what you want. See `:h current-directory`, `:h chdir()`, `:h getcwd()`, and
    `:h haslocaldir()`. *)
val set_current_dir : here:[%call_pos] -> _ Client.t -> string -> unit Deferred.Or_error.t

module Option : sig
  (*$ Vcaml_cinaps.generate_options_intf ~scope:Global *)
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
  (*$*)

  val get : here:[%call_pos] -> _ Client.t -> 'a t -> 'a Deferred.Or_error.t
  val set : here:[%call_pos] -> _ Client.t -> 'a t -> 'a -> unit Deferred.Or_error.t

  val get_dynamic_info
    :  here:[%call_pos]
    -> _ Client.t
    -> 'a t
    -> 'a Dynamic_option_info.t Deferred.Or_error.t
end
