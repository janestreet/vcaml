open Core
open Async

(** This module models the construction and execution of Neovim commands. All of this is
    described in `:h user-commands`, but names have been changed to make the model more
    intuitive. *)

module Number_of_arguments : sig
  (** Number of arguments that can be passed to a command. See `:h command-nargs`. *)
  type t =
    | Zero
    | One
    | Any
    | At_most_one
    | At_least_one
  [@@deriving sexp_of]
end

module Completion : sig
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
end

module Modifiers : sig
  module Filter : sig
    type t =
      | Only of string
      | Excluding of string
    [@@deriving sexp_of]
  end

  module Silent : sig
    type t =
      | No
      | Yes of { silence_errors : bool }
    [@@deriving sexp_of]
  end

  module Split : sig
    type t =
      | Above_left
      | Below_right
      | Top_left
      | Bottom_right
    [@@deriving sexp_of]
  end

  (** Modifiers applied to a command invocation. See `:h command-modifiers`. *)
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

(** At least as of 0.9.1, the documentation that describes how the range and count
    arguments can be specified for user-defined commands (`:h command-range`) is at best
    misleading. While this module attempts to model things reasonably, there are still
    underlying inconsistencies and undefined behaviors that make a definition of "correct"
    elusive. See https://github.com/neovim/neovim/issues/24081 for more information. When
    that issue gets resolved, we should reevaluate this model.

    This model takes the position that a command that accepts both a range and a count is
    ill-defined. To that end:

    1. A command cannot be [create]d that takes both a range and a count.

    2. A range and a count cannot be passed simultaneously when [exec]ing a command.

    3. Since there is no way to distinguish a command that just takes a count from a
       command that takes both a range and a count, we assume the command is well-defined
       and model it as a command that takes a count.

    However, it is important to note that ill-defined commands are not illegal - in fact,
    there are some built-in commands that are defined with [-range -count]. They are
    ill-defined in that it is not clear from the manual how they are meant to work
    generally and in that they have surprising edge cases that defy clean semantics. Also,
    ill-defined commands are not possible to create via the Neovim API - they can only be
    created via the native [:command] command. *)
module Range_or_count : sig
  module Of : sig
    (** The type of value to which the count/range refers. This affects the interpretation
        of special characters, such as ".", "$", and "%", in the range (or in the count
        when provided in the line number position). E.g., when [of_ = Windows], "$" will
        be the last window number (see `:h command-addr`). *)
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

  module Spec : sig
    module Default_range : sig
      type t =
        | Current
        | All
      [@@deriving sexp_of]
    end

    module How_count_can_be_passed : sig
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
  end

  type t =
    | Range of
        { start_inclusive : int
        ; end_inclusive : int
        } (** Ranges are 1-based, as they are when you pass them on the command line. *)
    | Count of int
  [@@deriving sexp_of]
end

(** Create a new user-defined command. *)
val create
  :  here:[%call_pos]
  -> _ Client.t
  -> ?keepscript:bool
  -> ?bang:bool
  -> ?bar:bool
  -> ?register:bool
  -> ?nargs:Number_of_arguments.t
  -> ?range_or_count:Range_or_count.Spec.t
  -> ?completion:Completion.t
  -> ?fail_if_exists:bool
  -> unit
  -> name:string
  -> scope:[ `Global | `Buffer_local of Nvim_internal.Buffer.Or_current.t ]
  -> unit Ocaml_from_nvim.Callback.t
     (** Anonymous RPCs do not have access to modifiers, arguments, or ranges provided to
         commands, so they are only suitable for implementing simple commands. *)
  -> unit Deferred.Or_error.t

(** Delete a user-defined command. *)
val delete
  :  here:[%call_pos]
  -> _ Client.t
  -> string
  -> scope:[ `Global | `Buffer_local of Nvim_internal.Buffer.Or_current.t ]
  -> unit Deferred.Or_error.t

(** Modifiers applied to a command invocation. See `:h command-modifiers`. *)
type 'a with_command_modifiers :=
  ?filter:Modifiers.Filter.t
  -> ?silent:Modifiers.Silent.t
  -> ?unsilent:bool
  -> ?sandbox:bool
  -> ?noautocmd:bool
  -> ?browse:bool
  -> ?confirm:bool
  -> ?hide:bool
  -> ?horizontal:bool
  -> ?vertical:bool
  -> ?split:Modifiers.Split.t
  -> ?keepalt:bool
  -> ?keepjumps:bool
  -> ?keepmarks:bool
  -> ?keeppatterns:bool
  -> ?lockmarks:bool
  -> ?noswapfile:bool
  -> ?tab:int
  -> ?verbose:int
  -> 'a

type ('a, 'output) exec :=
  here:[%call_pos]
  -> 'a Client.t
  -> ?range_or_count:Range_or_count.t
  -> ?bang:bool
  -> ?register:char
  -> ?args:string list
  -> ?expand_args:bool
       (** default: [true]. When [true], expand the arguments (see `:h expand`). *)
  -> (string -> 'output Deferred.Or_error.t) with_command_modifiers

(** Execute a native command. *)
val exec : (_, unit) exec

(** When capturing output, the [silent] modifier is implicitly in effect. To force
    messages to appear on the screen during execution, use [unsilent]. *)
val exec_and_capture_output : (_, string) exec

module Definition : sig
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
end

(** Get a mapping of user-defined commands. *)
val user_defined_commands
  :  here:[%call_pos]
  -> _ Client.t
  -> scope:[ `Global | `Buffer_local of Nvim_internal.Buffer.Or_current.t ]
  -> Definition.t Or_error.t String.Map.t Deferred.Or_error.t

module Parse_result : sig
  (** As above, there is no way to distinguish a command that just takes a count from a
      command that takes both a range and a count, so we assume the command is
      well-defined and model it as a command that takes a count. *)
  module Range_or_count : sig
    type t =
      | Range of
          { start_inclusive : int
          ; end_inclusive : int
          ; of_ : Range_or_count.Of.t
          } (** Ranges are 1-based, as they are when you pass them on the command line. *)
      | Count of
          { count : int
          ; of_ : Range_or_count.Of.t
          ; source : [ `Default_when_omitted | `Specified ]
          }
      | Ambiguous_count_or_singleton_range of
          { value : int
          ; of_ : Range_or_count.Of.t
          }
      (** For commands that are defined with [-range=N], the parse result only returns a
          range value, not a count. This makes it impossible to distinguish a singleton
          range for a command with the [-range]/[-range=%] attribute from a count for a
          command with the [-range=N] attribute. *)
    [@@deriving sexp_of]
  end

  type t =
    { name : string
    ; range_or_count : Range_or_count.t option
    ; bang : bool
    ; bar : bool (** Indicates whether there *could* be a [nextcmd]. *)
    ; register : [ `This of char | `Not_provided | `Not_applicable ]
    ; args : string list
    ; expand_args : bool
    ; nargs : Number_of_arguments.t
    ; nextcmd : string option
    ; modifiers : Modifiers.t
    }
  [@@deriving sexp_of]
end

(** These API functions are served immediately without waiting in the input queue. *)
module Fast : sig
  (** Parse text as you would write it on the command line and return a structured
      interpretation. *)
  val parse
    :  here:[%call_pos]
    -> _ Client.t
    -> string
    -> Parse_result.t Deferred.Or_error.t
end
