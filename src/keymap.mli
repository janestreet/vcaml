open Core

module Mode : sig
  (** Map modes. For more information see `:h map-modes`. In the documentation here and in
      the implementation, we refer to a mode as "simple" if it corresponds with a single
      Vim mode, and as "complex" if it corresponds with multiple Vim modes. Language
      mappings are special and don't neatly fall into this distinction.

      Simple modes: Normal, Operator_pending, Insert, Cmd_line, Select, Visual, Terminal
      Complex modes: Visual_and_select, Normal_and_visual_and_operator_pending,
      Insert_and_command_line *)
  type t =
    | Normal
    | Operator_pending
    | Insert
    | Cmd_line
    | Select
    | Visual
    | Terminal
    | Visual_and_select
    | Normal_and_visual_and_operator_pending
    | Insert_and_command_line
    | Language
  [@@deriving compare, enumerate, equal, sexp_of]

  include Stringable.S with type t := t
end

(** A representation of a keybinding. *)
type t =
  { lhs : string
  ; rhs : string
  ; mode : Mode.t
  ; scope : [ `Global | `Buffer_local of Types.Buffer.t ]
  ; expr : bool
  ; nowait : bool
  ; silent : bool
  ; recursive : bool
  ; sid : int
  }
[@@deriving sexp_of]

(* Queries keymappings for a given scope and mode. For simple modes, returns all
   mappings that apply in that mode. For complex modes, returns all mappings that apply
   in any of the constituent modes. Queries for [Language] mode return only language
   mappings. *)
val get
  :  scope:[ `Global | `Buffer_local of Types.Buffer.t ]
  -> mode:Mode.t
  -> t list Api_call.Or_error.t

(** Add a keymapping. Only use [recursive] if you really know what you are doing. Also
    note that [nowait] is only meaningful for buffer-local mappings; on global mappings it
    has no effect (see :h map-nowait). *)
val set
  :  ?recursive:bool (** default: [false]. *)
  -> ?expr:bool (** default: [false] *)
  -> ?nowait:bool (** default: [false] *)
  -> ?silent:bool (** default: [false] *)
  -> ?scope:[ `Global | `Current_buffer ] (** default: [`Global] *)
  -> lhs:string
  -> rhs:string
  -> mode:Mode.t
  -> unit
  -> unit Api_call.Or_error.t
