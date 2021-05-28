open Core

type nargs =
  [ `Exactly of int
  | `List
  | `At_most_one
  | `At_least_one
  ]

type addr_type =
  [ `Lines
  | `Args
  | `Buffers
  | `Loaded_bufs
  | `Windows
  | `Tabs
  ]

type range_type =
  | Range of
      [ `Defaults_to_current_line
      | `Defaults_to_whole_file
      | `Defaults_to_line_nr of int
      ]
  | Count of int

type t =
  { name : string
  ; definition : string
  ; script_id : int
  ; bang : bool
  ; bar : bool
  ; register : bool
  ; nargs : nargs
  ; complete : string option
  ; complete_arg : string option
  ; range : range_type option
  ; addr : addr_type
  }

val of_msgpack : Msgpack.t -> t Or_error.t
