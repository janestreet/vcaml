type scope =
  | Global
  | Buffer
  | Window
  | Window_per_buffer
  | Tabpage

val generate_options_intf : scope:scope -> unit
val generate_options_impl : scope:scope -> unit
