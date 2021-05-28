(** A [Notifier.t] sends API calls as asynchronous notifications. In most cases you don't
    need this; you can just send a regular request and not wait for the result. However,
    there are some API calls that are documented as being more performant when sent as
    notifications. *)

module Notification : sig
  type t

  module Untested : sig
    (** From `:h api-highlights`: If an external highlighter plugin wants to add many
        highlights in a batch, performance can be improved by calling
        [nvim_buf_add_highlight] as an asynchronous notification, after first
        (synchronously) requesting a source id. *)
    val nvim_buf_add_highlight
      :  buffer:Nvim_internal.Buffer.t
      -> ns_id:int
      -> hl_group:string
      -> line:int
      -> col_start:int
      -> col_end:int
      -> t
  end

  module Defun : sig
    module Vim : sig
      module Type := Nvim_internal.Phantom

      type notification := t
      type ('fn, 'leftmost_input) t

      (** N.B. If you are wrapping a function that takes no arguments, just use [unit]. Do
          not use [Nil @-> unit]. *)
      val unit : (notification, notification) t

      val ( @-> ) : 'a Type.t -> ('b, _) t -> ('a -> 'b, 'a) t
    end
  end

  (** This function is analogous to [wrap_viml_function]. *)
  val custom : type_:('fn, _) Defun.Vim.t -> function_name:string -> 'fn
end

type t

module Error_type = Nvim_internal.Error_type

(** Only one [t] should be created for a given client. *)
val create : Client.t -> on_error_event:(Error_type.t -> message:string -> unit) -> t

val notify : t -> Notification.t -> unit

module For_testing : sig
  val send_raw : t -> function_name:string -> params:Msgpack.t -> unit
end
