open Core
open Import0
module Buffer = Nvim_internal.Buffer

module Connected_to = struct
  type t =
    | Stdio
    | Stderr
    | Socket
    | Job
  [@@deriving sexp_of]

  let of_msgpack msgpack =
    let open Or_error.Let_syntax in
    match%bind Type.of_msgpack String msgpack with
    | "stdio" -> Ok Stdio
    | "stderr" -> Ok Stderr
    | "socket" -> Ok Socket
    | "job" -> Ok Job
    | stream -> Or_error.error_s [%message "Unrecognized channel stream type" stream]
  ;;
end

module Protocol = struct
  type t =
    | Ansi (** Connected to a terminal *)
    | Msgpack_rpc
  [@@deriving sexp_of]

  let of_msgpack msgpack =
    let open Or_error.Let_syntax in
    match%bind Type.of_msgpack String msgpack with
    | "bytes" -> Ok None
    | "terminal" -> Ok (Some Ansi)
    | "rpc" -> Ok (Some Msgpack_rpc)
    | stream -> Or_error.error_s [%message "Unrecognized channel mode" stream]
  ;;
end

type t =
  { id : int
  ; connected_to : Connected_to.t
  ; protocol : Protocol.t option
  ; pty : string option
  ; buffer : Buffer.t option
  ; client : Client_info.t option
  }
[@@deriving sexp_of]

let of_msgpack_map map =
  let open Or_error.Let_syntax in
  let%bind id = find_or_error_and_convert map "id" (Type.of_msgpack Int) in
  let%bind connected_to =
    find_or_error_and_convert map "stream" Connected_to.of_msgpack
  in
  let%bind protocol = find_or_error_and_convert map "mode" Protocol.of_msgpack in
  let%bind pty = find_and_convert map "pty" (Type.of_msgpack String) in
  let%bind buffer = find_and_convert map "buffer" Buffer.of_msgpack in
  let%bind client = find_and_convert map "client" Client_info.of_msgpack in
  return { id; connected_to; protocol; pty; buffer; client }
;;
