open! Core
module Buffer = Nvim_internal.Buffer

type t =
  { id : int
  ; stream : [ `Stdio | `Stderr | `Socket | `Job ]
  ; mode : [ `Bytes | `Terminal | `Rpc ]
  ; pty : string option
  ; buffer : Buffer.t option
  ; client : Client_info.t option
  }
[@@deriving sexp_of]

let of_msgpack obj =
  let open Or_error.Let_syntax in
  let%bind m = Extract.map_of_msgpack_map obj in
  let%bind id =
    match Map.find_or_error m "id" with
    | Ok (Integer i) -> return i
    | _ -> Or_error.error_string "malformed channel info"
  in
  let%bind stream =
    match Map.find_or_error m "stream" with
    | Ok (String "stdio") -> return `Stdio
    | Ok (String "stderr") -> return `Stderr
    | Ok (String "socket") -> return `Socket
    | Ok (String "job") -> return `Job
    | _ -> Or_error.error_string "malformed channel info"
  in
  let%bind mode =
    match Map.find_or_error m "mode" with
    | Ok (String "bytes") -> return `Bytes
    | Ok (String "terminal") -> return `Terminal
    | Ok (String "rpc") -> return `Rpc
    | _ -> Or_error.error_string "malformed channel info"
  in
  let%bind pty = Extract.and_convert_optional m "pty" Extract.string in
  let%bind buffer = Extract.and_convert_optional m "buffer" Buffer.of_msgpack in
  let%bind client = Extract.and_convert_optional m "client" Client_info.of_msgpack in
  return { id; stream; mode; pty; buffer; client }
;;
