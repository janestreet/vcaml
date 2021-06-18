open Core
module Unix = Core_unix
module Parser = Angstrom.Buffered

let msgid = ref 0

type t =
  { socket : Unix.File_descr.t
  ; bytes : Bytes.t
  ; mutable state : Msgpack.t Angstrom.Buffered.state
  ; mutable channel : int
  }

let close t = Unix.close t.socket
let channel t = t.channel
let debug = ref true
let print_s sexp = if !debug then print_s sexp

let send t message =
  let buf = Msgpack.string_of_t_exn message in
  let len = String.length buf in
  let bytes_written = Unix.single_write_substring ~pos:0 ~len t.socket ~buf in
  match bytes_written = len with
  | false -> failwith "Failed to send message"
  | true -> print_s [%message "Sent" ~_:(message : Msgpack.t)]
;;

let request t method_name params =
  incr msgid;
  let message =
    Msgpack.Array [ Integer 0; Integer !msgid; String method_name; Array params ]
  in
  send t message
;;

let notify t method_name params =
  let message = Msgpack.Array [ Integer 2; String method_name; Array params ] in
  send t message
;;

let respond t ~msgid response =
  let message =
    match response with
    | Ok result -> Msgpack.Array [ Integer 1; Integer msgid; Nil; result ]
    | Error error -> Msgpack.Array [ Integer 1; Integer msgid; error; Nil ]
  in
  send t message
;;

let rec receive t =
  match t.state with
  | Fail (_, _, msg) ->
    close t;
    raise_s [%message "Failed to parse message" msg]
  | Partial _ as state ->
    (match
       (Unix.select ~read:[ t.socket ] ~write:[] ~except:[] ~timeout:`Immediately ()).read
     with
     | [] -> `Waiting_for_neovim
     | _ :: _ ->
       let input =
         match Unix.read ~pos:0 ~len:(Bytes.length t.bytes) t.socket ~buf:t.bytes with
         | 0 -> `Eof
         | bytes_read -> `String (Bytes.To_string.sub ~pos:0 ~len:bytes_read t.bytes)
       in
       t.state <- Parser.feed state input;
       receive t)
  | Done ({ buf; off; len }, message) ->
    print_s [%message "Received" ~_:(message : Msgpack.t)];
    let remaining_input = Bigstring.sub buf ~pos:off ~len in
    t.state
    <- Parser.feed
         (Parser.parse Msgpack.Internal.Parser.msg)
         (`Bigstring remaining_input);
    `Message message
;;

let receive_all_available t =
  let open Reversed_list in
  let rec aux t ~messages =
    match receive t with
    | `Waiting_for_neovim -> rev messages
    | `Message message -> aux t ~messages:(message :: messages)
  in
  aux t ~messages:[]
;;

let open_ socketname =
  debug := false;
  let socket = Unix.socket ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0 () in
  Unix.connect socket ~addr:(ADDR_UNIX socketname);
  let t =
    { socket
    ; bytes = Bytes.create 4096
    ; state = Parser.parse Msgpack.Internal.Parser.msg
    ; channel = -1
    }
  in
  let name = Uuid.to_string (Uuid_unix.create ()) in
  notify t "nvim_set_client_info" [ String name; Map []; String "remote"; Map []; Map [] ];
  request t "nvim_list_chans" [];
  let rec wait_for_response t = function
    | 0 -> failwith "Timed out waiting for Neovim to respond"
    | n ->
      (match receive t with
       | `Message message -> message
       | `Waiting_for_neovim ->
         ignore (Unix.nanosleep 0.1 : float);
         wait_for_response t (n - 1))
  in
  let channel =
    match wait_for_response t 10 with
    | Array [ Integer 1; Integer m; Nil; Array channel_infos ] when !msgid = m ->
      channel_infos
      (* We only use Vcaml to parse the channel info because it's tedious to replicate
         that logic. We don't open it anywhere else because the purpose of this tool is to
         see if a problem lies outside Vcaml. *)
      |> List.map ~f:Vcaml.Channel_info.of_msgpack
      |> Or_error.combine_errors
      |> ok_exn
      |> List.find_map ~f:(fun channel_info ->
        Option.some_if
          (Option.exists channel_info.client ~f:(fun client_info ->
             Option.exists client_info.name ~f:(String.equal name)))
          channel_info.id)
      |> Option.value_exn
    | message -> raise_s [%message "Failed to parse channels" (message : Msgpack.t)]
  in
  t.channel <- channel;
  debug := true;
  t
;;
