open Core
module Unix = Core_unix
module Parser = Angstrom.Buffered

module Vcaml = struct
  open Vcaml

  (** We only use Vcaml for pretty-printing Neovim's Msgpack extensions. We don't use it
      anywhere else because the purpose of this tool is to see if a problem lies outside
      Vcaml. *)
  let pp = pp
end

include Vcaml

type t =
  { socket : Unix.File_descr.t
  ; bytes : Bytes.t
  ; mutable state : Msgpack.t Angstrom.Buffered.state
  ; mutable channel : int
  ; mutable msgid : int
  ; mutable verbose : bool
  ; mutable closed : bool
  }

let close t =
  Unix.close t.socket;
  t.closed <- true
;;

let channel t = t.channel
let verbose t value = t.verbose <- value

let debug_send, debug_receive =
  let `Peer_1_to_2 send, `Peer_2_to_1 receive =
    Msgpack_debug.create_debug_printers
      ~pp
      ~color:true
      ~peer1:"OCaml"
      ~peer2:"Nvim"
      stdout
  in
  let send t msg = if t.verbose then send msg in
  let receive t msg = if t.verbose then receive msg in
  send, receive
;;

let send t message =
  debug_send t message;
  let buf = Msgpack.string_of_t_exn message in
  let len = String.length buf in
  match t.closed with
  | true -> failwith "Failed to send message (connection is closed)"
  | false ->
    let bytes_written = Unix.single_write_substring ~pos:0 ~len t.socket ~buf in
    if bytes_written < len then failwith "Failed to send message"
;;

let request t method_name params =
  t.msgid <- t.msgid + 1;
  let message = Msgpack.Array [ Int 0; Int t.msgid; String method_name; Array params ] in
  send t message
;;

let notify t method_name params =
  let message = Msgpack.Array [ Int 2; String method_name; Array params ] in
  send t message
;;

let respond t ~msgid response =
  let message =
    match response with
    | Ok result -> Msgpack.Array [ Int 1; Int msgid; Nil; result ]
    | Error error -> Msgpack.Array [ Int 1; Int msgid; error; Nil ]
  in
  send t message
;;

let rec receive t =
  match t.closed with
  | true -> `Connection_closed
  | false ->
    (match t.state with
     | Fail ({ buf; off; len }, marks, msg) ->
       (match off = len with
        | true ->
          close t;
          `Connection_closed
        | false ->
          let unconsumed = Bigstringaf.substring buf ~off ~len in
          raise_s
            [%message
              "Failed to parse message"
                msg
                (marks : string list)
                (unconsumed : String.Hexdump.Pretty.t)])
     | Partial _ as state ->
       (match
          (Unix.select ~read:[ t.socket ] ~write:[] ~except:[] ~timeout:`Immediately ())
            .read
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
       debug_receive t message;
       let remaining_input = Bigstring.sub buf ~pos:off ~len in
       t.state
       <- Parser.feed
            (Parser.parse Msgpack.Internal.Parser.msg)
            (`Bigstring remaining_input);
       `Message message)
;;

let receive_all_available t =
  let open Reversed_list in
  let rec aux t ~messages =
    match receive t with
    | `Connection_closed | `Waiting_for_neovim -> rev messages
    | `Message message -> aux t ~messages:(message :: messages)
  in
  aux t ~messages:[]
;;

let open_ socketname =
  let socket = Unix.socket ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0 () in
  Unix.connect socket ~addr:(ADDR_UNIX socketname);
  let t =
    { socket
    ; bytes = Bytes.create 4096
    ; state = Parser.parse Msgpack.Internal.Parser.msg
    ; channel = -1
    ; msgid = 0
    ; verbose = false
    ; closed = false
    }
  in
  request t "nvim_get_api_info" [];
  let rec wait_for_response t = function
    | 0 -> failwith "Timed out waiting for Neovim to respond"
    | n ->
      (match receive t with
       | `Connection_closed -> failwith "Connection to Neovim closed"
       | `Message message -> message
       | `Waiting_for_neovim ->
         ignore (Unix.nanosleep 0.1 : float);
         wait_for_response t (n - 1))
  in
  let channel =
    match wait_for_response t 10 with
    | Array [ Int 1; Int msgid; Nil; Array [ Int channel; _metadata ] ]
      when t.msgid = msgid -> channel
    | message ->
      raise_s
        [%message "Failed to parse [nvim_get_api_info] response" (message : Msgpack.t)]
  in
  t.channel <- channel;
  t
;;
