open Core
open Async

module Format = struct
  module T = struct
    type t =
      | Bytes
      | Json
      | Hex
      | Sexp
    [@@deriving enumerate, sexp_of]
  end

  include T

  let arg_type = Command.Arg_type.enumerated_sexpable ~case_sensitive:false (module T)
end

let rec jsonaf_of_msgpack msgpack =
  let open Jsonaf.Export in
  match (msgpack : Msgpack.t) with
  | Nil -> jsonaf_of_unit ()
  | Bool bool -> jsonaf_of_bool bool
  | String string -> jsonaf_of_string string
  | Binary bytes -> jsonaf_of_bytes bytes
  | Int int -> jsonaf_of_int int
  | Int64 int64 -> `Number (Printf.sprintf "%Ld" int64)
  | Uint64 uint64 -> `Number (Printf.sprintf "%Lu" uint64)
  | Float float -> jsonaf_of_float float
  | Array ts -> `Array (List.map ts ~f:jsonaf_of_msgpack)
  | Map alist ->
    `Object
      (List.map alist ~f:(fun (key, value) ->
         match key with
         | String key -> key, jsonaf_of_msgpack value
         | _ ->
           raise_s
             [%message
               "Cannot convert Msgpack map key to JSON map key - not a string"
                 (key : Msgpack.t)]))
  | Ext { type_id; data } ->
    `Object [ "type", jsonaf_of_int type_id; "data", jsonaf_of_bytes data ]
;;

let rec msgpack_of_jsonaf : Jsonaf.t -> Msgpack.t = function
  | `Null -> Nil
  | `False -> Bool false
  | `True -> Bool true
  | `String string -> String string
  | `Number string ->
    (match Int.of_string string with
     | int -> Int int
     | exception _ ->
       (match Scanf.sscanf string "%Ld%!" Fn.id with
        | int64 -> Int64 int64
        | exception _ ->
          (match Scanf.sscanf string "%Lu%!" Fn.id with
           | uint64 -> Uint64 uint64
           | exception _ ->
             (match Float.of_string string with
              | float -> Float float
              | exception _ -> raise_s [%message "Failed to parse number" ~_:string]))))
  | `Object [ ("type", `Number type_id); ("data", `String data) ] ->
    Ext { type_id = Int.of_string type_id; data = Bytes.of_string data }
  | `Object alist ->
    alist
    |> List.map ~f:(fun (key, value) -> Msgpack.String key, msgpack_of_jsonaf value)
    |> Map
  | `Array ts -> Array (List.map ts ~f:msgpack_of_jsonaf)
;;

(* Check if the two msgpack values are effectively equivalent (accounts for lossiness due
   to JSON translation). *)
let rec effectively_equivalent_msgpack m1 m2 =
  match (m1 : Msgpack.t), (m2 : Msgpack.t) with
  | Nil, Nil -> true
  | Nil, _ -> false
  | Bool b1, Bool b2 -> Bool.equal b1 b2
  | Bool _, _ -> false
  | String s1, String s2 -> String.equal s1 s2
  | String s1, Binary s2 -> String.equal s1 (Bytes.to_string s2)
  | String _, _ -> false
  | Binary s1, String s2 -> String.equal (Bytes.to_string s1) s2
  | Binary s1, Binary s2 -> Bytes.equal s1 s2
  | Binary _, _ -> false
  | Int i1, Int i2 -> Int.equal i1 i2
  | Int i1, Int64 i2 -> Int64.equal (Int64.of_int i1) i2
  | Int i1, Uint64 i2 -> i1 >= 0 && Int64.equal (Int64.of_int i1) i2
  | Int i1, Float i2 -> Float.is_integer i2 && i1 = Float.to_int i2
  | Int _, _ -> false
  | Int64 i1, Int i2 -> Int64.equal i1 (Int64.of_int i2)
  | Int64 i1, Int64 i2 -> Int64.equal i1 i2
  | Int64 i1, Uint64 i2 -> Int64.(i1 > zero) && Int64.equal i1 i2
  | Int64 i1, Float i2 -> Float.is_integer i2 && Int64.equal i1 (Float.to_int64 i2)
  | Int64 _, _ -> false
  | Uint64 i1, Int i2 -> i2 >= 0 && Int64.equal i1 (Int64.of_int i2)
  | Uint64 i1, Int64 i2 -> Int64.(i2 > zero) && Int64.equal i1 i2
  | Uint64 i1, Uint64 i2 -> Int64.equal i1 i2
  | Uint64 i1, Float i2 ->
    Float.is_integer i2 && Float.is_positive i2 && Int64.equal i1 (Float.to_int64 i2)
  | Uint64 _, _ -> false
  | Float i1, Int i2 -> Float.is_integer i1 && Float.to_int i1 = i2
  | Float i1, Int64 i2 -> Float.is_integer i1 && Int64.equal (Float.to_int64 i1) i2
  | Float i1, Uint64 i2 ->
    Float.is_integer i1 && Float.is_positive i1 && Int64.equal (Float.to_int64 i1) i2
  | Float f1, Float f2 -> Float.equal f1 f2
  | Float _, _ -> false
  | Array a1, Array a2 -> List.equal effectively_equivalent_msgpack a1 a2
  | Array _, _ -> false
  | Map a1, Map a2 ->
    let effectively_equivalent_msgpack_maps =
      Tuple2.equal ~eq1:Msgpack.equal ~eq2:effectively_equivalent_msgpack
    in
    List.equal effectively_equivalent_msgpack_maps a1 a2
  | Map _, _ -> false
  | Ext e1, Ext e2 -> e1.type_id = e2.type_id && Bytes.equal e1.data e2.data
  | Ext _, _ -> false
;;

let hex_of_bytes bytes =
  String.concat_map bytes ~f:(fun byte -> sprintf "%02x" (Char.to_int byte))
;;

let conv ~from ~to_ ~reader ~writer =
  let%bind reader =
    match (from : Format.t) with
    | Bytes | Json | Sexp -> return reader
    | Hex ->
      let pipe_r =
        Reader.pipe reader
        |> Pipe.map' ~max_queue_length:1 ~f:(fun queue ->
          return (Queue.concat_map queue ~f:String.to_list))
      in
      Pipe.create_reader ~close_on_exception:false (fun writer ->
        let write2 c1 c2 =
          let hex_digits = String.of_char_list [ c1; c2 ] in
          Scanf.sscanf hex_digits "%02x" (fun code ->
            code |> Char.of_int_exn |> String.of_char |> Pipe.write writer)
        in
        Deferred.repeat_until_finished None (fun leftover ->
          match%bind Pipe.read_exactly pipe_r ~num_values:2 with
          | `Eof ->
            (match leftover with
             | None ->
               Pipe.close writer;
               return (`Finished ())
             | Some ch -> failwithf "Invalid hex input: trailing char '%c'" ch ())
          | `Fewer queue ->
            let first = Queue.get queue 0 in
            (match leftover with
             | None -> return (`Repeat (Some first))
             | Some ch ->
               let%map () = write2 ch first in
               `Repeat None)
          | `Exactly queue ->
            let first = Queue.get queue 0 in
            let second = Queue.get queue 1 in
            (match leftover with
             | None ->
               let%map () = write2 first second in
               `Repeat None
             | Some ch ->
               let%map () = write2 ch first in
               `Repeat (Some second))))
      |> Reader.of_pipe (Info.of_string "stdin")
  in
  let write_msgpack =
    let msgpack_to_output_format =
      match (to_ : Format.t) with
      | Bytes -> Msgpack.string_of_t_exn ?bufsize:None
      | Hex -> Fn.compose hex_of_bytes Msgpack.string_of_t_exn
      | Json ->
        (* Inserting a trailing newline here delineates the different JSON messages (e.g.,
           allowing "12" and "1" "2" to be distinguished) and also makes the CLI output
           more readable. *)
        Fn.compose (sprintf !"%{Jsonaf#hum}\n") jsonaf_of_msgpack
      | Sexp ->
        fun msgpack ->
          msgpack
          |> [%sexp_of: Msgpack.t]
          |> Sexp.to_string_hum
          (* Adding whitespace after "Nil" is needed to avoid conflation with the next
             sexp, which may also be Nil, in which case the two strings will be read as a
             single atom. All other Mspgack constructors have arguments, so they have sexp
             list representations. *)
          |> (function
           | "Nil" -> "Nil "
           | sexp -> sexp)
    in
    fun msgpack ->
      Writer.write writer (msgpack_to_output_format msgpack);
      Writer.flushed writer
  in
  let%bind read_result =
    match from with
    | Bytes | Hex ->
      Angstrom_async.parse_many Msgpack.Internal.Parser.msg write_msgpack reader
    | Json ->
      (match%bind
         Angstrom_async.parse_many
           Jsonaf_kernel.Parser.t_without_trailing_whitespace
           (Fn.compose write_msgpack msgpack_of_jsonaf)
           reader
       with
       | Error _ as error -> return error
       | Ok () ->
         Angstrom_async.parse
           Angstrom.(
             skip_while Char.is_whitespace *> peek_char
             >>= function
             | None -> return ()
             | Some ch -> fail (sprintf "'%c'" ch) <?> "Trailing character")
           reader)
    | Sexp ->
      Monitor.try_with (fun () ->
        Reader.read_sexps reader
        |> Pipe.iter ~f:(Fn.compose write_msgpack [%of_sexp: Msgpack.t]))
      >>| Result.map_error ~f:Exn.to_string
  in
  let%map () = Writer.close writer
  and () = Reader.close reader in
  match read_result with
  | Error message -> failwith message
  | Ok () -> ()
;;

let command =
  Command.async
    ~behave_nicely_in_pipeline:true
    ~summary:"Convert Msgpack messages"
    (let%map_open.Command () = return ()
     and from = flag "from" (required Format.arg_type) ~doc:"FORMAT Input format"
     and to_ = flag "to" (required Format.arg_type) ~doc:"FORMAT Output format" in
     fun () -> conv ~from ~to_ ~reader:(force Reader.stdin) ~writer:(force Writer.stdout))
;;

let%expect_test "Msgpack ~= Msgpack->JSON->Msgpack" =
  Quickcheck.test
    (Msgpack.quickcheck_generator ~only_string_keys:true ~only_finite_floats:true)
    ~sexp_of:[%sexp_of: Msgpack.t]
    ~f:(fun expected ->
      let json = jsonaf_of_msgpack expected in
      let actual = msgpack_of_jsonaf json in
      match effectively_equivalent_msgpack expected actual with
      | true -> ()
      | false ->
        raise_s
          [%message
            "Not effectively equivalent"
              (expected : Msgpack.t)
              (actual : Msgpack.t)
              (json : Jsonaf.t)]);
  [%expect {| |}];
  return ()
;;

let%expect_test "Msgpack->JSON->Msgpack == Msgpack->JSON->Msgpack->JSON->Msgpack" =
  Quickcheck.test
    (Msgpack.quickcheck_generator ~only_string_keys:true ~only_finite_floats:true)
    ~sexp_of:[%sexp_of: Msgpack.t]
    ~f:(fun original ->
      let expected = original |> jsonaf_of_msgpack |> msgpack_of_jsonaf in
      let json = jsonaf_of_msgpack expected in
      let actual = msgpack_of_jsonaf json in
      match Msgpack.equal expected actual with
      | true -> ()
      | false ->
        raise_s
          [%message
            "Not exactly equivalent"
              (original : Msgpack.t)
              (expected : Msgpack.t)
              (actual : Msgpack.t)
              (json : Jsonaf.t)]);
  [%expect {| |}];
  return ()
;;

let%expect_test "Msgpack ~= Msgpack->Sexp->Msgpack" =
  Quickcheck.test
    (* We don't want to test NAN with roundtripping through sexp because [of_sexp: float]
       converts -NAN to NAN. *)
    (Msgpack.quickcheck_generator ~only_string_keys:false ~only_finite_floats:true)
    ~sexp_of:[%sexp_of: Msgpack.t]
    ~f:(fun expected ->
      let sexp = [%sexp_of: Msgpack.t] expected in
      let actual = [%of_sexp: Msgpack.t] sexp in
      match effectively_equivalent_msgpack expected actual with
      | true -> ()
      | false ->
        raise_s
          [%message
            "Not effectively equivalent"
              ~expected:(Msgpack.string_of_t_exn expected)
              ~actual:(Msgpack.string_of_t_exn actual)
              (sexp : Sexp.t)
              (actual : Msgpack.t)]);
  [%expect {| |}];
  return ()
;;

let%expect_test "Msgpack->Sexp->Msgpack == Msgpack->Sexp->Msgpack->Sexp->Msgpack" =
  Quickcheck.test
    (Msgpack.quickcheck_generator ~only_string_keys:false ~only_finite_floats:true)
    ~sexp_of:[%sexp_of: Msgpack.t]
    ~f:(fun original ->
      let expected = original |> [%sexp_of: Msgpack.t] |> [%of_sexp: Msgpack.t] in
      let sexp = [%sexp_of: Msgpack.t] expected in
      let actual = [%of_sexp: Msgpack.t] sexp in
      match Msgpack.equal expected actual with
      | true -> ()
      | false ->
        raise_s
          [%message
            "Not exactly equivalent"
              ~original:(Msgpack.string_of_t_exn original)
              ~expected:(Msgpack.string_of_t_exn expected)
              ~actual:(Msgpack.string_of_t_exn actual)
              (sexp : Sexp.t)]);
  [%expect {| |}];
  return ()
;;

let quickcheck_conv_roundtrip quickcheck_generator ~format =
  let pipe () =
    let%map `Reader reader_fd, `Writer writer_fd = Unix.pipe (Info.of_string "") in
    Writer.create writer_fd, Reader.create reader_fd
  in
  let%bind writer1, reader1 = pipe () in
  let%bind writer2, reader2 = pipe () in
  let%bind writer3, reader3 = pipe () in
  let pass1 = conv ~from:Bytes ~to_:format ~reader:reader1 ~writer:writer2 in
  let pass2 = conv ~from:format ~to_:Bytes ~reader:reader2 ~writer:writer3 in
  let%bind () =
    Async_quickcheck.async_test
      quickcheck_generator
      ~sexp_of:[%sexp_of: Msgpack.t]
      ~f:(fun msgpack ->
        let expected_bytes = Msgpack.string_of_t_exn msgpack in
        Writer.write writer1 expected_bytes;
        let%bind () = Writer.flushed writer1 in
        let actual_bytes = Bytes.create (String.length expected_bytes) in
        match%map Reader.read reader3 actual_bytes with
        | `Eof ->
          raise_s
            [%message
              "Expected bytes but got EOF" (expected_bytes : string) (msgpack : Msgpack.t)]
        | `Ok n_bytes ->
          let actual_bytes =
            (* We use [String.prefix] here in case
               [n_bytes < String.length expected_bytes] so we exclude trailing garbage. *)
            String.prefix (Bytes.to_string actual_bytes) n_bytes
          in
          (match
             n_bytes = String.length expected_bytes
             && String.equal actual_bytes expected_bytes
           with
           | true -> ()
           | false ->
             raise_s
               [%message
                 "Mismatch between actual and expected bytes"
                   (expected_bytes : string)
                   (actual_bytes : string)
                   (msgpack : Msgpack.t)]))
  in
  let%bind () = Writer.close writer1 in
  let%bind () = pass1 in
  let%bind () = pass2 in
  let%bind () =
    match%map Reader.contents reader3 with
    | "" -> ()
    | contents -> raise_s [%message "Unconsumed bytes" ~_:(contents : string)]
  in
  return ()
;;

let%expect_test "Hex/Bytes Roundtrip" =
  let%map () =
    quickcheck_conv_roundtrip
      (Msgpack.quickcheck_generator ~only_string_keys:false ~only_finite_floats:false)
      ~format:Hex
  in
  [%expect {| |}]
;;

let%expect_test "Sexp/Bytes Roundtrip" =
  let%map () =
    quickcheck_conv_roundtrip
      (Msgpack.quickcheck_generator ~only_string_keys:false ~only_finite_floats:true)
      ~format:Sexp
  in
  [%expect {| |}]
;;

let%expect_test "Json/Bytes Roundtrip" =
  let%map () =
    quickcheck_conv_roundtrip
      (let%map.Quickcheck.Generator msg =
         Msgpack.quickcheck_generator ~only_string_keys:true ~only_finite_floats:true
       in
       (* Since we already checked the correctness of the Msgpack->JSON roundtripping in
          the earlier tests, we just test the [conv] roundtrip on the lossy version. *)
       msg |> jsonaf_of_msgpack |> msgpack_of_jsonaf)
      ~format:Json
  in
  [%expect {| |}]
;;
