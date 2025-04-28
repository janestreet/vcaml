open Core
module Api_result = Nvim_internal.Api_result
module Api_version = Nvim_internal.Api_version
module Error_type = Nvim_internal.Error_type
module Type = Nvim_internal.Phantom

let find_and_convert map (key : string) convert =
  match Map.find map key with
  | None -> Ok None
  | Some msgpack -> convert msgpack |> Or_error.map ~f:Option.return
;;

let find_or_error_and_convert map key convert =
  match find_and_convert map key convert with
  | Error _ as error -> error
  | Ok (Some result) -> Ok result
  | Ok None ->
    Or_error.error_s [%message "Missing key" key (map : Msgpack.t String.Map.t)]
;;

(* Transforms the witness of an [Api_result.t] into a different type. This is used in
   cases where we know more about the structure of the returned value than is reported by
   [api_info] (which we use for auto-generating the witnesses). Disciplined use of
   [map_witness] instead of binding on the [Or_error.t] result provides better error
   reporting, as all conversion happens in the context of [Type.of_msgpack], which reports
   the full message on failure. *)
let map_witness (type a b) (api_result : a Api_result.t) ~(f : a -> b Or_error.t) =
  let witness =
    Type.Custom
      (module struct
        type t = b

        let of_msgpack msgpack =
          msgpack |> Type.of_msgpack api_result.witness |> Or_error.bind ~f
        ;;

        let to_msgpack (_ : t) = failwith "Only [of_msgpack] is supported for this type"
      end)
  in
  Api_result.{ api_result with witness }
;;

module Source_code_position = struct
  type t = Source_code_position.t [@@deriving equal]

  let vcaml_root =
    match [%here].pos_fname |> Filename.dirname |> Filename.dirname with
    | "." -> ""
    | root -> root
  ;;

  let sexp_of_t (t : t) =
    let t =
      match String.chop_prefix t.pos_fname ~prefix:vcaml_root with
      | None -> t
      | Some path -> { t with pos_fname = "lib/vcaml" ^/ path }
    in
    match Dynamic.get Backtrace.elide with
    | false -> [%sexp (t : Source_code_position.t)]
    | true -> Atom (sprintf !"%{Source_code_position.pos_fname}:LINE:COL" t)
  ;;
end
