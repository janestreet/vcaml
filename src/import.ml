open Core

let tag_callsite here =
  let here =
    match !Backtrace.elide with
    | false -> [%sexp (here : Source_code_position.t)]
    | true -> Atom (sprintf !"%{Source_code_position.pos_fname}:LINE:COL" here)
  in
  Or_error.tag_s ~tag:[%message "Called from" ~_:(here : Sexp.t)]
;;
