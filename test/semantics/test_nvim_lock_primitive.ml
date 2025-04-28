open Core
module Nvim_lock = Vcaml.Private.Nvim_lock
module Permission_to_run = Nvim_lock.Permission_to_run

(* These tests exercise the [Nvim_lock.t] primitive. See ./test_blocking_nvim.ml for the
   higher-level integration tests of VCaml's blocking semantics. *)

module Action = struct
  type t =
    | Take of { id : int }
    | Expire of { id : int }
    | Expire_other_users of
        { id_to_expire : int
        ; new_id : int
        }
  [@@deriving sexp_of]
end

module Doubly_linked = struct
  include Doubly_linked

  let get_elt t idx = findi_elt t ~f:(fun i _ -> i = idx) |> Option.value_exn |> snd
  let get t idx = Elt.value (get_elt t idx)

  let remove_nth t idx =
    let elt = get_elt t idx in
    remove t elt;
    Elt.value elt
  ;;
end

(* This generator generates a valid sequence of actions - specifically, the ids that are
   expired are ones that already exist - and uses weights that attempt to increase the
   likelihood of interesting cases being exercised. *)
let generator =
  let open Quickcheck.Generator in
  let open Quickcheck.Generator.Let_syntax in
  let%bind remaining = Int.gen_uniform_incl 0 1000 in
  let gen_new = return `New in
  let gen_use_expired = return `Use_expired in
  let gen_use_unexpired = return `Use_unexpired in
  let gen_expiration_method =
    match%map bool with
    | true -> `Expire
    | false -> `Expire_other_users
  in
  let expired = Doubly_linked.create () in
  let unexpired = Doubly_linked.create () in
  let rec gen_actions = function
    | 0 -> return []
    | remaining ->
      let%bind action =
        [ 5.0, gen_new
        ; 1.0 *. Float.of_int (Doubly_linked.length unexpired), gen_use_unexpired
        ; 0.01 *. Float.of_int (Doubly_linked.length expired), gen_use_expired
        ]
        |> weighted_union
      in
      let new_id () =
        let id = Doubly_linked.length unexpired + Doubly_linked.length expired in
        ignore (Doubly_linked.insert_first unexpired id : int Doubly_linked.Elt.t);
        id
      in
      let%bind action =
        match action with
        | `New -> return (Action.Take { id = new_id () })
        | `Use_expired ->
          let%map idx = Int.gen_uniform_incl 0 (Doubly_linked.length expired - 1)
          and expiration_method = gen_expiration_method in
          let id = Doubly_linked.get expired idx in
          (match expiration_method with
           | `Expire -> Action.Expire { id }
           | `Expire_other_users ->
             Expire_other_users { id_to_expire = id; new_id = new_id () })
        | `Use_unexpired ->
          let%map idx = Int.gen_uniform_incl 0 (Doubly_linked.length unexpired - 1)
          and expiration_method = gen_expiration_method in
          let id = Doubly_linked.remove_nth unexpired idx in
          ignore (Doubly_linked.insert_first expired id : int Doubly_linked.Elt.t);
          (match expiration_method with
           | `Expire -> Action.Expire { id }
           | `Expire_other_users ->
             Expire_other_users { id_to_expire = id; new_id = new_id () })
      in
      let%map actions = gen_actions (remaining - 1) in
      action :: actions
  in
  gen_actions remaining
;;

type permission = [ `Ok | `Expired ] option [@@deriving equal, sexp_of]

let%expect_test "Well-behaved semantics" =
  Quickcheck.test ~sexp_of:[%sexp_of: Action.t list] generator ~f:(fun actions ->
    let permission_pool = Queue.create () in
    let unexpired = Doubly_linked.create () in
    let nvim_lock = Nvim_lock.create () in
    List.iter actions ~f:(function
      | Take { id } ->
        assert (id = Queue.length permission_pool);
        let permission_to_run = Nvim_lock.take nvim_lock in
        let value = Permission_to_run.peek permission_to_run in
        (match value with
         | Some `Ok -> ()
         | None | Some `Expired ->
           raise_s
             [%message
               "[take] returned a [Permission_to_run.t] without permission"
                 (value : permission)]);
        let elt = Doubly_linked.insert_first unexpired permission_to_run in
        Queue.enqueue permission_pool elt
      | Expire { id } ->
        let elt = Queue.get permission_pool id in
        if Doubly_linked.mem_elt unexpired elt then Doubly_linked.remove unexpired elt;
        let permission_to_run = Doubly_linked.Elt.value elt in
        Nvim_lock.expire nvim_lock permission_to_run
      | Expire_other_users { id_to_expire; new_id } ->
        let old_elt = Queue.get permission_pool id_to_expire in
        let old_permission_to_run = Doubly_linked.Elt.value old_elt in
        let old_value = Permission_to_run.peek old_permission_to_run in
        let new_permission_to_run =
          Nvim_lock.expire_other_users nvim_lock old_permission_to_run
        in
        let new_value = Permission_to_run.peek new_permission_to_run in
        if not ([%equal: permission] old_value new_value)
        then
          raise_s
            [%message
              "[expire_other_users] returned a permission with a different value"
                (old_value : permission)
                (new_value : permission)];
        assert (new_id = Queue.length permission_pool);
        let new_elt =
          match Doubly_linked.mem_elt unexpired old_elt with
          | true ->
            let new_elt =
              Doubly_linked.insert_before unexpired old_elt new_permission_to_run
            in
            Doubly_linked.remove unexpired old_elt;
            new_elt
          | false ->
            Doubly_linked.insert_first (Doubly_linked.create ()) new_permission_to_run
        in
        Queue.enqueue permission_pool new_elt);
    (* Now that we've run the actions, check the invariants. *)
    let latest_permission = Doubly_linked.remove_first unexpired in
    Option.iter latest_permission ~f:(fun permission_to_run ->
      let value = Permission_to_run.peek permission_to_run in
      match value with
      | Some `Ok -> ()
      | None | Some `Expired ->
        raise_s
          [%message
            "The most recently taken unexpired permission does not have permission to run"
              (value : permission)]);
    Queue.iteri permission_pool ~f:(fun id elt ->
      let permission_to_run = Doubly_linked.Elt.value elt in
      match Permission_to_run.peek permission_to_run with
      | Some `Expired ->
        if Doubly_linked.mem_elt unexpired elt
        then
          raise_s
            [%message
              "Found an expired [Permission_to_run.t] that should be empty" (id : int)]
      | Some `Ok ->
        (match latest_permission with
         | Some latest_permission when phys_equal permission_to_run latest_permission ->
           ()
         | _ ->
           raise_s
             [%message
               "Found a [Permission_to_run.t] that was [`Ok] but is not the most \
                recently taken unexpired permission"
                 (id : int)])
      | None ->
        if not (Doubly_linked.mem_elt unexpired elt)
        then
          raise_s
            [%message
              "Found an empty [Permission_to_run.t] that should be expired" (id : int)]));
  [%expect {| |}]
;;
