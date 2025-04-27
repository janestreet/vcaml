open Core
open Async

module Permission_to_run = struct
  type t = [ `Ok | `Expired ] Mvar.Read_write.t

  let value_available = Mvar.value_available
  let peek = Mvar.peek
  let taken = Mvar.taken
end

type t = Permission_to_run.t Queue.t

let create () = Queue.create ()

let take t =
  Option.iter (Queue.peek_back t) ~f:(fun permission_to_run ->
    match Mvar.take_now_exn permission_to_run with
    | `Ok -> ()
    | `Expired ->
      failwith "BUG: [Nvim_lock.t] is tracking an expired [Permission_to_run.t]");
  let permission_to_run = Mvar.create () in
  Mvar.set permission_to_run `Ok;
  Queue.enqueue t permission_to_run;
  permission_to_run
;;

let rec reset_t_and_restore_permission t =
  match Queue.peek_back t with
  | None -> ()
  | Some permission_to_run ->
    (match Mvar.peek permission_to_run with
     | None -> Mvar.set permission_to_run `Ok
     | Some `Ok ->
       failwith
         "BUG: While restoring permission to run, found a [Permission_to_run.t] that \
          already had permission to run." [@nontail]
     | Some `Expired ->
       let (_ : Permission_to_run.t) = Queue.dequeue_back_exn t in
       reset_t_and_restore_permission t)
;;

let expire t permission_to_run =
  match Mvar.peek permission_to_run with
  | None -> Mvar.set permission_to_run `Expired
  | Some `Expired -> ()
  | Some `Ok ->
    Mvar.set permission_to_run `Expired;
    reset_t_and_restore_permission t
;;

let expire_other_users t old_permission_to_run =
  match Mvar.peek old_permission_to_run with
  | Some `Expired -> old_permission_to_run
  | None | Some `Ok ->
    let result = ref None in
    for i = Queue.length t - 1 downto 0 do
      match !result with
      | Some _ -> ()
      | None ->
        if phys_equal (Queue.get t i) old_permission_to_run
        then (
          let new_permission_to_run = Mvar.create () in
          Option.iter
            (Mvar.peek old_permission_to_run)
            ~f:(Mvar.set new_permission_to_run);
          Mvar.set old_permission_to_run `Expired;
          Queue.set t i new_permission_to_run;
          result := Some new_permission_to_run)
    done;
    (match !result with
     | Some new_permission_to_run -> new_permission_to_run
     | None ->
       failwith
         "BUG: Found an unexpired [Permission_to_run.t] that was not in [Nvim_lock.t]"
       [@nontail])
;;
