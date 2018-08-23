(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(* [Lwt_sequence] is deprecated – we don't want users outside Lwt using it.
   However, it is still used internally by Lwt. So, briefly disable warning 3
   ("deprecated"), and create a local, non-deprecated alias for
   [Lwt_sequence] that can be referred to by the rest of the code in this
   module without triggering any more warnings. *)
[@@@ocaml.warning "-3"]
module Lwt_sequence = Lwt_sequence
[@@@ocaml.warning "+3"]

open Lwt.Infix

let enter_iter_hooks = Lwt_sequence.create ()
let leave_iter_hooks = Lwt_sequence.create ()
let yielded = Lwt_sequence.create ()

let yield () = (Lwt.add_task_r [@ocaml.warning "-3"]) yielded

let run_already_called = ref `No
let run_already_called_mutex = Mutex.create ()

let run p =
  (* Fail in case a call to Lwt_main.run is nested under another invocation of
     Lwt_main.run. *)
  Mutex.lock run_already_called_mutex;

  let error_message_if_call_is_nested =
    match !run_already_called with
    | `From backtrace_string ->
      Some (Printf.sprintf "%s\n%s\n%s"
        "Nested calls to Lwt_main.run are not allowed"
        "Lwt_main.run already called from:"
        backtrace_string)
    | `From_somewhere ->
      Some ("Nested calls to Lwt_main.run are not allowed")
    | `No ->
      let called_from =
        if Printexc.backtrace_status () then
          let backtrace =
            try raise Exit
            with Exit -> Printexc.get_backtrace ()
          in
          `From backtrace
        else
          `From_somewhere
      in
      run_already_called := called_from;
      None
  in

  Mutex.unlock run_already_called_mutex;

  begin match error_message_if_call_is_nested with
  | Some message -> failwith message
  | None -> ()
  end;

  let rec run_loop () =
    (* Fulfill paused promises now. *)
    Lwt.wakeup_paused ();
    match Lwt.poll p with
    | Some x ->
      x
    | None ->
      (* Call enter hooks. *)
      Lwt_sequence.iter_l (fun f -> f ()) enter_iter_hooks;

      (* Do the main loop call. *)
      let should_block_waiting_for_io =
        Lwt.paused_count () = 0 && Lwt_sequence.is_empty yielded in
      Lwt_engine.iter should_block_waiting_for_io;

      (* Fulfill paused promises again. *)
      Lwt.wakeup_paused ();

      (* Fulfill yield promises. *)
      if not (Lwt_sequence.is_empty yielded) then begin
        let tmp = Lwt_sequence.create () in
        Lwt_sequence.transfer_r yielded tmp;
        Lwt_sequence.iter_l (fun resolver -> Lwt.wakeup resolver ()) tmp
      end;

      (* Call leave hooks. *)
      Lwt_sequence.iter_l (fun f -> f ()) leave_iter_hooks;

      (* Repeat. *)
      run_loop ()
  in

  let loop_result = run_loop () in

  Mutex.lock run_already_called_mutex;
  run_already_called := `No;
  Mutex.unlock run_already_called_mutex;

  loop_result

let exit_hooks = Lwt_sequence.create ()

let rec call_hooks () =
  match Lwt_sequence.take_opt_l exit_hooks with
  | None ->
    Lwt.return_unit
  | Some f ->
    Lwt.catch
      (fun () -> f ())
      (fun _  -> Lwt.return_unit) >>= fun () ->
    call_hooks ()

let () =
  at_exit (fun () ->
    Lwt.abandon_wakeups ();
    run (call_hooks ()))

let at_exit f = ignore (Lwt_sequence.add_l f exit_hooks)
