(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** Main loop and event queue *)

(** This module controls the ``main-loop'' of Lwt. *)

val run : 'a Lwt.t -> 'a
  (** [Lwt_main.run p] calls the Lwt scheduler, performing I/O until [p]
      resolves. [Lwt_main.run p] returns the value in [p] if [p] is fulfilled.
      If [p] is rejected with an exception instead, [Lwt_main.run p] raises that
      exception.

      Every native and bytecode program that uses Lwt should call this function
      at its top level. It implements the Lwt main loop.

      Example:
      {[
let main () = Lwt_io.write_line Lwt_io.stdout "hello world"

let () = Lwt_main.run (main ())
      ]}

      [Lwt_main.run] is not available when targeting JavaScript, because the
      environment (such as Node.js or the browser's script engine) implements
      the I/O loop.

      Nested calls to [Lwt_main.run] are not allowed. That is, do not call
      [Lwt_main.run] in a callback triggered by a promise that is resolved by
      an outer invocation of [Lwt_main.run]. If your program makes such a call,
      [Lwt_main.run] will raise [Failure]. This should be considered a logic
      error (the code is inherently broken).

      It is not safe to call [Lwt_main.run] in a function registered with
      [Pervasives.at_exit], use {!Lwt_main.at_exit} instead. *)

val yield : unit -> unit Lwt.t
  (** [yield ()] is a threads which suspends itself and then resumes
      as soon as possible and terminates. *)

[@@@ocaml.warning "-3"]

val enter_iter_hooks : (unit -> unit) Lwt_sequence.t
  (** Functions that are called before the main iteration. *)

val leave_iter_hooks : (unit -> unit) Lwt_sequence.t
  (** Functions that are called after the main iteration. *)

val exit_hooks : (unit -> unit Lwt.t) Lwt_sequence.t
  (** Sets of functions executed just before the program exit.

      Notes:
      - each hook is called exactly one time
      - exceptions raised by hooks are ignored *)

[@@@ocaml.warning "+3"]

val at_exit : (unit -> unit Lwt.t) -> unit
  (** [at_exit hook] adds hook at the left of [exit_hooks]*)
