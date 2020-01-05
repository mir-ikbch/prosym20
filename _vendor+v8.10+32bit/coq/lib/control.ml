(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(*s interruption *)

let interrupt = ref false

let steps = ref 0

let enable_thread_delay = ref false

(* implemented in coq-js/js_stub/interrupt.js *)
external interrupt_pending : unit -> bool = "interrupt_pending"

let timeout_deadline : (float * (unit -> unit)) option ref = ref None

let jscoq_event_yield () =
  if interrupt_pending() then interrupt := true;
  match !timeout_deadline with
  | Some (time, callback) -> if Unix.gettimeofday () > time then callback ();
  | None -> ()

let check_for_interrupt () =
  jscoq_event_yield ();
  if !interrupt then begin interrupt := false; raise Sys.Break end;
  incr steps;
  if !enable_thread_delay && !steps = 1000 then begin
    Thread.delay 0.001;
    steps := 0;
  end

(** This function does not work on windows, sigh... *)
let unix_timeout n f x e =
  let timeout_handler _ = raise e in
  let psh = Sys.signal Sys.sigalrm (Sys.Signal_handle timeout_handler) in
  let _ = Unix.alarm n in
  let restore_timeout () =
    let _ = Unix.alarm 0 in
    Sys.set_signal Sys.sigalrm psh
  in
  try
    let res = f x in
    restore_timeout ();
    res
  with e ->
    let e = Backtrace.add_backtrace e in
    restore_timeout ();
    Exninfo.iraise e

let windows_timeout n f x e =
  let killed = ref false in
  let exited = ref false in
  let thread init =
    while not !killed do
      let cur = Unix.gettimeofday () in
      if float_of_int n <= cur -. init then begin
        interrupt := true;
        exited := true;
        Thread.exit ()
      end;
      Thread.delay 0.5
    done
  in
  let init = Unix.gettimeofday () in
  let _id = CThread.create thread init in
  try
    let res = f x in
    let () = killed := true in
    let cur = Unix.gettimeofday () in
    (* The thread did not interrupt, but the computation took longer than
       expected. *)
    let () = if float_of_int n <= cur -. init then begin
      exited := true;
      raise Sys.Break
    end in
    res
  with
  | Sys.Break ->
    (* Just in case, it could be a regular Ctrl+C *)
    if not !exited then begin killed := true; raise Sys.Break end
    else raise e
  | e ->
    let () = killed := true in
    let e = Backtrace.add_backtrace e in
    Exninfo.iraise e


let unwind ~(protect:unit -> unit) f =
  try let y = f () in protect (); y
  with e -> protect (); raise e

let jscoq_timeout n f x e =
  let expired = ref false in
  timeout_deadline := Some (Unix.gettimeofday () +. float_of_int n,
                            fun () -> expired := true; interrupt := true);
  let protect () = jscoq_event_yield (); timeout_deadline := None;
                   interrupt := false in
  let res = try unwind ~protect (fun () -> f x)
            with Sys.Break -> raise @@ if !expired then e else Sys.Break in
  if !expired then raise e;
  res

type timeout = { timeout : 'a 'b. int -> ('a -> 'b) -> 'a -> exn -> 'b }

(*
let timeout_fun = match Sys.os_type with
| "Unix" | "Cygwin" -> { timeout = unix_timeout }
| _ -> { timeout = windows_timeout }
*)

let timeout_fun = { timeout = jscoq_timeout }

let timeout_fun_ref = ref timeout_fun
let set_timeout f = timeout_fun_ref := f

let timeout n f e = !timeout_fun_ref.timeout n f e

let protect_sigalrm f x =
  let timed_out = ref false in
  let timeout_handler _ = timed_out := true in
  try
    let old_handler = Sys.signal Sys.sigalrm (Sys.Signal_handle timeout_handler) in
    try
      let res = f x in
      Sys.set_signal Sys.sigalrm old_handler;
      match !timed_out, old_handler with
      | true, Sys.Signal_handle f -> f Sys.sigalrm; res
      | _, _ -> res
    with e ->
      let e = Backtrace.add_backtrace e in
      Sys.set_signal Sys.sigalrm old_handler;
      Exninfo.iraise e
  with Invalid_argument _ -> (* This happens on Windows, as handling SIGALRM does not seem supported *)
    f x
