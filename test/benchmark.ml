(***********************************************************************)
(*                                                                     *)
(*                      The Cryptokit library                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Paris                *)
(*                                                                     *)
(*  Copyright 2017 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License v2,      *)
(*  with the special exception on linking described in file LICENSE.   *)
(*                                                                     *)
(***********************************************************************)

(* Speed test *)

open Printf
open PRNG

let time_fn msg fn =
  let start = Sys.time() in
  let res = fn() in
  let stop = Sys.time() in
  Printf.printf "%6.2f  %s\n" (stop -. start) msg;
  flush stdout;
  res

let repeat0 n fn () =
  for i = 1 to n do ignore (fn ()) done

let repeat1 n fn arg () =
  for i = 1 to n do ignore (fn arg) done

let repeat2 n fn arg1 arg2 () =
  for i = 1 to n do ignore (fn arg1 arg2) done

let repeat_state0 n fn () =
  let s = State.seed "xxx" in repeat1 n fn s ()

let repeat_state1 n fn arg () =
  let s = State.seed "xxx" in repeat2 n fn s arg ()

let repeat_pure0 n fn () =
  let rec rep n p =
    if n <= 0 then () else (let (_, p') = fn p in rep (n-1) p')
  in rep n (Pure.seed "xxx")

let repeat_pure1 n fn arg () =
  let rec rep n p =
    if n <= 0 then () else (let (_, p') = fn arg p in rep (n-1) p')
  in rep n (Pure.seed "xxx")

let n = 50_000_000

let _ =
  printf "Times are in seconds for %d repetitions, unless indicated.\n" n;
  printf "---- State interface ----\n";
  time_fn "bit" (repeat_state0 n State.bit);
  time_fn "bits" (repeat_state0 n State.bits);
  time_fn "int 0xFFEE" (repeat_state1 n State.int 0xFFEE);
  time_fn "int32 0xFFEEDD" (repeat_state1 n State.int32 0xFFEEDDl);
  time_fn "int64 0xFFEEDDCCAA" (repeat_state1 n State.int64 0xFFEEDDCCAAL);
  time_fn "float 1.0" (repeat_state1 n State.float 1.0);
  time_fn "split" (repeat_state0 n State.split);
  time_fn "seed 8" (repeat1 n State.seed "01234567");
  time_fn "seed 16 (/10)" (repeat1 (n/10) State.seed "0123456789ABCDEF");
  time_fn "make 3" (repeat1 n State.make [|1234;5678;90909|]);
  printf "---- Pure interface ----\n";
  time_fn "bit" (repeat_pure0 n Pure.bit);
  time_fn "bits" (repeat_pure0 n Pure.bits);
  time_fn "int 0xFFEE" (repeat_pure1 n Pure.int 0xFFEE);
  time_fn "int32 0xFFEEDD" (repeat_pure1 n Pure.int32 0xFFEEDDl);
  time_fn "int64 0xFFEEDDCCAA" (repeat_pure1 n Pure.int64 0xFFEEDDCCAAL);
  time_fn "float 1.0" (repeat_pure1 n Pure.float 1.0);
  time_fn "split" (repeat_pure0 n Pure.split);
  printf "---- OCaml's Random module ----\n";
  time_fn "bit" (repeat0 n Random.bool);
  time_fn "bits" (repeat0 n Random.bits);
  time_fn "int 0xFFEE" (repeat1 n Random.int 0xFFEE);
  time_fn "int32 0xFFEEDD" (repeat1 n Random.int32 0xFFEEDDl);
  time_fn "int64 0xFFEEDDCCAA" (repeat1 n Random.int64 0xFFEEDDCCAAL);
  time_fn "float 1.0" (repeat1 n Random.float 1.0);
  time_fn "make 3 (/1000)" (repeat1 (n/1000) Random.State.make [|1234;5678;90909|]);
  ()


