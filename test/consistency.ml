(***********************************************************************)
(*                                                                     *)
(*                      The PRINGO library                             *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Paris                *)
(*                                                                     *)
(*  Copyright 2017 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License v2,      *)
(*  with the special exception on linking described in file LICENSE.   *)
(*                                                                     *)
(***********************************************************************)

(* Check that the Pure and the State implementations agree *)

open Printf
open PRNG

module Checker(X: sig module State: STATE module Pure: PURE end) = struct

open X

(* This is a variant of the state monad that carries both a Pure PRNG
   (monadically) and a State PRNG (as an environment). *)

type 'a mon = State.t -> Pure.t -> 'a * Pure.t

let ret (x: 'a) : 'a mon = fun s p -> (x, p)

let (>>=) (m: 'a mon) (f: 'a -> 'b mon) : 'b mon =
  fun s p -> let (r1, p1) = m s p in f r1 s p1

let rec check1 ?eq ?(n = 1) msg (sop: State.t -> 'a)
                                (pop: Pure.t -> 'a * Pure.t) : 'a mon =
  fun s p ->
    let rs = sop s in
    let (rp, p') = pop p in
    if not (match eq with None -> rs = rp | Some f -> f rs rp) then begin
      printf "%s (%d): results differ\n" msg n;
      exit 2
    end;
    if n <= 1 then (rp, p') else check1 ?eq ~n:(n-1) msg sop pop s p'

let rec check2 ?eq ?(n = 1) msg (sop: State.t -> 'a -> 'b)
                   (pop: 'a -> Pure.t -> 'b * Pure.t)
                   (arg: 'a) : 'b mon =
  fun s p ->
    let rs = sop s arg in
    let (rp, p') = pop arg p in
    if not (match eq with None -> rs = rp | Some f -> f rs rp) then begin
      printf "%s (%d): results differ\n" msg n;
      exit 2
    end;
    if n <= 1 then (rp, p') else check2 ?eq ~n:(n-1) msg sop pop arg s p'

let lift (sop: State.t -> 'a) (pop: Pure.t -> 'b * Pure.t) : ('a * 'b) mon =
  fun s p ->
    let rs = sop s in
    let (rp, p') = pop p in
    ((rs, rp), p')

(* Sequential checks *)

let seq_checks =
  check1 ~n:10 "bit" State.bit Pure.bit >>= fun _ ->
  check1 ~n:10 "byte" State.byte Pure.byte >>= fun _ ->
  check1 ~n:10 "bits30" State.bits30 Pure.bits30 >>= fun _ ->
  check1 ~n:10 "bits32" State.bits32 Pure.bits32 >>= fun _ ->
  check1 ~n:10 "bits64" State.bits64 Pure.bits64 >>= fun _ ->
  check2 ~n:100 "int 42" State.int Pure.int 42 >>= fun _ ->
  check2 ~n:100 "int32 0xFFEE" State.int32 Pure.int32 0xFFEEl >>= fun _ ->
  check2 ~n:100 "int64 max_int" State.int64 Pure.int64 Int64.max_int >>= fun _ ->
  check2 ~n:100 "nativeint 0x123456" State.nativeint Pure.nativeint 0x123456n >>= fun _ ->
  check2 ~n:100 "float 1.0" State.float Pure.float 1.0 >>= fun _ ->
  lift State.split Pure.split

(* Splitting *)

let rec tree_checks n s p =
  if n > 0 then begin
    let ((new_s, new_p), final_p) = seq_checks s p in
    tree_checks (n-1) new_s new_p;
    tree_checks (n-1) s final_p
  end

(* Seeding *)

let full_checks_seed n seed =
  tree_checks n (State.seed seed) (Pure.seed seed)

let full_checks_make n seed =
  tree_checks n (State.make seed) (Pure.make seed)

end

module ChkSplitmix = Checker(PRNG.Splitmix)
module ChkChacha   = Checker(PRNG.Chacha)
module ChkLXM      = Checker(PRNG.LXM)

(* All together *)

let _ =
  printf "Splitmix, with seed...\n";
  ChkSplitmix.full_checks_seed 4 "Supercalifragiliciousexpialidolcius";
  printf "Splitmix, with make...\n";
  ChkSplitmix.full_checks_make 4 [|314159; 2718|];
  printf "Chacha, with seed...\n";
  ChkChacha.full_checks_seed 4 "Supercalifragiliciousexpialidolcius";
  printf "Chacha, with make...\n";
  ChkChacha.full_checks_make 4 [|314159; 2718|];
  printf "LXM, with seed...\n";
  ChkLXM.full_checks_seed 4 "Supercalifragiliciousexpialidolcius";
  printf "LXM, with make...\n";
  ChkLXM.full_checks_make 4 [|314159; 2718|];
  printf "Test passed!\n"


