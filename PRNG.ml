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

(** This is an implementation of the Splitmix PRNG, from:
      Guy L. Steele Jr., Doug Lea, Christine H. Flood
      "Fast Splittable Pseudorandom Number Generators"
      OOPSLA 2014.
    Two interfaces are provided: stateful and monadic. *)

(** The seeders *)

(* The seeder from OCaml's runtime system *)
external sys_random_seed: unit -> int array = "caml_sys_random_seed"

(* A better seeder for machines that support /dev/urandom *)
let dev_urandom_seed len =
  match open_in_bin "/dev/urandom" with
  | exception Sys_error _ -> None
  | ic ->
      match really_input_string ic len with
      | exception (End_of_file | Sys_error _) -> close_in ic; None
      | s -> close_in ic; Some s

(** Byte array manipulation *)

external getbyte : bytes -> int -> int = "%bytes_safe_get"
external setbyte : bytes -> int -> int -> unit = "%bytes_safe_set"

(* Notations for int64 operations  *)

let (<<) = Int64.shift_left
let (>>) = Int64.shift_right_logical
let (^^) = Int64.logxor
let (&&&) = Int64.logand
let (|||) = Int64.logor
let ( ** ) = Int64.mul
let (++) = Int64.add
let (--) = Int64.sub

(* The core mixing functions.  Could be defined in OCaml, and would run
  quite fast on 64-bit platforms, but are too slow on 32-bit platforms. *)

external mix64: int64 -> int64 = "pringo_mix64" "pringo_mix64_unboxed"
  [@@unboxed] [@@noalloc]
external mix32: int64 -> int32 = "pringo_mix32" "pringo_mix32_unboxed"
  [@@unboxed] [@@noalloc]
external mix30: int64 -> int = "pringo_mix30"
external mixGamma: int64 -> int64 = "pringo_mixGamma" "pringo_mixGamma_unboxed"
  [@@unboxed] [@@noalloc]

(** Helpers for initialization *)

let golden_gamma = 0x9e3779b97f4a7c15L

let int64_of_seed s =
  let s = if String.length s <= 8 then s else Digest.string s in
  let rec extract i accu =
    if i < 0 then accu else
      extract (i-1)
              ((accu << 8) ++ Int64.of_int (Char.code (String.get s i))) in
  extract (min 7 (String.length s - 1)) 0L

let mix_init accu n =
  mix64 (accu ++ Int64.of_int (n land 0x3FFFFFFF))

(** The stateful interface *)

module State = struct

type t = { mutable seed: int64; gamma: int64; }

let seed s =
  { seed = int64_of_seed s; gamma = golden_gamma }

let make s =
  { seed = Array.fold_left mix_init 0L s; gamma = golden_gamma }

let make_self_init () =
  match dev_urandom_seed 8 with
  | Some s -> seed s
  | None   -> make (sys_random_seed())

let [@inline] nextseed t =
  let z = t.seed ++ t.gamma in t.seed <- z; z

let bit g = mix30 (nextseed g) land 0x1 = 1
let bool = bit

let bits8 g = mix30 (nextseed g) land 0xFF
let byte = bits8
let char g = Char.chr (bits8 g)

let bits30 g = mix30 (nextseed g)
let bits = bits30

let bits32 g = mix32 (nextseed g)

let bits64 g = mix64 (nextseed g)

let rec intaux g n =
  let r = bits30 g in
  let v = r mod n in
  if r - v > 0x3FFFFFFF - n + 1 then intaux g n else v

let int g bound =
  if bound > 0x3FFFFFFF || bound <= 0
  then invalid_arg "PRNG.State.int"
  else intaux g bound

let rec int32aux g n =
  let r = Int32.logand (bits32 g) 0x7FFF_FFFFl in
  let v = Int32.rem r n in
  if Int32.sub r v > Int32.(add (sub max_int n) 1l)
  then int32aux g n
  else v

let int32 g bound =
  if bound <= 0l
  then invalid_arg "PRNG.State.int32"
  else int32aux g bound

let rec int64aux g n =
  let r = Int64.logand (bits64 g) 0x7FFF_FFFF_FFFF_FFFFL in
  let v = Int64.rem r n in
  if Int64.sub r v > Int64.(add (sub max_int n) 1L)
  then int64aux g n
  else v

let int64 g bound =
  if bound <= 0L
  then invalid_arg "PRNG.State.int64"
  else int64aux g bound

let nativeint =
  if Nativeint.size = 32
  then fun g bound -> Nativeint.of_int32 (int32 g (Nativeint.to_int32 bound))
  else fun g bound -> Int64.to_nativeint (int64 g (Int64.of_nativeint bound))

let float g bound =
  (Int64.(to_float (shift_right_logical (bits64 g) 11)) *. 0x1.p-53) *. bound

let bytes g dst ofs len =
  if ofs < 0 || len < 0 || ofs > Bytes.length dst - len then
    invalid_arg "PRNG.State.bytes"
  else begin
    let rec fill ofs len =
      let n = bits64 g in
      setbyte dst ofs (Int64.to_int n);
      if len > 1 then setbyte dst (ofs+1) (Int64.to_int (n >> 8));
      if len > 2 then setbyte dst (ofs+2) (Int64.to_int (n >> 16));
      if len > 3 then setbyte dst (ofs+3) (Int64.to_int (n >> 24));
      if len > 4 then setbyte dst (ofs+4) (Int64.to_int (n >> 32));
      if len > 5 then setbyte dst (ofs+5) (Int64.to_int (n >> 40));
      if len > 6 then setbyte dst (ofs+6) (Int64.to_int (n >> 48));
      if len > 7 then setbyte dst (ofs+7) (Int64.to_int (n >> 56));
      if len > 8 then fill (ofs + 8) (len - 8)
    in if len > 0 then fill ofs len
  end

let split g =
  let n1 = nextseed g in
  let n2 = nextseed g in
  { seed = mix64 n1; gamma = mixGamma n2 }

let copy g = { seed = g.seed; gamma = g.gamma }

let reseed g s =
  g.seed <- int64_of_seed s
  (* let's keep the original gamma, why not? *)

let remake g s =
  g.seed <- Array.fold_left mix_init 0L s
  (* let's keep the original gamma, why not? *)

end

(** The pure interface *)

module Pure = struct

type t = { seed: int64; gamma: int64; }

let seed s =
  { seed = int64_of_seed s; gamma = golden_gamma }

let make s =
  { seed = Array.fold_left mix_init 0L s; gamma = golden_gamma }

let make_self_init () =
  match dev_urandom_seed 8 with
  | Some s -> seed s
  | None   -> make (sys_random_seed())

let [@inline] next g = { seed = g.seed ++ g.gamma; gamma = g.gamma }

let bit g = 
  let g = next g in ((mix30 g.seed land 0x1 = 1), g)
let bool = bit

let bits8 g =
  let g = next g in (mix30 g.seed land 0xFF, g)
let byte = bits8
let char g =
  let g = next g in (Char.chr (mix30 g.seed land 0xFF), g)

let bits30 g =
  let g = next g in (mix30 g.seed, g)
let bits = bits30

let bits32 g =
  let g = next g in (mix32 g.seed, g)

let bits64 g =
  let g = next g in (mix64 g.seed, g)

let rec intaux n g =
  let (r, g') = bits30 g in
  let v = r mod n in
  if r - v > 0x3FFFFFFF - n + 1 then intaux n g' else (v, g')

let int bound g =
  if bound > 0x3FFFFFFF || bound <= 0
  then invalid_arg "PRNG.Pure.int"
  else intaux bound g

let rec int32aux n g =
  let (r, g') = bits32 g in
  let r = Int32.logand r 0x7FFF_FFFFl in
  let v = Int32.rem r n in
  if Int32.sub r v > Int32.(add (sub max_int n) 1l)
  then int32aux n g'
  else (v, g')

let int32 bound g =
  if bound <= 0l
  then invalid_arg "PRNG.Pure.int32"
  else int32aux bound g

let rec int64aux n g =
  let (r, g') = bits64 g in
  let r = Int64.logand r 0x7FFF_FFFF_FFFF_FFFFL in
  let v = Int64.rem r n in
  if Int64.sub r v > Int64.(add (sub max_int n) 1L)
  then int64aux n g'
  else (v, g')

let int64 bound g =
  if bound <= 0L
  then invalid_arg "PRNG.Pure.int64"
  else int64aux bound g

let nativeint =
  if Nativeint.size = 32
  then begin
    fun bound g ->
      let (r, g') = int32 (Nativeint.to_int32 bound) g in
      (Nativeint.of_int32 r, g')
  end else begin
    fun bound g -> 
      let (r, g') = int64 (Int64.of_nativeint bound) g in
      (Int64.to_nativeint r, g')
  end

let float bound g =
  let (b, g') = bits64 g in
  ((Int64.(to_float (shift_right_logical b 11)) *. 0x1.p-53) *. bound, g')

let split g =
  let g1 = next g in
  let g2 = next g1 in
  ({ seed = mix64 g1.seed; gamma = mixGamma g2.seed }, g2)

end
