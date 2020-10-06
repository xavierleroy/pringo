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

(** The two interfaces *)

module type STATE = sig
  type t
  val seed: string -> t
  val make: int array -> t
  val make_self_init: unit -> t
  val bool: t -> bool
  val bit: t -> bool
  val float: t -> float -> float
  val byte: t -> int
  val bits8: t -> int
  val bits: t -> int
  val bits30: t -> int
  val int: t -> int -> int
  val bits32: t -> int32
  val int32: t -> int32 -> int32
  val bits64: t -> int64
  val int64: t -> int64 -> int64
  val nativebits: t -> nativeint
  val nativeint: t -> nativeint -> nativeint
  val char: t -> char
  val bytes: t -> bytes -> int -> int -> unit
  val split: t -> t
  val copy: t -> t
  val reseed: t -> string -> unit
  val remake: t -> int array -> unit
end

(** The purely-functional, monadic interface *)

module type PURE = sig
  type t
  val seed: string -> t
  val make: int array -> t
  val make_self_init: unit -> t
  val bool: t -> bool * t
  val bit: t -> bool * t
  val float: float -> t -> float * t
  val byte: t -> int * t
  val bits8: t -> int * t
  val int: int -> t -> int * t
  val bits: t -> int * t
  val bits30: t -> int * t
  val bits32: t -> int32 * t
  val int32: int32 -> t -> int32 * t
  val bits64: t -> int64 * t
  val int64: int64 -> t -> int64 * t
  val nativebits: t -> nativeint * t
  val nativeint: nativeint -> t -> nativeint * t
  val char: t -> char * t
  val split: t -> t * t
end

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
(* If we feel adventurous:
external getbyte : bytes -> int -> int = "%bytes_unsafe_get"
external setbyte : bytes -> int -> int -> unit = "%bytes_unsafe_set"
*)

(** Derived operations for the STATE interface *)

module StateDerived(X: sig
    type t
    val bits30: t -> int
    val bits32: t -> int32
    val bits64: t -> int64
    val errorprefix: string
  end) =
struct

let rec intaux g n =
  let r = X.bits30 g in
  let v = r mod n in
  if r - v > 0x3FFFFFFF - n + 1 then intaux g n else v

let int g bound =
  if bound > 0x3FFFFFFF || bound <= 0
  then invalid_arg (X.errorprefix ^ "int")
  else intaux g bound

let rec int32aux g n =
  let r = Int32.logand (X.bits32 g) 0x7FFF_FFFFl in
  let v = Int32.rem r n in
  if Int32.sub r v > Int32.(add (sub max_int n) 1l)
  then int32aux g n
  else v

let int32 g bound =
  if bound <= 0l
  then invalid_arg (X.errorprefix ^ "int32")
  else int32aux g bound

let rec int64aux g n =
  let r = Int64.logand (X.bits64 g) 0x7FFF_FFFF_FFFF_FFFFL in
  let v = Int64.rem r n in
  if Int64.sub r v > Int64.(add (sub max_int n) 1L)
  then int64aux g n
  else v

let int64 g bound =
  if bound <= 0L
  then invalid_arg (X.errorprefix ^ "int64")
  else int64aux g bound

let nativebits =
  if Nativeint.size = 32
  then fun g -> Nativeint.of_int32 (X.bits32 g)
  else fun g -> Int64.to_nativeint (X.bits64 g)

let nativeint =
  if Nativeint.size = 32
  then fun g bound -> Nativeint.of_int32 (int32 g (Nativeint.to_int32 bound))
  else fun g bound -> Int64.to_nativeint (int64 g (Int64.of_nativeint bound))

let float g bound =
  (Int64.(to_float (shift_right_logical (X.bits64 g) 11)) *. 0x1.p-53) *. bound

end

(** Derived operations for the PURE interface *)

module PureDerived(X: sig
    type t
    val bits30: t -> int * t
    val bits32: t -> int32 * t
    val bits64: t -> int64 * t
    val errorprefix: string
  end) =
struct

let rec intaux n g =
  let (r, g') = X.bits30 g in
  let v = r mod n in
  if r - v > 0x3FFFFFFF - n + 1 then intaux n g' else (v, g')

let int bound g =
  if bound > 0x3FFFFFFF || bound <= 0
  then invalid_arg (X.errorprefix ^ "int")
  else intaux bound g

let rec int32aux n g =
  let (r, g') = X.bits32 g in
  let r = Int32.logand r 0x7FFF_FFFFl in
  let v = Int32.rem r n in
  if Int32.sub r v > Int32.(add (sub max_int n) 1l)
  then int32aux n g'
  else (v, g')

let int32 bound g =
  if bound <= 0l
  then invalid_arg (X.errorprefix ^ "int32")
  else int32aux bound g

let rec int64aux n g =
  let (r, g') = X.bits64 g in
  let r = Int64.logand r 0x7FFF_FFFF_FFFF_FFFFL in
  let v = Int64.rem r n in
  if Int64.sub r v > Int64.(add (sub max_int n) 1L)
  then int64aux n g'
  else (v, g')

let int64 bound g =
  if bound <= 0L
  then invalid_arg (X.errorprefix ^ "int64")
  else int64aux bound g

let nativebits =
  if Nativeint.size = 32
  then fun g -> let (r, g') = X.bits32 g in (Nativeint.of_int32 r, g')
  else fun g -> let (r, g') = X.bits64 g in (Int64.to_nativeint r, g')

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
  let (b, g') = X.bits64 g in
  ((Int64.(to_float (shift_right_logical b 11)) *. 0x1.p-53) *. bound, g')

end

(** This is an implementation of the Splitmix PRNG, from:
      Guy L. Steele Jr., Doug Lea, Christine H. Flood
      "Fast Splittable Pseudorandom Number Generators"
      OOPSLA 2014.
    Two interfaces are provided: stateful and monadic. *)

module Splitmix = struct

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

include StateDerived(struct
  type nonrec t = t
  let bits30 = bits30
  let bits32 = bits32
  let bits64 = bits64
  let errorprefix = "PRNG.Splitmix.State."
end)

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

include PureDerived(struct
  type nonrec t = t
  let bits30 = bits30
  let bits32 = bits32
  let bits64 = bits64
  let errorprefix = "PRNG.Splitmix.Pure."
end)

let split g =
  let g1 = next g in
  let g2 = next g1 in
  ({ seed = mix64 g1.seed; gamma = mixGamma g2.seed }, g2)

end

end

(** The Chacha implementation *)

module Chacha = struct

type key
type state = bytes

external chacha_make_key: string -> key = "pringo_chacha_make_key"
external chacha_make_state: bytes -> state = "pringo_chacha_make_state"
external chacha_transform: key -> state -> unit = "pringo_chacha_transform"

(** Helpers for initialization *)

let empty_bytes = Bytes.make 16 '\000'

let mix_init a =
  let buf = Bytes.create (4 * Array.length a) in
  let storeint n i =
    setbyte buf i n;
    setbyte buf (i+1) (n lsl 8);
    setbyte buf (i+2) (n lsl 16);
    setbyte buf (i+3) (n lsl 24) in
  Array.iteri (fun i n -> storeint n (i * 4)) a;
  Digest.bytes buf

(** Helpers to build integers *)

let [@inline] make30 b0 b1 b2 b3 =
  b0 + (b1 lsl 8) + (b2 lsl 16) + ((b3 land 0x3F) lsl 24)

let [@inline] make32 b0 b1 b2 b3 =  
  Int32.(add (add (of_int b0)
                  (shift_left (of_int b1) 8))
             (add (shift_left (of_int b2) 16)
                  (shift_left (of_int b3) 24)))

let [@inline] make64 b0 b1 b2 b3 b4 b5 b6 b7 =
  Int64.(add (add (add (of_int b0)
                       (shift_left (of_int b1) 8))
                  (add (shift_left (of_int b2) 16)
                       (shift_left (of_int b3) 24)))
             (add (add (shift_left (of_int b4) 32)
                       (shift_left (of_int b5) 40))
                  (add (shift_left (of_int b6) 48)
                       (shift_left (of_int b7) 56))))

(** The stateful interface *)

module State = struct

type t = { mutable key: key; mutable st: state; mutable next: int }

let seed s =
  { key = chacha_make_key s; st = chacha_make_state empty_bytes; next = 64 }

let make s =
  { key = chacha_make_key (mix_init s); 
    st = chacha_make_state empty_bytes; next = 64 }

let make_self_init () =
  match dev_urandom_seed 16 with
  | Some s -> seed s
  | None   -> make (sys_random_seed())

(* Layout of the state:
      0...63  up to 64 bytes of already-generated pseudo-random data
     64...79  16 bytes for the counter and the nonce *)

let byte g =
  let i = g.next in
  if i <= 63 then begin
    g.next <- i + 1;
    getbyte g.st i
  end else begin
    chacha_transform g.key g.st;
    g.next <- 1;
    getbyte g.st 0
  end

let bits8 = byte
let char g = Char.chr (bits8 g)

let bit g = byte g land 0x1 = 1
let bool = bit

let bits30 g =
  let i = g.next in
  if i <= 60 then begin
    g.next <- i + 4;
    make30 (getbyte g.st i) (getbyte g.st (i+1))
           (getbyte g.st (i+2)) (getbyte g.st (i+3))
  end else begin
    let b0 = byte g in let b1 = byte g in
    let b2 = byte g in let b3 = byte g in
    make30 b0 b1 b2 b3
  end

let bits = bits30

let bits32 g =
  let i = g.next in
  if i <= 60 then begin
    g.next <- i + 4;
    make32 (getbyte g.st i) (getbyte g.st (i+1))
           (getbyte g.st (i+2)) (getbyte g.st (i+3))
  end else begin
    let b0 = byte g in let b1 = byte g in
    let b2 = byte g in let b3 = byte g in
    make32 b0 b1 b2 b3
  end

let bits64 g =
  let i = g.next in
  if i <= 56 then begin
    g.next <- i + 8;
    make64 (getbyte g.st i) (getbyte g.st (i+1))
           (getbyte g.st (i+2)) (getbyte g.st (i+3))
           (getbyte g.st (i+4)) (getbyte g.st (i+5))
           (getbyte g.st (i+6)) (getbyte g.st (i+7))
  end else begin
    let b0 = byte g in let b1 = byte g in
    let b2 = byte g in let b3 = byte g in
    let b4 = byte g in let b5 = byte g in
    let b6 = byte g in let b7 = byte g in
    make64 b0 b1 b2 b3 b4 b5 b6 b7
  end

include StateDerived(struct
  type nonrec t = t
  let bits30 = bits30
  let bits32 = bits32
  let bits64 = bits64
  let errorprefix = "PRNG.Chacha.State."
end)

let bytes g dst ofs len =
  if ofs < 0 || len < 0 || Bytes.length dst - len > ofs then
    invalid_arg "PRNG.Chacha.State.bytes";
  let rec fill ofs len =
    let i = g.next in
    let avail = 64 - i in
    if len <= avail then begin
      Bytes.blit g.st i dst ofs len;
      g.next <- i + len;
    end else begin
      let avail = 64 - i in
      Bytes.blit g.st avail dst ofs len;
      chacha_transform g.key g.st;
      g.next <- 0;
      fill (ofs + avail) (len - avail)
    end
  in fill ofs len

let split g =
  let k = Bytes.create 16 in
  bytes g k 0 16;
  { key = g.key; st = chacha_make_state k; next = 64 }

let copy g = { key = g.key; st = Bytes.copy g.st; next = g.next }

let reseed g s =
  g.key <- chacha_make_key s;
  g.st <- chacha_make_state empty_bytes;
  g.next <- 64

let remake g s =
  g.key <- chacha_make_key (mix_init s);
  g.st <- chacha_make_state empty_bytes;
  g.next <- 64

end

(** The pure interface *)

module Pure = struct

type t = { key: key; st: state; next: int }

let seed s =
  { key = chacha_make_key s; st = chacha_make_state empty_bytes; next = 64 }

let make s =
  { key = chacha_make_key (mix_init s); 
    st = chacha_make_state empty_bytes; next = 64 }

let make_self_init () =
  match dev_urandom_seed 16 with
  | Some s -> seed s
  | None   -> make (sys_random_seed())

let byte g =
  let i = g.next in
  if i <= 63 then begin
    (getbyte g.st i, {g with next = i + 1})
  end else begin
    let st' = Bytes.copy g.st in
    chacha_transform g.key st';
    (getbyte st' 0, {g with st = st'; next = 1})
  end

let bits8 = byte
let char g = let (n, g') = byte g in (Char.chr n, g')

let bit g = let (n, g') = byte g in (n land 0x1 = 1, g')
let bool = bit

let bits30 g =
  let i = g.next in
  if i <= 60 then begin
    (make30 (getbyte g.st i) (getbyte g.st (i+1))
            (getbyte g.st (i+2)) (getbyte g.st (i+3)),
     {g with next = i + 4})
  end else begin
    let (b0, g) = byte g in let (b1, g) = byte g in
    let (b2, g) = byte g in let (b3, g) = byte g in
    (make30 b0 b1 b2 b3, g)
  end

let bits = bits30

let bits32 g =
  let i = g.next in
  if i <= 60 then begin
    (make32 (getbyte g.st i) (getbyte g.st (i+1))
            (getbyte g.st (i+2)) (getbyte g.st (i+3)),
     {g with next = i + 4})
  end else begin
    let (b0, g) = byte g in let (b1, g) = byte g in
    let (b2, g) = byte g in let (b3, g) = byte g in
    (make32 b0 b1 b2 b3, g)
  end

let bits64 g =
  let i = g.next in
  if i <= 56 then begin
    (make64 (getbyte g.st i) (getbyte g.st (i+1))
            (getbyte g.st (i+2)) (getbyte g.st (i+3))
            (getbyte g.st (i+4)) (getbyte g.st (i+5))
            (getbyte g.st (i+6)) (getbyte g.st (i+7)),
     {g with next = i + 8})
  end else begin
    let (b0, g) = byte g in let (b1, g) = byte g in
    let (b2, g) = byte g in let (b3, g) = byte g in
    let (b4, g) = byte g in let (b5, g) = byte g in
    let (b6, g) = byte g in let (b7, g) = byte g in
    (make64 b0 b1 b2 b3 b4 b5 b6 b7, g)
  end

include PureDerived(struct
  type nonrec t = t
  let bits30 = bits30
  let bits32 = bits32
  let bits64 = bits64
  let errorprefix = "PRNG.Chacha.Pure."
end)

let bytes g dst ofs len =
  if ofs < 0 || len < 0 || Bytes.length dst - len > ofs then
    invalid_arg "PRNG.Chacha.Pure.bytes";
  let rec fill g ofs len =
    let i = g.next in
    let avail = 64 - i in
    if len <= avail then begin
      Bytes.blit g.st i dst ofs len;
      {g with next = i + len}
    end else begin
      let avail = 64 - i in
      Bytes.blit g.st avail dst ofs len;
      let st' = Bytes.copy g.st in
      chacha_transform g.key st';
      fill {g with st = st'; next = 0} (ofs + avail) (len - avail)
    end
  in fill g ofs len

let split g =
  let k = Bytes.create 16 in
  let g = bytes g k 0 16 in
  ( { key = g.key; st = chacha_make_state k; next = 64 }, g )

end

end
