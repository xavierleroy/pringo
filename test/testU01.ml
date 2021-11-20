(* Interface with the Test01 library *)

type generator =
  | Gen01 of (unit -> float)
  | Genbits of (unit -> int32)

external smallcrush: string -> generator -> unit = "test01_smallcrush"
external crush: string -> generator -> unit = "test01_smallcrush"
external bigcrush: string -> generator -> unit = "test01_smallcrush"

(* Derived generators *)

let _Genbits8 (f: unit -> int) =
  Genbits (fun () ->
    let b1 = f () in
    let b2 = f () in
    let b3 = f () in
    let b4 = f () in
    let b12 = b1 lsl 8 + b2 in
    let b34 = b3 lsl 8 + b4 in
    Int32.(add (shift_left (of_int b12) 16) (of_int b34)))

let _Genseq (f: int32 Seq.t) =
  let rf = ref f in
  Genbits (fun () ->
    match !rf () with
    | Seq.Nil -> assert false
    | Seq.Cons(n, f') -> rf := f'; n)

let _Genbits64 (f: unit -> int64) =
  let rec seq () =
    let n = f () in
    let n1 = Int64.(to_int32 (shift_right n 32)) in
    let n2 = Int64.to_int32 n in
    Seq.Cons(n1, fun () -> Seq.Cons(n2, seq))
  in _Genseq seq

(* Test harness *)

let seed = ref "Jamais un coup de dés n'abolira le hasard. -Mallarmé"

module type TEST = sig
  val gen_bytes: unit -> generator
  val gen_int32: unit -> generator
  val gen_int64: unit -> generator
  val gen_blocks: int -> generator
  val treesplits: int -> generator
  val laggedsplit: int -> generator
  val split_l: unit -> generator
  val split_r: unit -> generator
  val split_a: unit -> generator
  val split_s: unit -> generator
end

module Maketest (R: PRNG.STATE) : TEST = struct

let init () = R.seed !seed

let gen_floats () =
  let g = init() in Gen01 (fun () -> R.uniform g)

let gen_bytes () =
  let g = init() in _Genbits8 (fun () -> R.byte g)

let gen_int32 () =
  let g = init() in Genbits (fun () -> R.bits32 g)

let gen_int64 () =
  let g = init() in _Genbits64 (fun () -> R.bits64 g)

let gen_blocks n =
  let g = init() in
  let b = Bytes.create n in
  let pos = ref n in
  _Genbits8 (fun () ->
    let p = !pos in
    if p < n
    then (pos := p + 1; Bytes.get_uint8 b p)
    else (R.bytes g b 0 n; pos := 1; Bytes.get_uint8 b 0))

let treesplits n =
  let rec mkgens n g =
    if n <= 0 then [g] else begin
      let g' = R.split g in
      mkgens (n-1) g @ mkgens (n-1) g'
    end in
  let gl = mkgens n (init()) in
  let rec seq l () =
    match l with
    | [] -> seq gl ()
    | g :: l' -> Seq.Cons(R.bits32 g, seq l')
  in _Genseq (seq gl)

let laggedsplit n =
  let rec seq g i () =
    if i < n
    then Seq.Cons(R.bits32 g, seq g (i + 1))
    else seq (R.split g) 0 ()
  in _Genseq (seq (init()) 0)

(* Split sequence "S_L".  Split, generate number with "left" generator, then
   recurse using "right" generator.

   This and the following "split sequences" are defined in sections 5.5 and 5.6
   of:

   Hans Georg Schaathun. 2015. Evaluation of splittable pseudo-random
   generators. Journal of Functional Programming, Vol. 25.
   https://doi.org/10.1017/S095679681500012X

     split
     /  \
   (1)   …

 *)

let split_l () =
  let rec spl g () =
    let gR = R.split g in (* now gL = g *)
    Seq.Cons(R.bits32 g (* 1 *), spl gR)
  in _Genseq (spl (init()))

(* Split sequence "S_R".  Split, generate number with "right" generator, then
   recurse using "left" generator.

     split
     /  \
    …   (1)

 *)

let split_r () =
  let rec spl g () =
    let gR = R.split g in (* now gL = g *)
    Seq.Cons(R.bits32 gR (* 1 *), spl g)
  in _Genseq (spl (init()))

(* Split sequence "S_A".  Split, generate number with "right" generator, then
   split again, generate number with "left" generator, and recurse with "right"
   generator.

     split
     /  \
   split (1)
   /  \
 (2)   …

 *)

let split_a () =
  let rec spl g () =
    let gR = R.split g in
    let n1 = R.bits32 gR in (* 1 *)
    let gLR = R.split g in
    let n2 = R.bits32 g in  (* 2 *)
    Seq.Cons(n1, fun () -> Seq.Cons(n2, spl gLR))
  in _Genseq (spl (init()))

(* Split sequence "S".

          split
         /     \
       …      split
             /     \
        split      split
        /  \       /  \
      (1)  (2)   (3)  (4)

 *)

let split_s () =
  let rec spl g () =
    let gR = R.split g in     (* now gL   = g *)
    let gRR = R.split gR in   (* now gRL  = gR *)
    let gRRR = R.split gRR in (* now gRRL = gRR *)
    let gRLR = R.split gR in  (* now gRLL = gR *)
    let n1 = R.bits32 gR (* 1 *) in
    let n2 = R.bits32 gRLR (* 2 *) in
    let n3 = R.bits32 gRR (* 3 *) in
    let n4 = R.bits32 gRRR (* 4 *) in
    Seq.Cons(n1, fun () -> Seq.Cons(n2, fun () -> Seq.Cons(n3, fun () -> Seq.Cons(n4, spl g))))
  in _Genseq (spl (init()))

end

module T1 = Maketest(PRNG.Splitmix.State)
module T2 = Maketest(PRNG.Chacha.State)
module T3 = Maketest(PRNG.LXM.State)

let dut = ref (module T1 : TEST)

let gen_bytes () =
  let module T = (val !dut) in T.gen_bytes()
let gen_int32 () =
  let module T = (val !dut) in T.gen_int32()
let gen_int64 () =
  let module T = (val !dut) in T.gen_int64()
let gen_blocks n =
  let module T = (val !dut) in T.gen_blocks n
let treesplits n =
  let module T = (val !dut) in T.treesplits n
let laggedsplit n =
  let module T = (val !dut) in T.laggedsplit n
let split_l () =
  let module T = (val !dut) in T.split_l()
let split_r () =
  let module T = (val !dut) in T.split_r()
let split_a () =
  let module T = (val !dut) in T.split_a()
let split_s () =
  let module T = (val !dut) in T.split_s()

let gen_config s =
  let l =
    match String.split_on_char '-' s with
    | "splitmix" :: l -> dut := (module T1 : TEST); l
    | "chacha" :: l -> dut := (module T2 : TEST); l
    | "lxm" :: l -> dut := (module T3 : TEST); l
    | _ -> raise (Arg.Bad ("unknown configuration " ^ s)) in
  match l with
  | ["seq8"] -> gen_bytes()
  | ["seq32"] -> gen_int32()
  | ["seq64"] -> gen_int64()
  | ["block"; n] -> gen_blocks (int_of_string n)
  | ["treesplit"; n] -> treesplits (int_of_string n)
  | ["laggedsplit"; n] -> laggedsplit (int_of_string n)
  | ["splitl"] -> split_l()
  | ["splitr"] -> split_r()
  | ["splita"] -> split_a()
  | ["splits"] -> split_s()
  | _ -> raise (Arg.Bad ("unknown configuration " ^ s))

let size = ref 1

let run_config config =
  match !size with
  | 1 -> smallcrush config (gen_config config)
  | 2 -> crush config (gen_config config)
  | 3 -> bigcrush config (gen_config config)
  | _ -> assert false

let _ =
  Arg.(parse [
    "-small", Unit (fun () -> size := 1),
       "Run the small crush tests";
    "-medium", Unit (fun () -> size := 2),
       "Run the normal crush tests";
    "-big", Unit (fun () -> size := 3),
       "Run the big crush tests";
  ]
  run_config
  "Usage: ./testU01 <options> [config]\nOptions are:")
