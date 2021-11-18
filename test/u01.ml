open TestU01

(* Creating generators *)

let gfloat name (f: unit -> float) =
  Unif01.create_extern_gen_01 name f

let gint32 name (f: unit -> int32) =
  Unif01.create_extern_gen_int32 name f

let gint8 name (f: unit -> int) =
  let g () =
    let b1 = f () in
    let b2 = f () in
    let b3 = f () in
    let b4 = f () in
    let b12 = b1 lsl 8 + b2 in
    let b34 = b3 lsl 8 + b4 in
    Int32.(add (shift_left (of_int b12) 16) (of_int b34))
  in gint32 name g

let gint32list name (f: unit -> int32 list) =
  let acc = ref [] in
  let rec g () =
    match !acc with
    | [] -> acc := f(); g()
    | h :: t -> acc := t; h
  in gint32 name g

let gint32seq name (s: int32 Seq.t) =
  let rs = ref s in
  let rec f () =
    match !rs () with
    | Seq.Nil -> assert false
    | Seq.Cons(n, s') -> rs := s'; n
  in gint32 name f

let gint64 name (f: unit -> int64) =
  let g () =
    let n = f() in
    [Int64.to_int32 n; Int64.(to_int32 (shift_right_logical n 32))]
  in gint32list name g

(* Test harness *)

let seed = ref "Jamais un coup de dés n'abolira le hasard. -Mallarmé"

module type TEST = sig
  val gen_floats: unit -> Unif01.gen
  val gen_bytes: unit -> Unif01.gen
  val gen_int32: unit -> Unif01.gen
  val gen_int64: unit -> Unif01.gen
  val gen_blocks: int -> Unif01.gen
  val treesplits: int -> Unif01.gen
  val laggedsplit: int -> Unif01.gen
  val split_l: unit -> Unif01.gen
  val split_r: unit -> Unif01.gen
  val split_a: unit -> Unif01.gen
  val split_s: unit -> Unif01.gen
end

module Maketest (R: PRNG.STATE) : TEST = struct

let init () = R.seed !seed

let gen_floats () =
  let g = init() in gfloat "float" (fun () -> R.uniform g)

let gen_bytes () =
  let g = init() in gint8 "byte" (fun () -> R.byte g)

let gen_int32 () =
  let g = init() in gint32 "int32" (fun () -> R.bits32 g)

let gen_int64 () =
  let g = init() in gint64 "int64" (fun () -> R.bits64 g)

let gen_blocks n =
  let g = init() in
  let b = Bytes.create n in
  let pos = ref n in
  let f () =
    let p = !pos in
    if p < n
    then (pos := p + 1; Bytes.get_uint8 b p)
    else (R.bytes g b 0 n; pos := 1; Bytes.get_uint8 b 0)
  in gint8 "blocks" f

let treesplits n =
  let rec mkgens n g =
    if n <= 0 then [g] else begin
      let g' = R.split g in
      mkgens (n-1) g @ mkgens (n-1) g'
    end in
  let gl = mkgens n (init()) in
  let f () = List.map R.bits32 gl
  in gint32list "treesplits" f

let laggedsplit n =
  let g = ref (init())
  and i = ref 0 in
  let rec f () =
    if !i < n
    then (incr i; R.bits32 !g)
    else (i := 0; g := R.split !g; f ())
  in gint32 "laggedsplit" f

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
  in gint32seq "split_l" (spl (init()))

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
  in gint32seq "split_r" (spl (init()))

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
  in gint32seq "split_a" (spl (init()))

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
  in gint32seq "split_s" (spl (init()))

end

module T1 = Maketest(PRNG.Splitmix.State)
module T2 = Maketest(PRNG.Chacha.State)
module T3 = Maketest(PRNG.LXM.State)

let dut = ref (module T1 : TEST)

let gen_floats () =
  let module T = (val !dut) in T.gen_floats()
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
  | ["float"] -> gen_floats()
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
  | 1 -> Bbattery.small_crush (gen_config config)
  | 2 -> Bbattery.crush (gen_config config)
  | 3 -> Bbattery.big_crush (gen_config config)
  | 4 -> Bbattery.rabbit (gen_config config) (2.0 ** 26.)
  | 5 -> Bbattery.alphabit (gen_config config) (2.0 ** 30.) 0 32
  | _ -> assert false

let _ =
  Arg.(parse [
    "-small", Unit (fun () -> size := 1),
       "Run the small crush tests";
    "-medium", Unit (fun () -> size := 2),
       "Run the normal crush tests";
    "-big", Unit (fun () -> size := 3),
       "Run the big crush tests";
    "-rabbit", Unit (fun () -> size := 4),
       "Run the Rabbit tests";
    "-alphabit", Unit (fun () -> size := 5),
       "Run the Alphabit tests";
  ]
  run_config
  "Usage: ./testU01 <options> [config]\nOptions are:")
