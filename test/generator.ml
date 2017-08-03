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

(* Testing PRNGs using the Dieharder statistical tests,
   http://webhome.phy.duke.edu/~rgb/General/dieharder.php *)

module R = PRNG.Chacha.State

let seed = ref "Jamais un coup de dés n'abolira le hasard. -Mallarmé"

let init () = R.seed !seed

let out8 g =
  output_byte stdout (R.byte g)

let out32 g =
  let n = R.bits32 g in
  output_byte stdout (Int32.to_int n);
  output_byte stdout (Int32.(to_int (shift_right_logical n 8)));
  output_byte stdout (Int32.(to_int (shift_right_logical n 16)));
  output_byte stdout (Int32.(to_int (shift_right_logical n 24)))

let out64 g =
  let n = R.bits64 g in
  output_byte stdout (Int64.to_int n);
  output_byte stdout (Int64.(to_int (shift_right_logical n 8)));
  output_byte stdout (Int64.(to_int (shift_right_logical n 16)));
  output_byte stdout (Int64.(to_int (shift_right_logical n 24)));
  output_byte stdout (Int64.(to_int (shift_right_logical n 32)));
  output_byte stdout (Int64.(to_int (shift_right_logical n 40)));
  output_byte stdout (Int64.(to_int (shift_right_logical n 48)));
  output_byte stdout (Int64.(to_int (shift_right_logical n 56)))

(* Generate byte per byte on stdout *)

let gen_bytes () =
  let g = init() in while true do out8 g done

(* Generate using 32-bit integers *)

let gen_int32 () =
  let g = init() in while true do out32 g done

(* Generate using 64-bit integers *)

let gen_int64 () =
  let g = init() in while true do out64 g done

(* Generate using N-byte blocks *)

let gen_blocks n =
  let g = init() in
  let b = Bytes.create n in
  while true do
    R.bytes g b 0 n;
    output stdout b 0 n
  done

(* Tree of splits.  Produce 2^N generators then interleave their outputs *)

let treesplits n =
  let rec mkgens n g =
    if n <= 0 then [g] else begin
      let g' = R.split g in
      mkgens (n-1) g @ mkgens (n-1) g'
    end in
  let gl = mkgens n (init()) in
  while true do List.iter out32 gl done

(* Lagged split.  Split, then produce N numbers with the original RNG, then
   switch to the splitted RNG and repeat.  *)

let laggedsplit n =
  let rec lag g =
    let g' = R.split g in
    for _i = 1 to n do out32 g done;
    lag g'
  in lag (init())

let _ =
  Arg.(parse [
    "-seed", Set_string seed,
      " <seed>  Choose a seed";
    "-seq8", Unit gen_bytes,
      " Produce 8-bit numbers sequentially";
    "-seq32", Unit gen_int32,
      " Produce 32-bit numbers sequentially";
    "-seq64", Unit gen_int64,
      " Produce 64-bit numbers sequentially";
    "-block", Int gen_blocks,
      " <n>  Produce n-byte blocks sequentially";
    "-treesplit", Int treesplits,
      " <n>  Perform 2^n splits then round robin between them";
    "-laggedsplit", Int laggedsplit,
      " <n>  Split, produce n 32-bit numbers, then use the split"
  ]
  (fun _ -> raise (Arg.Bad "Please select an option"))
  "Usage: ./testDH <options> | dieharder -a -g 200.\nOptions are:")

