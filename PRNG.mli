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

(** The stateful interface *)

module type STATE = sig
  type t
  val seed: string -> t
  val make: int array -> t
  val make_self_init: unit -> t

  val bit: t -> bool
  val bits8: t -> int
  val byte: t -> int
  val bits: t -> int
  val bits30: t -> int
  val bits32: t -> int32
  val bits64: t -> int64
  val bytes: t -> bytes -> int -> int -> unit

  val int: t -> int -> int
  val int32: t -> int32 -> int32
  val nativeint: t -> nativeint -> nativeint
  val int64: t -> int64 -> int64
  val float: t -> float -> float
  val bool: t -> bool
  val char: t -> char

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

  val bit: t -> bool * t
  val bits8: t -> int * t
  val byte: t -> int * t
  val bits: t -> int * t
  val bits30: t -> int * t
  val bits32: t -> int32 * t
  val bits64: t -> int64 * t

  val int: int -> t -> int * t
  val int32: int32 -> t -> int32 * t
  val nativeint: nativeint -> t -> nativeint * t
  val int64: int64 -> t -> int64 * t
  val float: float -> t -> float * t
  val bool: t -> bool * t
  val char: t -> char * t

  val split: t -> t * t
end

(** The Splitmix implementation *)

module Splitmix: sig
  module State: STATE
  module Pure: PURE
end

(** The Chacha-20 implementation *)

module Chacha: sig
  module State: STATE
  module Pure: PURE
end

