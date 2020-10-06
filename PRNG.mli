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

(** This library provides pseudo-random number generators (PRNGs) comparable
  to that of the Random module from OCaml's standard library, but with
  two extensions:
- Generators are "splittable": they support a [split] operation that
  returns a new generator that is statistically independent from the
  current generator.  Both generators can be used in parallel, and can
  be further splitted, without introducing statistical bias.  This
  splitting operation is particularly useful to implement the lazy
  generation of pseudo-random infinite data structures such as
  functions or streams.
- In addition to a stateful, imperative interface resembling that of
  the [Random.State] standard library module, another, purely
  functional interface is provided.  In the functional interface, the
  current state of the PRNG appears as parameter but also as result of
  the number generation functions.  This interface can be used
  directly within a state monad.
*)

(** {2 The stateful interface} *)

module type STATE = sig
  type t
    (** The type of generators *)

(** {3 Creating and seeding PRNGs} *)

  val seed: string -> t
    (** Initialize a generator from the given seed.  The seed is given
        as a character string.  The length and randomness of the seed
        limit the total entropy of the generator.  For example, 64
        bits of entropy can be obtained by giving a seed consisting of
        8 cryptographically-strong random characters (as obtained
        e.g. by reading [/dev/random]. *)
  val make: int array -> t
    (** Initialize a generator from the given seed.  The seed is given
        as an array of integers.  *)
  val make_self_init: unit -> t
    (** Initialize a generator from a random seed obtained from the
        operating system.  Tries hard to provide at least 
        64 bits of entropy.  With high probability, successive calls
        to [make_self_init] return different PRNGs with different seeds. *)
        
(** {3 Generating pseudo-random data} *)

  val bool: t -> bool
  val bit: t -> bool
    (** Return a Boolean value in [false,true] with 0.5 probability each. *)

  val float: t -> float -> float
    (** [float g x] returns a floating-point number evenly distributed
        between 0.0 and [x].  If [x] is negative, negative numbers 
        between [x] and 0.0 are returned. *)

  val byte: t -> int
  val bits8: t -> int
    (** Return an 8-bit integer evenly distributed between 0 and 255. *)

  val bits: t -> int
  val bits30: t -> int
    (** Return a 30-bit integer evenly distributed between 0 and 2{^30}-1
        (that is, 1073741823, or 0x3FFFFFFF). *)

  val int: t -> int -> int
    (** [int g n] returns an integer evenly distributed between 0 included
        and [n] excluded.  Hence there are [n] possible return values
        with probability [1/n] each.  [n] must be greater than 0 and
        no greater than 2{^30}-1. *)

  val bits32: t -> int32
    (** Return a 32-bit integer evenly distributed between {!Int32.min_int}
        and {!Int32.max_int}.  *)

  val int32: t -> int32 -> int32
    (** [int32 g n] returns a 32-bit integer evenly distributed between
        0 included and [n] excluded.  [n] must be strictly positive.

        Note that [int32 Int32.max_int] produces numbers between 0 and
        [Int32.max_int] excluded.  To produce numbers between 0 and
        [Int32.max_int] included, use
        [Int32.logand (bits32 g) Int64.max_int]. *)

  val bits64: t -> int64
    (** Return a 64-bit integer evenly distributed between {!Int64.min_int}
        and {!Int64.max_int}.  *)

  val int64: t -> int64 -> int64
    (** [int64 g n] returns a 64-bit integer evenly distributed between
        0 included and [n] excluded.  [n] must be strictly positive.

        Note that [int64 Int64.max_int] produces numbers between 0 and
        [Int64.max_int] excluded.  To produce numbers between 0 and
        [Int64.max_int] included, use
        [Int64.logand (bits64 g) Int64.max_int]. *)

  val nativebits: t -> nativeint
    (** [nativebits g] returns a platform-native integer (32 or 64
        bits) evenly distributed between {!Nativeint.min_int} and
        {!Nativeint.max_int}.  *)

  val nativeint: t -> nativeint -> nativeint
    (** [nativeint g n] returns a platform-native integer between
        0 included and [n] included.  [n] must be strictly positive. *)

  val char: t -> char
    (** Return a character evenly distributed among ['\000' ... '\255']. *)

  val bytes: t -> bytes -> int -> int -> unit
    (** [bytes g b ofs len] produces [len] bytes of pseudo-random data
      and stores them in byte sequence [b] at offsets [ofs] to [ofs+len-1].

      Raise [Invalid_argument] if [len < 0] or [ofs] and [len] do not
      designate a valid range of [b]. *)

(** {3 Splitting and copying} *)

  val split: t -> t
    (** [split g] returns a fresh generator [g'] that is statistically
        independent from the current generator [g].  The two generators
        [g] and [g'] can be used in parallel and will produce independent
        pseudo-random data.  Each generator [g] and [g'] can be splitted
        again in the future. *)

  val copy: t -> t
    (** [copy g] returns a generator [g'] that has the same state as [g].
        The two generators [g] and [g'] produce the same data. *)

(** {3 Reseeding} *)

  val reseed: t -> string -> unit
    (** [reseed g s] reinitializes the generator [g] with fresh seed data
        from string [s].  This is like [seed s] except that the existing
        generator [g] is seeded, instead of a new generator being returned.
        It is good practice to reseed a PRNG after a certain quantity
        of pseudo-random data has been produced from it: typically
        2{^32} numbers for the {!PRNG.Splitmix} generator and
        2{^64} bytes for then {!PRNG.Chacha} generator. *)
  val remake: t -> int array -> unit
    (** [remake g a] reinitializes the generator [g] with fresh seed data
        from array [a].  This is like [reseed] except that the seed is
        given as an array of integers. *)

end

(** {2} The purely-functional, monadic interface *)

(** In this alternate interface, number-generating functions do not
  update the current state of the generator in-place.  Instead, they
  return the updated generator as a second result.  It is the
  programmer's responsibility to correctly thread the generators
  through the program, typically by using a state monad.

  All operations of the [STATE] interface are provided except
  [bytes] (too imperative) and [copy], [reseed] and [remake] (pointless). *)

module type PURE = sig
  type t

(** {3 Creating and seeding PRNGs} *)

  val seed: string -> t
  val make: int array -> t
  val make_self_init: unit -> t

(** {3 Generating pseudo-random data} *)

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

(** {3 Splitting} *)

  val split: t -> t * t
end

(** {2 The Splitmix implementation} *)

module Splitmix: sig
  module State: STATE
  module Pure: PURE
end
  (** This is an implementation of the [STATE] and [PURE] interfaces
      based on the Splitmix design by Guy L. Steele Jr., Doug Lea, and
      Christine H. Flood.  

      For seeding, 64 bits of entropy is recommended.  Seeds of 8
      characters or less are used as a 64-bit integer.  Longer seeds
      are hashed using {!Digest.string} before being used.

      Reseeding is recommended after 2{^32} numbers have been generated. *)

(** {2 The Chacha-20 implementation} *)

module Chacha: sig
  module State: STATE
  module Pure: PURE
end

  (** This is an implementation of the [STATE] and [PURE] interfaces
      based on the Chacha 20 stream cipher by D. J. Bernstein.

      For seeding, 128 bits of entropy is recommended.  Seeds of up
      to 32 characters are used as keys to the Chacha 20 cipher.
      Characters beyond the first 32 are ignored.

      Reseeding is recommended after 2{^64} numbers have been generated. *)

