# PRINGO: Pseudo-Random, splIttable Number Generators for Ocaml

## Overview

This small library provides several pseudo-random number generators.  From a hopefully truly random seed provided by the user or obtained from the system, those generators provide infinite streams of numbers whose statistical properties resemble the properties of streams of random numbers. 

Compared with the pseudo-random number generators provided by the `Random` module from the OCaml standard library, the PRINGO generators have two extra features:
* They are **splittable**: they support a `split` operation that returns a new generator that is statistically independent from the current generator.  Both generators can be used in parallel, and can be split further, without introducing statistical bias.  This splitting operation is particularly useful to implement the lazy generation of pseudo-random infinite data structures such as functions or streams.
* In addition to a stateful, imperative interface resembling that of the `Random.State` standard library module, another, **purely functional interface** is provided.  In the functional interface, the current state of the PRNG appears as parameter but also as result of the number generation functions.  This interface can be used directly within a state monad.

Three PRNGS are provided:
* `Splitmix`, as described in the paper [_Fast Splittable Pseudorandom Number Generators_](http://gee.cs.oswego.edu/dl/papers/oopsla14.pdf) by Guy L. Steele Jr., Doug Lea, and Christine H. Flood, published in the proceedings of OOPSLA 2014.
* `Chacha`, which is based on the [Chacha stream cipher](https://cr.yp.to/chacha.html) by D. J. Bernstein. Splitting is implemented by pseudorandomly generating a 128-bit initial state for the new PRNG using the current PRNG.
* `LXM`, as described in the paper [LXM: Better Splittable Pseudorandom Number Generators (and Almost as Fast)](https://doi.org/10.1145/3485525) by Guy L. Steele Jr. and Sebastiano Vigna, published in the proceedings of OOPSLA 2021.  We use the L64X128 variant.

All PRNGs pass the [TestU01](http://simul.iro.umontreal.ca/testu01/tu01.html) and [Dieharder](http://webhome.phy.duke.edu/~rgb/General/dieharder.php) statistical randomness tests.

On 64-bit architectures, `Splitmix` is the fastest, closely followed by LXM.  Both are slightly faster than OCaml's `Random` standard library implementation.

On 32-bit architectures, `Chacha` is the faster of the three, a bit slower than OCaml's `Random` module.

Splitmix has a 64-bit internal state, which is diversified by a 64-bit value called γ.  Splitting is achieved by changing both γ and the state, while other operations change only the state.  For a fixed γ, the period is 2<sup>64</sup>, but it is recommended to reseed after 2<sup>32</sup> numbers were generated.  From the initial seed, 64 bits worth of entropy are used.  Splitmix is not cryptographically strong: the internal state can be reconstructed from any two consecutive calls to `bits64`.  

LXM, or more precisely the L64X128 variant used in PRINGO, has a 192-bit internal state plus a 64-bit value (the additive parameter) for diversification.  Seeding and splitting change both the state and the additive parameter, while other operations change only the state.  For a fixed additive parameter, the period is 2<sup>192</sup>-2<sup>64</sup>.  Reseeding should not be necessary in practice.  From the initial seed, up to 32 bytes (256 bits) are used to initialize the state and the additive parameter.  Like Splitmix, LXM is not cryptographically strong.

Chacha is the 8-round Chacha stream cipher encrypting a sequence of zeros.  The internal state is a 128-bit counter.  Splitting is achieved by generating a pseudo-random initial value for the counter of the new PRNG.  The period of the PRNG is unclear.  It is recommended to generate no more than 2<sup>64</sup> bytes before reseeding.  Up to 32 bytes (256 bits) of the seed are used as the Chacha key, although 16 bytes (128 bits) are probably enough.  The PRNG is probably cryptographically strong, even though it uses the 8-round variant of Chacha, which has a lower security margin than the 20-round variant widely used as a stream cipher.

## Installation and usage

The only dependencies are a recent enough version of OCaml (4.05.0 or up) and the findlib/ocamlfind library manager.

To build and install, just do `make` then become superuser if necessary and do `make install`.

To use the library, use `ocamlfind` and select the `pringo` package, e.g.
```
        ocamlfind opt -linkpkg -package pringo
```

Documentation is available [online](https://xavierleroy.org/pringo/PRNG.html)
and as comments in the interface `PRNG.mli`.

## Testing

The `testu01` OPAM package must be installed.  
- `make -jN smalltest` runs the "small crush" battery of tests.  It takes a few minutes with N=8.
- `make -jN fulltest` runs the "crush", "rabbit", and "alphabit" batteries of tests.  It takes a few hours with N=8.
- `make -jN hugetest` runs the "big crush" battery.  It takes on the order of one full day with N=8.

## Copyright and license

Copyright Inria.  License LGPL v2 with special exception for static linking.
