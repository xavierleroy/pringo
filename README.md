# PRINGO: Pseudo-Random, splIttable Number Generators for Ocaml

## Overview

This small library provides several pseudo-random number generators.  From a hopefully truly random seed provided by the user or obtained from the system, those generators provide infinite streams of numbers whose statistical properties resemble the properties of streams of random numbers. 

Compared with the pseudo-random number generators provided by the `Random` module from the OCaml standard library, the PRINGO generators have two extra features:
* They are **splittable**: they support a `split` operation that returns a new generator that is statistically independent from the current generator.  Both generators can be used in parallel, and can be further splitted, without introducing statistical bias.  This splitting operation is particularly useful to implement the lazy generation of pseudo-random infinite data structures such as functions or streams.
* In addition to a stateful, imperative interface resembling that of the `Random.State` standard library module, another, **purely functional interface** is provided.  In the functional interface, the current state of the PRNG appears as parameter but also as result of the number generation functions.  This interface can be used directly within a state monad.

Two PRNGS are provided:
* `Splitmix`, as described in the paper [_Fast Splittable Pseudorandom Number Generators_](http://gee.cs.oswego.edu/dl/papers/oopsla14.pdf) by Guy L. Steele Jr., Doug Lea, and Christine H. Flood, published in the proceedings of OOPSLA 2014.
* `Chacha`, which is based on the [Chacha20 stream cipher](https://cr.yp.to/chacha.html) by D. J. Bernstein.  Splitting is implemented by "jumping", i.e. randomly generating a 128-bit initial state for the new PRNG using the current PRNG.

Both PRNGs pass the [Dieharder](http://webhome.phy.duke.edu/~rgb/General/dieharder.php) statistical randomness test.

On 64-bit architectures, `Splitmix` is the faster of the two, and is even slightly faster than OCaml's `Random` standard library implementation.

On 32-bit architectures, `Chacha` is the faster of the two, but is still twice as slow as OCaml's `Random` module.

Splitmix has a 64-bit internal state, which is diversified by a 64-bit value called γ.  Splitting is achieved by changing both γ and the state, while other operations change only the state.  For a fixed γ, the period is 2<sup>64</sup>, but it is recommended to reseed after 2<sup>32</sup> numbers were generated.  From the initial seed, 64 bits worth of entropy are used.  Splitmix is not cryptographically strong: the internal state can be reconstructed from any two consecutive calls to `bits64`.  

Chacha is the Chacha 20-round stream cipher encrypting a sequence of zeros.  The internal state is a 128-bit counter.  Splitting is achieved by generating a pseudo-random initial value for the counter of the new PRNG.  The period of the PRNG is unclear.  It is recommended to generate no more than 2<sup>64</sup> bytes before reseeding.  Up to 32 bytes (256 bits) of the seed are used as the Chacha-20 key, although 16 bytes (128 bits) are probably enough.  Chacha-20 has been extensively cryptanalyzed and is widely used as a stream cipher, hence the Chacha PRNG is cryptographically strong to the best of the current knowledge.

## Installation and usage

The only dependencies are a recent enough version of OCaml (4.04.0 or up) and the findlib/ocamlfind library manager.

To build and install, just do `make` then become superuser if necessary and do `make install`.

To use the library, use `ocamlfind` and select the `pringo` package, e.g.
```
        ocamlfind opt -linkpkg -package pringo
```

Documentation is available as comments in the interface `PRNG.mli`.

## Copyright and license

Copyright Inria.  License LGPL v2 with special exception for static linking.

