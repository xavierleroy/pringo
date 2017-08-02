/***********************************************************************/
/*                                                                     */
/*                      The Cryptokit library                          */
/*                                                                     */
/*            Xavier Leroy, projet Gallium, INRIA Paris                */
/*                                                                     */
/*  Copyright 2017 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License v2,      */
/*  with the special exception on linking described in file LICENSE.   */
/*                                                                     */
/***********************************************************************/

#include <stdint.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>

CAMLprim uint64_t pringo_mix64_unboxed(uint64_t z)
{
  z = (z ^ (z >> 33)) * 0xff51afd7ed558ccdULL;
  z = (z ^ (z >> 33)) * 0xc4ceb9fe1a85ec53ULL;
  return z ^ (z >> 33);
}

CAMLprim value pringo_mix64(value vz)
{
  return caml_copy_int64(pringo_mix64_unboxed(Int64_val(vz)));
}

CAMLprim uint32_t pringo_mix32_unboxed(uint64_t z)
{
  z = (z ^ (z >> 33)) * 0xff51afd7ed558ccdULL;
  z = (z ^ (z >> 33)) * 0xc4ceb9fe1a85ec53ULL;
  return (uint32_t)(z >> 32);
}

CAMLprim value pringo_mix32(value vz)
{
  return caml_copy_int32(pringo_mix32_unboxed(Int64_val(vz)));
}

static inline intnat pringo_mix30_unboxed(uint64_t z)
{
  z = (z ^ (z >> 33)) * 0xff51afd7ed558ccdULL;
  z = (z ^ (z >> 33)) * 0xc4ceb9fe1a85ec53ULL;
  return (intnat)(z >> 34);
}

CAMLprim value pringo_mix30(value vz)
{
  return Val_long(pringo_mix30_unboxed(Int64_val(vz)));
}

static inline uint64_t mix64variant13(uint64_t z)
{
  z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ULL;
  z = (z ^ (z >> 27)) * 0x94d049bb133111ebULL;
  return z ^ (z >> 31);
}

static inline int popcount64(uint64_t x)
{
  x = x - ((x >> 1) & 0x5555555555555555ULL);
  x = (x & 0x3333333333333333ULL) + ((x >> 2) & 0x3333333333333333ULL);
  x = (x + (x >> 4)) & 0x0f0f0f0f0f0f0f0fULL;
  return (x * 0x0101010101010101ULL) >> 56;
}

CAMLprim uint64_t pringo_mixGamma_unboxed(uint64_t z)
{
  z = mix64variant13(z) | 1ULL;
  if (popcount64(z ^ (z >> 1)) >= 24) z ^= 0xaaaaaaaaaaaaaaaaULL;
  return z; 
}

CAMLprim value pringo_mixGamma(value vz)
{
  return caml_copy_int64(pringo_mixGamma_unboxed(Int64_val(vz)));
}
