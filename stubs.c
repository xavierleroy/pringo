/***********************************************************************/
/*                                                                     */
/*                      The PRINGO library                             */
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
#include <string.h>
#include <caml/alloc.h>
#include <caml/config.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

/* Mixing functions for Splitmix */

CAMLprim uint64_t pringo_mix64_unboxed(uint64_t z)
{
  /* This is "Stafford variant 13" as used in the JDK */
  z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ULL;
  z = (z ^ (z >> 27)) * 0x94d049bb133111ebULL;
  return z ^ (z >> 31);
}

CAMLprim value pringo_mix64(value vz)
{
  return caml_copy_int64(pringo_mix64_unboxed(Int64_val(vz)));
}

CAMLprim uint32_t pringo_mix32_unboxed(uint64_t z)
{
  /* This is "Stafford variant 4" as used in the JDK */
  z = (z ^ (z >> 33)) * 0x62a9d9ed799705f5L;
  z = (z ^ (z >> 28)) * 0xcb24d0a5c88c35b3L;
  return (uint32_t)(z >> 32);
}

CAMLprim value pringo_mix32(value vz)
{
  return caml_copy_int32(pringo_mix32_unboxed(Int64_val(vz)));
}

static inline intnat pringo_mix30_unboxed(uint64_t z)
{
  z = (z ^ (z >> 33)) * 0x62a9d9ed799705f5L;
  z = (z ^ (z >> 28)) * 0xcb24d0a5c88c35b3L;
  return (intnat)(z >> 34);
}

CAMLprim value pringo_mix30(value vz)
{
  return Val_long(pringo_mix30_unboxed(Int64_val(vz)));
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
  int n;
  z = (z ^ (z >> 33)) * 0xff51afd7ed558ccdULL; /* MurmurHash3 mix constants */
  z = (z ^ (z >> 33)) * 0xc4ceb9fe1a85ec53ULL;
  z = (z ^ (z >> 33)) | 1;
  n = popcount64(z ^ (z >> 1));
  return n < 24 ? z ^= 0xaaaaaaaaaaaaaaaaULL : z;
}

CAMLprim value pringo_mixGamma(value vz)
{
  return caml_copy_int64(pringo_mixGamma_unboxed(Int64_val(vz)));
}

/* Primitives for the Chacha20 cipher */

struct chacha20_key {
  uint32_t key[12];
};

struct chacha20_state {
  uint8_t output[64];
  uint32_t ctr[2];
  uint32_t nonce[2];
};

static void chacha20_init_key(struct chacha20_key * k,
                              uint8_t * key, size_t key_len);
static void chacha20_block(const struct chacha20_key * key,
                           struct chacha20_state * st);

#ifndef Data_abstract_val
#define Data_abstract_val Op_val
#endif

#define Key_val(v) ((struct chacha20_key *) Data_abstract_val(v))
#define State_val(v) ((struct chacha20_state *) String_val(v))
#define Wsizeof(ty) ((sizeof(ty) + sizeof(value) - 1) / sizeof(value))

static inline void U32TO8_LITTLE(uint8_t * dst, uint32_t val)
{
#ifdef ARCH_BIG_ENDIAN
  dst[0] = val;
  dst[1] = val >> 8;
  dst[2] = val >> 16;
  dst[3] = val >> 24;
#else
  *((uint32_t *) dst) = val;
#endif
}

static inline uint32_t U8TO32_LITTLE(const uint8_t * src)
{
#ifdef ARCH_BIG_ENDIAN
  return (uint32_t) src[0]
       | ((uint32_t) src[1] << 8)
       | ((uint32_t) src[2] << 16)
       | ((uint32_t) src[3] << 24);
#else
  return *((const uint32_t *) src);
#endif
}

CAMLprim value pringo_chacha_make_key(value vkey)
{
  mlsize_t keylen;
  uint8_t keybytes[32];
  value res;

  keylen = caml_string_length(vkey);
  if (keylen > 32) keylen = 32;
  memcpy(keybytes, String_val(vkey), keylen);
  memset(keybytes + keylen, 0, 32 - keylen);
  res = caml_alloc_small(Wsizeof(struct chacha20_key), Abstract_tag);
  chacha20_init_key(Key_val(res), keybytes, keylen <= 16 ? 16 : 32);
  memset(keybytes, 0, 32);      /* just in case key is sensitive */
  return res;
}

CAMLprim value pringo_chacha_make_state(value vstate)
{
  CAMLparam1(vstate);
  value res = caml_alloc_string(sizeof(struct chacha20_state));
  State_val(res)->ctr[0] = U8TO32_LITTLE(&Byte_u(vstate, 0));
  State_val(res)->ctr[1] = U8TO32_LITTLE(&Byte_u(vstate, 4));
  State_val(res)->nonce[0] = U8TO32_LITTLE(&Byte_u(vstate, 8));
  State_val(res)->nonce[1] = U8TO32_LITTLE(&Byte_u(vstate, 12));
  CAMLreturn(res);
}

CAMLprim value pringo_chacha_transform(value vkey, value vstate)
{
  chacha20_block(Key_val(vkey), State_val(vstate));
  return Val_unit;
}

/* Based on D. J. Bernstein's chacha-regs.c version 200801118,
  https://cr.yp.to/streamciphers/timings/estreambench/submissions/salsa20/chacha8/regs/chacha.c
  The initial code is in the public domain */

#define ROTATE(v,c) ((v) << (c) | (v) >> (32 - (c)))
#define XOR(v,w) ((v) ^ (w))
#define PLUS(v,w) ((v) + (w))
#define PLUSONE(v) ((v) + 1)

#define QUARTERROUND(a,b,c,d) \
  a = PLUS(a,b); d = ROTATE(XOR(d,a),16); \
  c = PLUS(c,d); b = ROTATE(XOR(b,c),12); \
  a = PLUS(a,b); d = ROTATE(XOR(d,a), 8); \
  c = PLUS(c,d); b = ROTATE(XOR(b,c), 7);

static void chacha20_block(const struct chacha20_key * k,
                           struct chacha20_state * s)
{
  uint32_t x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15;
  int i;

  x0 = k->key[0];
  x1 = k->key[1];
  x2 = k->key[2];
  x3 = k->key[3];
  x4 = k->key[4];
  x5 = k->key[5];
  x6 = k->key[6];
  x7 = k->key[7];
  x8 = k->key[8];
  x9 = k->key[9];
  x10 = k->key[10];
  x11 = k->key[11];
  x12 = s->ctr[0];
  x13 = s->ctr[1];
  x14 = s->nonce[0];
  x15 = s->nonce[1];
  for (i = 8; i > 0; i -= 2) {
    QUARTERROUND( x0, x4, x8,x12)
    QUARTERROUND( x1, x5, x9,x13)
    QUARTERROUND( x2, x6,x10,x14)
    QUARTERROUND( x3, x7,x11,x15)
    QUARTERROUND( x0, x5,x10,x15)
    QUARTERROUND( x1, x6,x11,x12)
    QUARTERROUND( x2, x7, x8,x13)
    QUARTERROUND( x3, x4, x9,x14)
  }
  x0 = PLUS(x0,k->key[0]);
  x1 = PLUS(x1,k->key[1]);
  x2 = PLUS(x2,k->key[2]);
  x3 = PLUS(x3,k->key[3]);
  x4 = PLUS(x4,k->key[4]);
  x5 = PLUS(x5,k->key[5]);
  x6 = PLUS(x6,k->key[6]);
  x7 = PLUS(x7,k->key[7]);
  x8 = PLUS(x8,k->key[8]);
  x9 = PLUS(x9,k->key[9]);
  x10 = PLUS(x10,k->key[10]);
  x11 = PLUS(x11,k->key[11]);
  x12 = PLUS(x12,s->ctr[0]);
  x13 = PLUS(x13,s->ctr[1]);
  x14 = PLUS(x14,s->nonce[0]);
  x15 = PLUS(x15,s->nonce[1]);
  U32TO8_LITTLE(s->output + 0,x0);
  U32TO8_LITTLE(s->output + 4,x1);
  U32TO8_LITTLE(s->output + 8,x2);
  U32TO8_LITTLE(s->output + 12,x3);
  U32TO8_LITTLE(s->output + 16,x4);
  U32TO8_LITTLE(s->output + 20,x5);
  U32TO8_LITTLE(s->output + 24,x6);
  U32TO8_LITTLE(s->output + 28,x7);
  U32TO8_LITTLE(s->output + 32,x8);
  U32TO8_LITTLE(s->output + 36,x9);
  U32TO8_LITTLE(s->output + 40,x10);
  U32TO8_LITTLE(s->output + 44,x11);
  U32TO8_LITTLE(s->output + 48,x12);
  U32TO8_LITTLE(s->output + 52,x13);
  U32TO8_LITTLE(s->output + 56,x14);
  U32TO8_LITTLE(s->output + 60,x15);
  /* Increment the 64-bit counter and, on overflow, the 64-bit nonce */
  /* (Incrementing the nonce is not standard but a reasonable default.) */
  if (++ s->ctr[0] == 0)
    if (++ s->ctr[1] == 0)
      if (++ s->nonce[0] == 0)
        ++ s->nonce[1];
}

static void chacha20_init_key(struct chacha20_key * k,
                              uint8_t * key, size_t key_len)
{
  const uint8_t *constants = 
    (uint8_t *) (key_len == 32 ? "expand 32-byte k" : "expand 16-byte k");
  CAMLassert (key_length == 16 || key_length == 32);
  k->key[0] = U8TO32_LITTLE(constants + 0);
  k->key[1] = U8TO32_LITTLE(constants + 4);
  k->key[2] = U8TO32_LITTLE(constants + 8);
  k->key[3] = U8TO32_LITTLE(constants + 12);
  k->key[4] = U8TO32_LITTLE(key + 0);
  k->key[5] = U8TO32_LITTLE(key + 4);
  k->key[6] = U8TO32_LITTLE(key + 8);
  k->key[7] = U8TO32_LITTLE(key + 12);
  if (key_len == 32) key += 16;
  k->key[8] = U8TO32_LITTLE(key + 0);
  k->key[9] = U8TO32_LITTLE(key + 4);
  k->key[10] = U8TO32_LITTLE(key + 8);
  k->key[11] = U8TO32_LITTLE(key + 12);
}
