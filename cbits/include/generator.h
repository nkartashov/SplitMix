#ifndef GENERATOR_H
#define GENERATOR_H

#include "stdlib.h"
#include "math_operations.h"

struct SplitMix64 {
  uint64_t seed;
  uint64_t gamma;
};

void advance(struct SplitMix64* generator);
uint64_t next_seed(struct SplitMix64* generator);
uint32_t next_int32(struct SplitMix64* generator);
uint64_t next_int64(struct SplitMix64* generator);
uint64_t next_bounded_int64(struct SplitMix64* rng, uint64_t bound);
struct SplitMix64* split_generator(struct SplitMix64* generator);

#endif
