#include "generator.h"

inline void advance(struct SplitMix64* generator);
inline uint64_t next_seed(struct SplitMix64* generator);

inline void advance(struct SplitMix64* generator) {
  generator->seed += generator->gamma;
}

inline uint64_t next_seed(struct SplitMix64* generator) {
  uint64_t result = generator->seed;
  advance(generator);
  return result;
}

uint32_t next_int32(struct SplitMix64* generator) {
  return mix32(next_seed(generator));
}

uint64_t next_int64(struct SplitMix64* generator) {
  return mix64(next_seed(generator));
}

uint64_t next_bounded_int64(struct SplitMix64* generator, uint64_t bound) {
  uint64_t threshold = -bound % bound;
  while (1) {
    uint64_t r = next_int64(generator);
    if (r >= threshold) {
      return r % bound;
    }
  }
}

struct SplitMix64* split_generator(struct SplitMix64* generator) {
  struct SplitMix64* new_generator = (struct SplitMix64*) malloc(sizeof(struct SplitMix64));
  new_generator->seed = next_int64(generator);
  new_generator->gamma = mix_gamma(next_seed(generator));
  return new_generator;
}
