#include "stdint.h"
#include "math_operations.h"

uint64_t xor_shift(uint32_t bits, uint64_t value);
uint64_t xor_shift(uint32_t bits, uint64_t value);
uint64_t xor_shift33(uint64_t value);
uint64_t first_round_mix32(uint64_t value);
uint64_t second_round_mix32(uint64_t value);

uint32_t mix32(uint64_t value) {
  return (uint32_t) (second_round_mix32(first_round_mix32(value)) >> 32);
}

uint64_t first_round_mix32(uint64_t value) {
  return xor_shift33(value) * 0xff51afd7ed558ccd;
}

uint64_t second_round_mix32(uint64_t value) {
  return xor_shift33(value) * 0xc4ceb9fe1a85ec53;
}

uint64_t xor_shift(uint32_t bits, uint64_t value) {
  return value ^ (value >> bits);
}

uint64_t xor_shift33(uint64_t value) {
  return xor_shift(33, value);
}
