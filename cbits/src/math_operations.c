#include "math_operations.h"

uint64_t xor_shift(uint32_t bits, uint64_t value);
uint64_t xor_shift(uint32_t bits, uint64_t value);
uint64_t xor_shift33(uint64_t value);
uint64_t first_round_mix32(uint64_t value);
uint64_t second_round_mix32(uint64_t value);
uint64_t first_round_mix64(uint64_t value);
uint64_t second_round_mix64(uint64_t value);
uint64_t first_round_mix64_variant13(uint64_t value);
uint64_t second_round_mix64_variant13(uint64_t value);

uint32_t mix32(uint64_t value) {
  return (uint32_t) (second_round_mix32(first_round_mix32(value)) >> 32);
}

uint64_t first_round_mix32(uint64_t value) {
  return xor_shift33(value) * 0xff51afd7ed558ccd;
}

uint64_t second_round_mix32(uint64_t value) {
  return xor_shift33(value) * 0xc4ceb9fe1a85ec53;
}

uint64_t mix64(uint64_t value) {
  return xor_shift33(second_round_mix64(first_round_mix64(value)));
}

uint64_t first_round_mix64(uint64_t value) {
  return xor_shift33(value) * 0xff51afd7ed558ccd;
}

uint64_t second_round_mix64(uint64_t value) {
  return xor_shift33(value) * 0xc4ceb9fe1a85ec53;
}

uint64_t mix64variant13(uint64_t value) {
  return xor_shift(31, second_round_mix64_variant13(first_round_mix64_variant13(value)));
}

uint64_t first_round_mix64_variant13(uint64_t value) {
  return xor_shift(30, value) * 0xbf58476d1ce4e5b9;
}

uint64_t second_round_mix64_variant13(uint64_t value) {
  return xor_shift(27, value) * 0x94d049bb133111eb;
}

uint64_t xor_shift(uint32_t bits, uint64_t value) {
  return value ^ (value >> bits);
}

uint64_t xor_shift33(uint64_t value) {
  return xor_shift(33, value);
}

const uint64_t m1  = 0x5555555555555555; //binary: 0101...
const uint64_t m2  = 0x3333333333333333; //binary: 00110011..
const uint64_t m4  = 0x0f0f0f0f0f0f0f0f; //binary:  4 zeros,  4 ones ...
const uint64_t h01 = 0x0101010101010101; //the sum of 256 to the power of 0,1,2,3...

// Population count (Hamming weight) taken from
// https://en.wikipedia.org/wiki/Hamming_weight
int pop_count(uint64_t x) {
  x -= (x >> 1) & m1;             //put count of each 2 bits into those 2 bits
  x = (x & m2) + ((x >> 2) & m2); //put count of each 4 bits into those 4 bits
  x = (x + (x >> 4)) & m4;        //put count of each 8 bits into those 8 bits
  return (x * h01) >> 56;         //returns left 8 bits of x + (x<<8) + (x<<16) + (x<<24) + ...
}

uint64_t mix_gamma(uint64_t value) {
  uint64_t mixed_gamma = mix64variant13(value) | 1;
  int bit_count = pop_count(xor_shift(1, mixed_gamma));
  if (bit_count >= 24) {
    return mixed_gamma ^ 0xaaaaaaaaaaaaaaaa;
  }
  return mixed_gamma;
}
