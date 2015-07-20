#ifndef MATH_OPERATIONS_H
#define MATH_OPERATIONS_H

#include "stdint.h"

uint32_t mix32(uint64_t input);
uint64_t mix64(uint64_t input);
uint64_t mix_gamma(uint64_t input);

#endif
