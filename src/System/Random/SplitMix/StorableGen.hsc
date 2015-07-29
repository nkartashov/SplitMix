{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}
module System.Random.SplitMix.StorableGen where

import Foreign.Storable
import System.Random.SplitMix.Gen (SplitMix64(..))

#include "generator.h"

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable SplitMix64 where
  sizeOf _ = (#size struct SplitMix64)
  alignment _ = #{alignment struct SplitMix64}
  peek ptr = do
    seed <- (#peek struct SplitMix64, seed) ptr
    gamma <- (#peek struct SplitMix64, gamma) ptr
    return $ SplitMix64 seed gamma

  poke ptr (SplitMix64 seed gamma) = do
    #{poke struct SplitMix64, seed} ptr seed
    #{poke struct SplitMix64, gamma} ptr gamma
