{-# LANGUAGE ForeignFunctionInterface #-}
module System.Random.SplitMix.MathOperations
  ( c_mix32
  , c_mix64
  , c_mixGamma
  , xorShift33
  ,) where

import Data.Word (Word32, Word64)
import Data.Bits (xor, shiftR)

-- | Mixing fuction to produce 32 bit values as per the paper
foreign import ccall unsafe "mix32" c_mix32 :: Word64 -> Word32
-- | Mixing function to produce 64 bit values as per the paper
foreign import ccall unsafe "mix64" c_mix64 :: Word64 -> Word64
-- | Mixing fuction to produce gamma values as per the paper
-- always produces odd values
foreign import ccall unsafe "mix_gamma" c_mixGamma :: Word64 -> Word64

-- | Bitwise operation equivalent to f n v = (v >> n) ^ v
xorShift :: Int -> Word64 -> Word64
xorShift bits value = xor value $ shiftR value bits

-- | Bitwise operation equivalent to f v = (v >> 33) ^ v
xorShift33 :: Word64 -> Word64
xorShift33 = xorShift 33
