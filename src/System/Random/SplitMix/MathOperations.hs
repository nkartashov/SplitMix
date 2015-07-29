{-# LANGUAGE ForeignFunctionInterface #-}
module System.Random.SplitMix.MathOperations (
  mix32
  , mix64
  , mixGamma
  , xorShift33
  ,) where

import Data.Int
import Data.Word
import Data.Bits

foreign import ccall unsafe "mix32" c_mix32 :: Word64 -> Word32
foreign import ccall unsafe "mix64" c_mix64 :: Word64 -> Word64
foreign import ccall unsafe "mix_gamma" c_mixGamma :: Word64 -> Word64

mix64 :: Word64 -> Word64
mix64 = c_mix64

mix32 :: Word64 -> Word32
mix32 = c_mix32

mixGamma :: Word64 -> Word64
mixGamma = c_mixGamma

xorShift :: Int -> Word64 -> Word64
xorShift bits value = xor value $ shiftR value bits

xorShift33 :: Word64 -> Word64
xorShift33 = xorShift 33
