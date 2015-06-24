{-# LANGUAGE ForeignFunctionInterface #-}
module SplitMix.MathOperations (
  mix32
  , mix64
  , mixGamma
  , xorShift33
  ,) where

import Data.Int
import Data.Word
import Data.Bits

import Foreign.C.Types (CUInt(..), CULong(..))

foreign import ccall unsafe "mix32" c_mix32 :: CULong -> CUInt
foreign import ccall unsafe "mix64" c_mix64 :: CULong -> CULong
foreign import ccall unsafe "mix_gamma" c_mixGamma :: CULong -> CULong

mix64 :: Word64 -> Word64
mix64 value = let (CULong result) = c_mix64 $ CULong value in result

mix32 :: Word64 -> Word32
mix32 value = let (CUInt result) = c_mix32 $ CULong value in result

mixGamma :: Word64 -> Word64
mixGamma gamma = let (CULong result) = c_mixGamma $ CULong gamma in result

xorShift :: Int -> Word64 -> Word64
xorShift bits value = xor value $ shiftR value bits

xorShift33 :: Word64 -> Word64
xorShift33 = xorShift 33
