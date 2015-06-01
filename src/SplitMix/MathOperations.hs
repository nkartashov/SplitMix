module SplitMix.MathOperations (
  mix32
  , mix64
  , mix64variant13
  , mixGamma
  , xorShift33
  ,) where

import Data.Int
import Data.Word
import Data.Bits

mix64 :: Word64 -> Word64
mix64 = xorShift33 . secondRoundMix64 . firstRoundMix64
firstRoundMix64 = (* 0xff51afd7ed558ccd) . xorShift33
secondRoundMix64 = (* 0xc4ceb9fe1a85ec53) . xorShift33

mix32 :: Word64 -> Word32
mix32 = fromIntegral . ((flip shiftR) 32) . secondRoundMix32 . firstRoundMix32
firstRoundMix32 = (* 0xff51afd7ed558ccd) . xorShift33
secondRoundMix32 = (* 0xc4ceb9fe1a85ec53) . xorShift33

mix64variant13 :: Word64 -> Word64
mix64variant13 = xorShift31 . secondRoundMix64variant13 . secondRoundMix64variant13
firstRoundMix64variant13 = (* 0xbf58476d1ce4e5b9) . xorShift30
secondRoundMix64variant13 = (* 0x94d049bb133111eb) . xorShift27

mixGamma :: Word64 -> Word64
mixGamma gamma = if bitCount >= 24 then xor mixedGamma 0xaaaaaaaaaaaaaaaa
                                   else mixedGamma
  where
    mixedGamma = (mix64variant13 gamma) .|. 1
    bitCount = popCount $ xorShift 1 mixedGamma

xorShift :: Int -> Word64 -> Word64
xorShift bits  value = xor value $ shiftR value bits

xorShift27 :: Word64 -> Word64
xorShift27 = xorShift 27

xorShift30 :: Word64 -> Word64
xorShift30 = xorShift 30

xorShift31 :: Word64 -> Word64
xorShift31 = xorShift 31

xorShift33 :: Word64 -> Word64
xorShift33 = xorShift 33
