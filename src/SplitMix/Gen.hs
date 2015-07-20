{-# LANGUAGE BangPatterns #-}
module SplitMix.Gen (
  SplitMix64(..),
  toSeedGamma,
  withSystemRandom,
  newSplitMix64,
  newSeededSplitMix64,
  nextInt32,
  nextInt64,
  ) where

import Data.Word
import Data.Int
import Data.Bits
import System.Random (RandomGen, next, split)
import Control.Applicative

import Test.QuickCheck (Arbitrary(arbitrary))

import SplitMix.MathOperations
import SplitMix.Utils (goldenGamma, acquireSeedSystem)


data SplitMix64 = SplitMix64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64 deriving (Show, Eq)

instance  Arbitrary SplitMix64 where
  arbitrary = SplitMix64 <$> arbitrary <*> (fmap makeGammaOdd arbitrary)

makeGammaOdd :: Word64 -> Word64
makeGammaOdd gamma = if testBit gamma 0 then gamma
                                        else gamma .|. 1

nextSeed :: SplitMix64 -> (Word64, SplitMix64)
nextSeed (SplitMix64 seed gamma) = (newSeed, SplitMix64 newSeed gamma)
  where !newSeed = seed + gamma

doubleUlp :: Double
doubleUlp = 1.0 / 2097152 -- 1.0 / 1L << 53

toSeedGamma :: SplitMix64 -> (Word64, Word64)
toSeedGamma (SplitMix64 seed gamma) = (seed, gamma)

nextValue :: (Word64 -> a) -> SplitMix64 -> (a, SplitMix64)
nextValue mixer = runFst mixer . nextSeed

runFst :: (a -> b) -> (a, c) -> (b, c)
runFst f (!a, b) = (f a, b)

nextInt64 :: SplitMix64 -> (Word64, SplitMix64)
nextInt64 = nextValue mix64

nextInt32 :: SplitMix64 -> (Word32, SplitMix64)
nextInt32 = nextValue (fromIntegral . mix32)

nextInt :: SplitMix64 -> (Int, SplitMix64)
nextInt = nextValue (fromIntegral . mix32)

nextDouble :: SplitMix64 -> (Double, SplitMix64)
nextDouble gen = (doubleUlp * fromIntegral (shiftR int64 11), newGen)
  where (int64, newGen) = nextInt64 gen

splitGen :: SplitMix64 -> (SplitMix64, SplitMix64)
splitGen oldGen = (updatedGen', SplitMix64 (mix64 newSeed) (mixGamma newGamma))
  where
    !(newSeed, updatedGen) = nextSeed oldGen
    !(newGamma, updatedGen') = nextSeed updatedGen

newSeededSplitMix64 :: Word64 -> SplitMix64
newSeededSplitMix64 = (flip SplitMix64) goldenGamma

newSplitMix64 :: IO SplitMix64
newSplitMix64 = do
  unmixedSeed <- acquireSeedSystem
  return $ SplitMix64 (mix64 unmixedSeed) $ mixGamma $ unmixedSeed + goldenGamma

withSystemRandom :: (SplitMix64 -> a) -> IO a
withSystemRandom f = fmap f newSplitMix64

instance RandomGen SplitMix64 where
  next = nextInt
  split = splitGen
