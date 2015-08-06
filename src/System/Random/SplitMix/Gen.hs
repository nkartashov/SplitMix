{-# LANGUAGE BangPatterns #-}
module System.Random.SplitMix.Gen
  ( SplitMix64(..)
  , toSeedGamma
  , withSystemRandom
  , newSplitMix64
  , newSeededSplitMix64
  , nextInt32
  , nextInt64
  , nextDouble
  ) where

import Data.Word (Word32, Word64)
import Data.Int (Int64)
import Data.Bits ((.|.), shiftR, testBit)
import System.Random (RandomGen, next, split)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)

import Test.QuickCheck (Arbitrary(arbitrary))

import System.Random.SplitMix.MathOperations (c_mix32, c_mix64, c_mixGamma)
import System.Random.SplitMix.Utils (goldenGamma, acquireSeedSystem)

-- | SplitMix64 generator structure, holding all the needed
-- information for random number generation
data SplitMix64 = SplitMix64
  {-# UNPACK #-} !Word64 -- ^ seed of SplitMix64 generator
  {-# UNPACK #-} !Word64 -- ^ gamma of SplitMix64 generator, always odd
  deriving (Show, Eq)

instance Arbitrary SplitMix64 where
  arbitrary = SplitMix64 <$> arbitrary <*> (fmap makeGammaOdd arbitrary)

-- | Make gammas odd, so Arbitrary instance has a correct invariant
makeGammaOdd :: Word64 -> Word64
makeGammaOdd gamma = if testBit gamma 0 then gamma
                                        else gamma .|. 1

-- | Produces a new seed value from an existing one and a gamma and
-- returns an updated generator
nextSeed :: SplitMix64 -> (Word64, SplitMix64)
nextSeed (SplitMix64 seed gamma) = (newSeed, SplitMix64 newSeed gamma)
  where !newSeed = seed + gamma

-- | Constant used to mix values for nextDouble function
doubleUlp :: Double
doubleUlp = 1.0 / 2097152 -- 1.0 / 1L << 53

-- | Unwraps the generator for reproducible testing
toSeedGamma :: SplitMix64 -> (Word64, Word64)
toSeedGamma (SplitMix64 seed gamma) = (seed, gamma)

-- | Produces a new value of the type 'a' and an updated generator
nextValue :: (Word64 -> a) -> SplitMix64 -> (a, SplitMix64)
nextValue mixer = first mixer . nextSeed

-- | Produces a new Int64 value and an updated generator
nextInt64 :: SplitMix64 -> (Word64, SplitMix64)
nextInt64 = nextValue c_mix64

-- | Produces a new Int32 value and an updated generator
nextInt32 :: SplitMix64 -> (Word32, SplitMix64)
nextInt32 = nextValue (fromIntegral . c_mix32)

-- | Produces a new Int value and an updated generator
nextInt :: SplitMix64 -> (Int, SplitMix64)
nextInt = nextValue (fromIntegral . c_mix32)

-- | Produces a new Double value from 53 bits of the number using 'doubleUlp'
-- for mixing and an updated generator
nextDouble :: SplitMix64 -> (Double, SplitMix64)
nextDouble gen = (doubleUlp * fromIntegral (shiftR int64 11), newGen)
  where (int64, newGen) = nextInt64 gen

-- | Splits the given generator into two, returning updated first generator
-- and a new one with seed and gamma got from the first
splitGen :: SplitMix64 -> (SplitMix64, SplitMix64)
splitGen oldGen = (updatedGen', SplitMix64 (c_mix64 newSeed) (c_mixGamma newGamma))
  where
    (!newSeed, !updatedGen) = nextSeed oldGen
    (!newGamma, !updatedGen') = nextSeed updatedGen

-- | Uses provided seed to make a new SplitMix64 generator instance
newSeededSplitMix64 :: Word64 -> SplitMix64
newSeededSplitMix64 = flip SplitMix64 $ goldenGamma

-- | Makes a new instance of SplitMix64 generator using system provided
-- sources of randomness.
-- Will return a poorly seeded generator instance on Windows
newSplitMix64 :: IO SplitMix64
newSplitMix64 = do
  unmixedSeed <- acquireSeedSystem
  return $ SplitMix64 (c_mix64 unmixedSeed) $ c_mixGamma $! unmixedSeed + goldenGamma

-- | Performs a computation using a newly made SplitMix64 generator instance
withSystemRandom :: (SplitMix64 -> a) -> IO a
withSystemRandom f = fmap f newSplitMix64

instance RandomGen SplitMix64 where
  next = nextInt
  split = splitGen
