{-# LANGUAGE BangPatterns #-}
module SplitMix (
  SplitMix64,
  toSeedGamma,
  withSystemRandom,
  newSplitMix64,
  nextInt32,
  nextInt64,
  ) where

import Data.Word
import Data.Int
import Data.Bits
import System.Random (RandomGen, next, split)
import SplitMix.MathOperations

import Control.Applicative

import Test.QuickCheck (Arbitrary(arbitrary))

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray)
import System.IO (IOMode(..), hGetBuf, withBinaryFile)

data SplitMix64 = SplitMix64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64 deriving (Show, Eq)

instance  Arbitrary SplitMix64 where
  arbitrary = SplitMix64 <$> arbitrary <*> arbitrary

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
runFst f !(a, b) = (f a, b)

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

goldenGamma :: Word64
goldenGamma = 0x9e3779b97f4a7c15

newSeededSplitMix64 :: Word64 -> SplitMix64
newSeededSplitMix64 = (flip SplitMix64) goldenGamma

-- FIXME: portability, copied from mwc-random
acquireSeedSystem :: IO [Word64]
acquireSeedSystem = do
    let nbytes = 1024
        random = "/dev/urandom"
    allocaBytes nbytes $ \buf -> do
      nread <- withBinaryFile random ReadMode $ \h -> hGetBuf h buf nbytes
      peekArray (nread `div` 8) buf

newSplitMix64 :: IO SplitMix64
newSplitMix64 = do
  unmixedSeed <- fmap head acquireSeedSystem
  return $ SplitMix64 (mix64 unmixedSeed) $ mixGamma $ unmixedSeed + goldenGamma

withSystemRandom :: (SplitMix64 -> a) -> IO a
withSystemRandom f = fmap f newSplitMix64

instance RandomGen SplitMix64 where
  next = nextInt
  split = splitGen
