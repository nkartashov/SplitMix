module SplitMix () where

import Data.Word
import Data.Int
import System.Random
import SplitMix.Primes
import SplitMix.MathOperations

import Foreign.Marshal.Alloc   (allocaBytes)
import Foreign.Marshal.Array   (peekArray)
import System.IO        (IOMode(..), hGetBuf, withBinaryFile)

data SplitMix64 = SplitMix64 {
  seed :: Word64,
  gamma :: Word64
} deriving (Show, Eq)

nextSeed :: SplitMix64 -> (Word64, SplitMix64)
nextSeed (SplitMix64 seed gamma) = (newSeed, SplitMix64 newSeed gamma)
  where newSeed = seed + gamma

nextValue :: (Word64 -> a) -> SplitMix64 -> (a, SplitMix64)
nextValue mixer oldGen = (mixer raw64, newGen)
  where (raw64, newGen) = nextSeed oldGen

nextInt64 :: SplitMix64 -> (Word64, SplitMix64)
nextInt64 = nextValue mix64

nextInt32 :: SplitMix64 -> (Word32, SplitMix64)
nextInt32 = nextValue (fromIntegral . mix32)

nextInt :: SplitMix64 -> (Int, SplitMix64)
nextInt = nextValue (fromIntegral . mix32)

splitGen :: SplitMix64 -> (SplitMix64, SplitMix64)
splitGen oldGen = (updatedGen', SplitMix64 (mix64 newSeed) (mixGamma newGamma))
  where
    (newSeed, updatedGen) = nextSeed oldGen
    (newGamma, updatedGen') = nextSeed updatedGen

goldenGamma :: Word64
goldenGamma = 0x9e3779b97f4a7c15

newSeededSplitMix64 :: Word64 -> SplitMix64
newSeededSplitMix64 = (flip SplitMix64) goldenGamma

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

instance RandomGen SplitMix64 where
  next = nextInt
  split = splitGen
