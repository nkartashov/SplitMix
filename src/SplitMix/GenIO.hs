{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SplitMix.GenIO where

import Control.Monad (Monad)
import Control.Applicative ((<$>))
import Data.Int (Int32)
import Data.Word (Word32, Word64)
import Foreign.Ptr (Ptr)
import Data.Bits (rotateR)
import Foreign.Marshal.Utils (new)

import SplitMix.Gen (SplitMix64(..), newSplitMix64, newSeededSplitMix64)
import SplitMix.Utils (goldenGamma, acquireSeedSystem)
import SplitMix.StorableGen

newtype SplitMixGen = SplitMixGen (Ptr SplitMix64) deriving (Eq, Show)

-- NOTE: borrowed from pcg-random
class Monad m => Generator g m where
  uniform1 :: (Word32 -> a) -> g -> m a
  uniform2 :: (Word64 -> a) -> g -> m a
  uniform1B :: Integral a => (Word64 -> a) -> Word64 -> g -> m a

foreign import ccall unsafe "next_int32"
  c_nextInt32 :: Ptr SplitMix64 -> IO Word32

foreign import ccall unsafe "next_int64"
  c_nextInt64 :: Ptr SplitMix64 -> IO Word64

foreign import ccall unsafe "next_bounded_int64"
  c_nextBoundedInt64 :: Ptr SplitMix64 -> Word64 -> IO Word64

newSplitMixGen :: IO SplitMixGen
newSplitMixGen = fmap SplitMixGen $ newSplitMix64 >>= new

newSeededSplitMixGen :: Word64 -> IO SplitMixGen
newSeededSplitMixGen seed = fmap SplitMixGen $ new $ newSeededSplitMix64 seed

class Variate a where
  uniform  :: Generator g m => g -> m a
  uniformR :: Generator g m => (a, a) -> g -> m a
  uniformB :: Generator g m => a -> g -> m a

instance Generator SplitMixGen IO where
  uniform1 f (SplitMixGen p) = f <$> c_nextInt32 p
  uniform2 f (SplitMixGen p) = f <$> c_nextInt64 p
  uniform1B f b (SplitMixGen p) = f <$> c_nextBoundedInt64 p b

instance Variate Int32 where
  uniform = uniform1 fromIntegral
  uniformR = undefined
  uniformB b = uniform1B fromIntegral (fromIntegral b)

instance Variate Int where
  uniform = uniform1 fromIntegral
  uniformR = undefined
  uniformB b = uniform1B fromIntegral (fromIntegral b)
