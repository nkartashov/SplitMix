{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module System.Random.SplitMix.GenIO
 ( SplitMixGen
 , newSplitMixGen
 , deleteSplitMixGen
 , newSeededSplitMix64
 ) where

import Control.Monad (Monad)
import Control.Applicative ((<$>))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.Ptr (Ptr)
import Data.Bits (rotateR)
import Foreign.Marshal.Utils (new)
import Foreign.Marshal.Alloc (free)

import System.Random.SplitMix.Gen (SplitMix64(..), newSplitMix64, newSeededSplitMix64)
import System.Random.SplitMix.Utils (goldenGamma, acquireSeedSystem)
import System.Random.SplitMix.StorableGen ()
import System.Random.SplitMix.Generator (Generator(..))
import System.Random.SplitMix.Variate (Variate(..))

-- |  Wrapper for SplitMix64 structure for using it with C
newtype SplitMixGen = SplitMixGen (Ptr SplitMix64) deriving (Eq, Show)

-- | Gets as new randomly distributed Word32 value
foreign import ccall unsafe "next_int32"
  c_nextInt32 :: Ptr SplitMix64 -> IO Word32

-- | Gets as new randomly distributed Word64 value
foreign import ccall unsafe "next_int64"
  c_nextInt64 :: Ptr SplitMix64 -> IO Word64

-- | Gets as new randomly distributed Word64 value from the interval
-- [0, bound]
foreign import ccall unsafe "next_bounded_int64"
  c_nextBoundedInt64 :: Ptr SplitMix64 -> Word64 -> IO Word64

-- | Allocates a new instance of SplitMix64 generator
newSplitMixGen :: IO SplitMixGen
newSplitMixGen = fmap SplitMixGen $ newSplitMix64 >>= new

-- | Deallocates an instance of SplitMix64 generator
deleteSplitMixGen :: SplitMixGen -> IO ()
deleteSplitMixGen (SplitMixGen ptr) = free ptr

-- | Seeds a newly allocated SplitMix64 generator instance with a provided
-- 64 bit value
newSeededSplitMixGen :: Word64 -> IO SplitMixGen
newSeededSplitMixGen seed = fmap SplitMixGen $ new $ newSeededSplitMix64 seed


instance Generator SplitMixGen IO where
  uniform1 f (SplitMixGen p) = f <$> c_nextInt32 p
  {-# INLINE uniform1 #-}
  uniform2 f (SplitMixGen p) = f <$> c_nextInt64 p
  {-# INLINE uniform2 #-}
  uniform1B f b (SplitMixGen p) = f <$> c_nextBoundedInt64 p b
  {-# INLINE uniform1B #-}

-- NOTE: borrowed from pcg-random
-- | Produces a value in range given a generator for them
uniformRange :: (Generator g m, Integral a, Bounded a, Variate a)
             => (a,a) -> g -> m a
uniformRange (x1,x2) g
      | n == 0    = uniform g   -- Abuse overflow in unsigned types
      | otherwise = loop
        where
         -- Allow ranges where x2<x1
         (i, j) | x1 < x2   = (x1, x2)
                | otherwise = (x2, x1)
                 -- (# i, j #) | x1 < x2   = (# x1, x2 #)
                 --            | otherwise = (# x2, x1 #)
         n       = j - i + 1
         buckets = maxBound `div` n
         maxN    = buckets * n
         loop    = do x <- uniform g
                      if x < maxN then return $! i + (x `div` buckets)
                      else loop
{-# INLINE uniformRange #-}

instance Variate Int8 where
  uniform = uniform1 fromIntegral
  {-# INLINE uniform #-}
  uniformR = uniformRange
  {-# INLINE uniformR #-}
  uniformB b = uniform1B fromIntegral (fromIntegral b)
  {-# INLINE uniformB #-}

instance Variate Int16 where
  uniform = uniform1 fromIntegral
  {-# INLINE uniform #-}
  uniformR = uniformRange
  {-# INLINE uniformR #-}
  uniformB b = uniform1B fromIntegral (fromIntegral b)
  {-# INLINE uniformB #-}

instance Variate Int32 where
  uniform = uniform1 fromIntegral
  {-# INLINE uniform #-}
  uniformR = uniformRange
  {-# INLINE uniformR #-}
  uniformB b = uniform1B fromIntegral (fromIntegral b)
  {-# INLINE uniformB #-}

instance Variate Int64 where
  uniform = uniform2 fromIntegral
  {-# INLINE uniform #-}
  uniformR = uniformRange
  {-# INLINE uniformR #-}
  uniformB b = uniform1B fromIntegral (fromIntegral b)
  {-# INLINE uniformB #-}

instance Variate Word8 where
  uniform = uniform1 fromIntegral
  {-# INLINE uniform #-}
  uniformR = uniformRange
  {-# INLINE uniformR #-}
  uniformB b = uniform1B fromIntegral (fromIntegral b)
  {-# INLINE uniformB #-}

instance Variate Word16 where
  uniform = uniform1 fromIntegral
  {-# INLINE uniform #-}
  uniformR = uniformRange
  {-# INLINE uniformR #-}
  uniformB b = uniform1B fromIntegral (fromIntegral b)
  {-# INLINE uniformB #-}

instance Variate Word32 where
  uniform = uniform1 id
  {-# INLINE uniform #-}
  uniformR = uniformRange
  {-# INLINE uniformR #-}
  uniformB b = uniform1B fromIntegral (fromIntegral b)
  {-# INLINE uniformB #-}

instance Variate Word64 where
  uniform = uniform2 id
  {-# INLINE uniform #-}
  uniformR = uniformRange
  {-# INLINE uniformR #-}
  uniformB b = uniform1B id (fromIntegral b)
  {-# INLINE uniformB #-}

instance Variate Int where
  uniform = uniform1 fromIntegral
  {-# INLINE uniform #-}
  uniformR = uniformRange
  {-# INLINE uniformR #-}
  uniformB b = uniform1B fromIntegral (fromIntegral b)
  {-# INLINE uniformB #-}
