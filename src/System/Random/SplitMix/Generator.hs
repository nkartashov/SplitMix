{-# LANGUAGE MultiParamTypeClasses #-}
module System.Random.SplitMix.Generator where

import Data.Word (Word32, Word64)

-- NOTE: borrowed from pcg-random
-- | Defines a class of generators capable of providing with uniformly
-- distributed 32 bit, 64 bit and boundend dedd
class Monad m => Generator g m where
  uniform1 :: (Word32 -> a) -> g -> m a
  uniform2 :: (Word64 -> a) -> g -> m a
  uniform1B :: Integral a => (Word64 -> a) -> Word64 -> g -> m a
