{-# LANGUAGE MultiParamTypeClasses #-}
module System.Random.SplitMix.Variate where

import System.Random.SplitMix.Generator (Generator(..))

-- | Class of values that can be uniformly distributed in ranges
-- [0, b], [a, b] or the whole range of their possible values given a generator
class Variate a where
  uniform  :: Generator g m => g -> m a
  uniformR :: Generator g m => (a, a) -> g -> m a
  uniformB :: Generator g m => a -> g -> m a
