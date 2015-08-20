module System.Random.SplitMix
  ( module System.Random.SplitMix.GenIO
  , SplitMix64
  , toSeedGamma
  , withSystemRandom
  , newSplitMix64
  , newSeededSplitMix64
  , nextInt32
  , nextInt64
  , nextDouble
  ) where

import System.Random.SplitMix.Gen
  ( SplitMix64
  , toSeedGamma
  , withSystemRandom
  , newSplitMix64
  , newSeededSplitMix64
  , nextInt32
  , nextInt64
  , nextDouble
  )
import System.Random.SplitMix.GenIO
