module Test where

import System.Random (randoms)

import System.Random.SplitMix.Variate
import System.Random.SplitMix (newSplitMix64, newSplitMixGen)

randomInts :: IO [Int]
randomInts = randoms `fmap` newSplitMix64

randomIntsFromGen :: IO Int
randomIntsFromGen = newSplitMixGen >>= uniform
