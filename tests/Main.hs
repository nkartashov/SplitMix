module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Word (Word64)

import System.Random.SplitMix.Gen
import System.Random.SplitMix.MathOperations (c_mix64, c_mixGamma, xorShift33)

prop_xorShift33SelfInvertible i = i == (xorShift33 $ xorShift33 i)

unmix64 :: Word64 -> Word64
unmix64 = xorShift33 . secondRoundUnmix64 . firstRoundUnmix64
firstRoundUnmix64 = (* 0x9cb4b2f8129337db) . xorShift33
secondRoundUnmix64 = (* 0x4f74430c22a54005) . xorShift33

prop_mix64unmix64InvertibleCommutative i = (c_mix64 $ unmix64 i) == (unmix64 $ c_mix64 i)

prop_seedGammaPreservation :: SplitMix64 -> Bool
prop_seedGammaPreservation gen = seed == unmix64 q && gamma == (unmix64 q - unmix64 p)
  where
    (p, newGen) = nextInt64 gen
    (q, newGen') = nextInt64 newGen
    (seed, gamma) = toSeedGamma newGen'

prop_resultOfMixGammaShouldAlwaysBeOdd :: Word64 -> Bool
prop_resultOfMixGammaShouldAlwaysBeOdd = odd . c_mixGamma

prop_gammaIsAlwaysOdd :: SplitMix64 -> Bool
prop_gammaIsAlwaysOdd = odd . snd . toSeedGamma


main :: IO ()
main = defaultMain allTests

allTests = [mathOperationTests,
            splitMixOperationsTests]

mathOperationTests = testGroup "MathOperations"
  [testProperty "xorShift33 is an inverse to itself" prop_xorShift33SelfInvertible,
  testProperty "unmix64 inverts mix64 and commutes with it" prop_mix64unmix64InvertibleCommutative]

splitMixOperationsTests = testGroup "SplitMix operations"
  [testProperty "seedAndGamma should be preserved after nextInt64 calls" prop_seedGammaPreservation,
  testProperty "mixGamma function should always return odd gammas" prop_resultOfMixGammaShouldAlwaysBeOdd,
  testProperty "gamma should always be odd" prop_gammaIsAlwaysOdd]
