module SplitMix.Utils
  ( acquireSeedSystem
  , goldenGamma
  ) where

import Data.Word (Word64)

import System.IO (IOMode(..), hGetBuf, withBinaryFile)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray)

goldenGamma :: Word64
goldenGamma = 0x9e3779b97f4a7c15

-- NOTE: Will not work on any system without /dev/random,
--       copied from mwc-random
acquireSeedSystem :: IO Word64
acquireSeedSystem = do
    let nbytes = 8
        random = "/dev/urandom"
    allocaBytes nbytes $ \buf -> do
      nread <- withBinaryFile random ReadMode $ \h -> hGetBuf h buf nbytes
      fmap head $ peekArray (nread `div` 8) buf
