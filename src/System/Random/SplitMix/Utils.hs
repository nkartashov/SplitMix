{-# LANGUAGE ScopedTypeVariables #-}
module System.Random.SplitMix.Utils
  ( acquireSeedSystem
  , goldenGamma
  ) where

import Data.Word (Word32, Word64)
import Data.Bits ((.|.), shiftL)
import Data.Ratio (numerator, (%))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.IORef (newIORef, atomicModifyIORef)
import System.CPUTime (cpuTimePrecision, getCPUTime)
import System.IO (IOMode(..), hGetBuf, hPutStrLn, withBinaryFile, stderr)
import Control.Monad (liftM, when, unless)
import Foreign (allocaBytes, peek)
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Exception as E

goldenGamma :: Word64
goldenGamma = 0x9e3779b97f4a7c15

wordsTo64Bit :: (Integral a) => Word32 -> Word32 -> a
wordsTo64Bit x y =
      fromIntegral ((fromIntegral x `shiftL` 32) .|. fromIntegral y :: Word64)
{-# INLINE wordsTo64Bit #-}

-- The following functions are borrowed from pcg-random
-- | Acquire seed from current time. This is horrible fall-back for
-- Windows system.
acquireSeedTime :: IO Word64
acquireSeedTime = do
    c <- (numerator . (%cpuTimePrecision)) `liftM` getCPUTime
    t <- toRational `liftM` getPOSIXTime
    let n = fromIntegral (numerator t) :: Word64
    return $ wordsTo64Bit (fromIntegral c) (fromIntegral n)

-- | Get a random number from \"@\/dev\/urandom@\"
devRandom :: IO Word64
devRandom = allocaBytes 8 $ \buf -> do
              nread <- withBinaryFile "/dev/urandom" ReadMode $ \h -> hGetBuf h buf 8
              when (nread /= 8) $ error "unable to read from /dev/urandom"
              peek buf

-- | Get a random number from system source. If \"@\/dev\/urandom@\" is
--   not found return inferior random number from time.
acquireSeedSystem :: IO Word64
acquireSeedSystem =
  devRandom `E.catch` \(_ :: E.IOException) -> do
    seen <- atomicModifyIORef warned ((,) True)
    unless seen $ E.handle (\(_::E.IOException) -> return ()) $ do
      hPutStrLn stderr "Warning: Couldn't open /dev/urandom"
      hPutStrLn stderr ("Warning: using system clock for seed instead " ++
        "(quality will be lower)")
    acquireSeedTime
      where
        warned = unsafePerformIO $ newIORef False
        {-# NOINLINE warned #-}
