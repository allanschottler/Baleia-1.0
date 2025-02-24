module Baleia.Delay where
import Baleia.Internal
import Baleia.Sample

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
-- Delay with Feedback (Mono)
--------------------------------------------------------------------------------

data Delay = Delay { dBuf :: MV.IOVector Sample, dIdx :: Int }

processFdbkDelay :: Delay -> Sample -> Float -> (Sample, Delay)
processFdbkDelay st x fbGain = unsafePerformIO $ do
  let buf = dBuf st
      idx = dIdx st
  y <- MV.read buf idx
  let newVal = x + Mono fbGain * y
  MV.write buf idx newVal
  let newIdx = (idx + 1) `mod` MV.length buf
  return (y, st { dIdx = newIdx })
{-# NOINLINE processFdbkDelay #-}

--------------------------------------------------------------------------------
-- Stereo Feedback Delay SF (Library Level)
--------------------------------------------------------------------------------

fdbkDelay :: Int -> IO (SF (Float, Sample) Sample)
fdbkDelay n = do
  buf <- MV.new n
  MV.set buf Silence
  let initDelay = Delay { dBuf = buf, dIdx = 0 }
  return (fdbkDelay' initDelay)
  where
    fdbkDelay' st = SF $ \(fbGain, s) ->
      let (y, st') = processFdbkDelay st s fbGain
      in (y, fdbkDelay' st')
{-# NOINLINE fdbkDelay #-}