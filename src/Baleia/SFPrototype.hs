{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Baleia.SFPrototype where

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Control.Category
import Prelude hiding ((.), id)
import Control.Arrow
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.ST (runST)
import Foreign.Storable (Storable)
import Foreign.Ptr (Ptr)

--------------------------------------------------------------------------------
-- SF Abstraction
--------------------------------------------------------------------------------

newtype SF a b = SF { unSF :: a -> (b, SF a b) }

runSF :: SF a b -> a -> (b, SF a b)
runSF (SF f) = f
{-# INLINE runSF #-}

instance Category SF where
  id = SF (\a -> (a, id))
  (SF g) . (SF f) = SF $ \a ->
    let (b, sf1) = f a
        (c, sf2) = g b
    in (c, sf2 . sf1)
  {-# INLINE (.) #-}

instance Arrow SF where
  arr f = SF (\a -> (f a, arr f))
  {-# INLINE arr #-}
  first (SF f) = SF $ \(b, d) ->
    let (c, sf') = f b
    in ((c, d), first sf')
  {-# INLINE first #-}

--------------------------------------------------------------------------------
-- Efficient Delay Using a Mutable Ring Buffer (Mono)
--------------------------------------------------------------------------------

data Delay = Delay { dBuf :: MV.IOVector Float, dIdx :: Int }

processDelay :: Delay -> Float -> (Float, Delay)
processDelay st x = unsafePerformIO $ do
  let buf = dBuf st
      idx = dIdx st
  y <- MV.read buf idx
  MV.write buf idx x
  let newIdx = (idx + 1) `mod` MV.length buf
  return (y, st { dIdx = newIdx })

stereoDelaySF :: Int -> IO(SF (Float, Float) (Float, Float))
stereoDelaySF n = do
  bufL <- MV.new n
  MV.set bufL 0
  bufR <- MV.new n
  MV.set bufR 0
  let initDelayL = Delay { dBuf = bufL, dIdx = 0 }
      initDelayR = Delay { dBuf = bufR, dIdx = 0 }
  return (delayStereo initDelayL initDelayR)
  where
    delayStereo stL stR = SF $ \(l, r) ->
      let (yL, stL') = processDelay stL l
          (yR, stR') = processDelay stR r
      in ((yL, yR), delayStereo stL' stR')
{-# NOINLINE stereoDelaySF #-}

-- | Process one sample with feedback.
--   Instead of just writing the new input, we write: newVal = input + fbGain * delayedOutput.
processDelayFeedback :: Delay -> Float -> Float -> (Float, Delay)
processDelayFeedback st x fbGain = unsafePerformIO $ do
  let buf = dBuf st
      idx = dIdx st
  y <- MV.read buf idx
  let newVal = x + fbGain * y
  MV.write buf idx newVal
  let newIdx = (idx + 1) `mod` MV.length buf
  return (y, st { dIdx = newIdx })

-- | A stereo feedback delay SF.
stereoDelayFeedbackSF :: Int -> IO (Float -> SF (Float, Float) (Float, Float))
stereoDelayFeedbackSF n = do
  bufL <- MV.new n
  MV.set bufL 0
  bufR <- MV.new n
  MV.set bufR 0
  let initDelayL = Delay { dBuf = bufL, dIdx = 0 }
      initDelayR = Delay { dBuf = bufR, dIdx = 0 }
  pure $ \fbGain -> (delayStereo initDelayL initDelayR fbGain)
  where
    delayStereo stL stR fbGain = SF $ \(l, r) ->
      let (yL, stL') = processDelayFeedback stL l fbGain
          (yR, stR') = processDelayFeedback stR r fbGain
      in ((yL, yR), delayStereo stL' stR' fbGain)
{-# NOINLINE stereoDelayFeedbackSF #-}

