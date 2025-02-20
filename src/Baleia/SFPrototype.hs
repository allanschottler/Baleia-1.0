{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Baleia.SFPrototype where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>))
import Control.Category
import Prelude hiding ((.), id)
import Control.Arrow

--------------------------------------------------------------------------------
-- Pure SF Abstraction
--------------------------------------------------------------------------------

-- | A minimal signal function abstraction.
--   An SF takes an input sample and returns an output sample along with the updated SF.
newtype SF a b = SF { runSF :: a -> (b, SF a b) }

instance Category SF where
  id = SF (\a -> (a, id))
  (SF g) . (SF f) = SF $ \a ->
    let (b, sf1) = f a
        (c, sf2) = g b
    in (c, sf2 . sf1)

instance Arrow SF where
  arr f = SF (\a -> (f a, arr f))
  first (SF f) = SF $ \(b, d) ->
    let (c, sf') = f b
    in ((c, d), first sf')

--------------------------------------------------------------------------------
-- Pure Delay Using Data.Sequence
--------------------------------------------------------------------------------

-- | Process one sample through the delay buffer.
--   The buffer is modeled as a sequence of Float values.
delayStep :: Seq Float -> Float -> (Float, Seq Float)
delayStep buf x = case Seq.viewl buf of
  Seq.EmptyL       -> error "delayStep: buffer is empty"
  y Seq.:< rest -> (y, rest |> x)

-- | Pure delay SF (mono) using a Data.Sequence as a ring buffer.
--   The delay line is represented by a sequence of length n.
delaySFPure :: Int -> SF Float Float
delaySFPure n = delay (Seq.replicate n 0)
  where
    delay buf = SF $ \x ->
      let (y, buf') = delayStep buf x
      in (y, delay buf')
{-# INLINE delaySFPure #-}

--------------------------------------------------------------------------------
-- Pure Stereo Delay
--------------------------------------------------------------------------------

-- | Pure stereo delay SF using separate delay buffers for left and right channels.
stereoDelaySFPure :: Int -> SF (Float, Float) (Float, Float)
stereoDelaySFPure n = stereoDelay (Seq.replicate n 0, Seq.replicate n 0)
  where
    stereoDelay (bufL, bufR) = SF $ \(l, r) ->
      let (yL, bufL') = delayStep bufL l
          (yR, bufR') = delayStep bufR r
      in ((yL, yR), stereoDelay (bufL', bufR'))
{-# INLINE stereoDelaySFPure #-}

--------------------------------------------------------------------------------
-- Example Stereo Processing Chain
--------------------------------------------------------------------------------

-- | Example chain: simply a pure stereo delay with a 1-second delay at 44100 Hz.
exampleChainStereo :: SF (Float, Float) (Float, Float)
exampleChainStereo = stereoDelaySFPure 44100
{-# INLINE exampleChainStereo #-}

