{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
module Baleia.Internal where

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Control.Category
import Prelude hiding ((.), id)
import Control.Arrow
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Storable (Storable)
import Foreign.Storable.Tuple
import Foreign.Ptr (Ptr)
import Data.IORef
import Data.Fixed (mod')
import Data.Bits
import Data.Word
import Unsafe.Coerce
import Baleia.Meta
import Baleia.Math
import Baleia.Sample

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
  first (SF f) = SF $ \(b,d) ->
    let (c, sf') = f b
    in ((c,d), first sf')
  {-# INLINE first #-}
  (SF f) *** (SF g) = SF $ \(b,d) ->
    let (c, sf1) = f b
        (e, sf2) = g d
    in ((c,e), sf1 *** sf2)
  {-# INLINE (***) #-}
  (SF f) &&& (SF g) = SF $ \a ->
    let (b, sf1) = f a
        (c, sf2) = g a
    in ((b,c), sf1 &&& sf2)
  {-# INLINE (&&&) #-}

lerp a b t = a + (b - a) * t

amp :: SF (Float, Float) Float
amp = arr $ \(g, s) -> g * s
{-# INLINE amp #-}

mono :: SF Float Sample
mono = arr monos
{-# INLINE mono #-}

clip :: SF (Float, Float) Float
clip = arr $ \(thrsh, s) -> clipi thrsh s
  where clipi t v | abs v > t = t * signum v 
                  | otherwise = v
{-# INLINE clip #-}

switch :: SF (Bool, Float, Float) Float
switch = arr $ \(o, a, b) -> if o then a else b
{-# INLINE switch #-}

lin :: SF (Float, Float, Float) Float
lin = arr $ \(min, max, f) -> min + (max - min) * f
{-# INLINE lin #-}

lgr :: SF (Float, Float, Float) Float
lgr = arr $ \(min, max, f) -> min * (max / min) ** f
{-# INLINE lgr #-}

unipol :: SF Float Float
unipol = arr $ \s -> (s + 1) * 0.5
{-# INLINE unipol #-}

bipol :: SF Float Float
bipol = arr $ \s -> s * 2 - 1
{-# INLINE bipol #-}

--------------------------------------------------------------------------------
-- Block Processing
--------------------------------------------------------------------------------

processBlock sf ctrl vec =
  let n = V.length vec
      (ys, finalSF) = go 0 sf [] n
  in (V.fromList (reverse ys), finalSF)
  where
    go i currentSF acc n
      | i >= n    = (acc, currentSF)
      | otherwise =
          let (y, nextSF) = runSF currentSF (ctrl, (vec V.! i))
          in go (i + 1) nextSF (y : acc) n
{-# INLINE processBlock #-}

--------------------------------------------------------------------------------
-- Delay
--------------------------------------------------------------------------------

data Delay = Delay { dBuf :: MV.IOVector Float, dIdx :: Int }

processFdbkDelay :: Delay -> Float -> Float -> (Float, Delay)
processFdbkDelay st x fbGain = unsafePerformIO $ do
  let buf = dBuf st
      idx = dIdx st
  y <- MV.read buf idx
  let newVal = x + fbGain * y
  MV.write buf idx newVal
  let newIdx = (idx + 1) `mod` MV.length buf
  return (y, st { dIdx = newIdx })
{-# NOINLINE processFdbkDelay #-}

fdbkDelay :: Int -> SF (Float, Float) Float
fdbkDelay n = unsafePerformIO $ do
  buf <- MV.new n
  MV.set buf 0
  let initDelay = Delay { dBuf = buf, dIdx = 0 }
  return (fdbkDelay' initDelay)
  where
    fdbkDelay' st = SF $ \(fbGain, s) ->
      let (y, st') = processFdbkDelay st s fbGain
      in (y, fdbkDelay' st')
{-# NOINLINE fdbkDelay #-}

--------------------------------------------------------------------------------
-- Filters
--------------------------------------------------------------------------------

-- | Lowpass Bi-Quad Filter.
--   Processes a tuple (freq, q, sample) and outputs a filtered sample.
--   Uses a fixed sample rate of 44100.
lowpass :: SF (Float, Float, Float) Float
lowpass = go 0 0 0 0
  where
    go x1 x2 y1 y2 = SF $ \(freq, q, s) ->
      let w0 = 2 * pi * freq / 44100
          alpha = sin w0 / (2 * q)
          a0 = 1 + alpha
          a1 = -2 * cos w0
          a2 = 1 - alpha
          b0 = (1 - cos w0) / 2
          b1 = 1 - cos w0
          b2 = (1 - cos w0) / 2
          y = b0 * s + b1 * x1 + b2 * x2 - a1 * y1 - a2 * y2
      in (y, go x1 y1 x2 y2)
{-# INLINE lowpass #-}

--------------------------------------------------------------------------------
-- Oscillator
--------------------------------------------------------------------------------

sinWave :: V.Vector Float
sinWave = V.generate 1024 $ \i -> sin (2 * pi * fromIntegral i / 1024)
{-# NOINLINE sinWave #-}

sqrWave :: V.Vector Float
sqrWave = V.generate 1024 $ \i -> if i < 512 then 1 else -1
{-# NOINLINE sqrWave #-}

sawWave :: V.Vector Float
sawWave = V.generate 1024 $ \i -> 2 * fromIntegral i / 1023 - 1
{-# NOINLINE sawWave #-}

rampWave :: V.Vector Float
rampWave = V.generate 1024 $ \i -> fromIntegral i / 1023
{-# NOINLINE rampWave #-}

triWave :: V.Vector Float
triWave = V.generate 1024 $ \i -> 
    let t = fromIntegral i / 1023
    in if t < 0.5 then -1 + 4 * t else 3 - 4 * t
{-# NOINLINE triWave #-}

oscWavetable :: V.Vector Float -> SF Float Float
oscWavetable table =
  let tableLength = V.length table
      stdSzF = fromIntegral tableLength
      sampleRate = 44100
      go phase = SF $ \freq ->
        let f      = freq
            inc    = f * stdSzF / fromIntegral sampleRate
            phase' = phase + inc
            phaseW = phase' `mod'` stdSzF
            i      = floor phaseW
            iNext  = if i + 1 >= tableLength then 0 else i + 1
            alpha  = phaseW - fromIntegral i
            a      = table V.! i
            b      = table V.! iNext
            sample = lerp a b alpha
         in (sample, go phaseW)
  in go 0
{-# INLINE oscWavetable #-}

oscSin = oscWavetable sinWave
{-# INLINE oscSin #-}
oscSqr = oscWavetable sqrWave
{-# INLINE oscSqr #-}
oscTri = oscWavetable triWave
{-# INLINE oscTri #-}
oscSaw = oscWavetable sawWave
{-# INLINE oscSaw #-}
oscRamp = oscWavetable rampWave
{-# INLINE oscRamp #-}

lfoSin = unipol <<< oscWavetable sinWave
{-# INLINE lfoSin #-}
lfoSqr = unipol <<< oscWavetable sqrWave
{-# INLINE lfoSqr #-}
lfoTri = unipol <<< oscWavetable triWave
{-# INLINE lfoTri #-}
lfoSaw = unipol <<< oscWavetable sawWave
{-# INLINE lfoSaw #-}
lfoRamp = unipol <<< oscWavetable rampWave
{-# INLINE lfoRamp #-}

oscWNoise :: SF () Float
oscWNoise = go 22222
  where
    f s = (((s * 196314165) + 907633515) .&. 0x7FFFFF) + 0x40000000
    toFloat = unsafeCoerce :: Word32 -> Float
    center x = x - 3
    go seed = SF $ \() ->
      let newSeed = f seed
          noise   = center (toFloat newSeed)
      in (noise, go newSeed)
{-# INLINE oscWNoise #-}

pkCoefs :: [(Float, Float)]
pkCoefs =
  [ (0.997, 0.029591)  -- z0
  , (0.985, 0.032534)  -- z1
  , (0.950, 0.048056)  -- z2
  , (0.850, 0.090579)  -- z3
  , (0.620, 0.108990)  -- z4
  , (0.250, 0.255784)  -- z5
  ]

-- updatePink :: Float -> [Float] -> (Float, [Float])
-- updatePink x zs =
--   let newZs = zipWith (\z (a,b) -> a * z + b * x) zs pkCoefs
--   in (sum newZs, newZs)
-- {-# INLINE updatePink #-}

-- oscPkNoise :: SF (Control, Sample) (Control, Sample)
-- oscPkNoise = pinkState (replicate (length pkCoefs) 0)
--   where
--     pinkState zs = SF $ \(c, s) ->
--       let x       = fst s * 16384
--           (p,zs') = updatePink x zs
--           sample  = p * 0.0000243
--       in ((c, mono sample), pinkState zs')
-- {-# INLINE oscPkNoise #-}