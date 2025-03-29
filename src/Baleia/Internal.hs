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

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi
{-# INLINE clamp #-}

lerp a b t = a + (b - a) * t
{-# INLINE lerp #-}

logr a b t = a * (b / a) ** t
{-# INLINE logr #-}

bool a = fromIntegral $ round a
{-# INLINE bool #-}

-- pick' xs x = xs !! pick'' (length xs) x
--   where pick'' n x = min (floor (x * fromIntegral n)) (n - 1)
-- {-# INLINE pick' #-}

-- pick :: SF ([Float], Float) Float
-- pick = SF $ \(xs, x) -> (pick' xs x, pick)
-- {-# INLINE pick #-}

mono :: Float -> Sample
mono x = (x, x)
{-# INLINE mono #-}

clip :: Float -> Float -> Float
clip t v | abs v > t = t * signum v 
          | otherwise = v
{-# INLINE clip #-}

unipol :: Float -> Float
unipol s = (s + 1) * 0.5
{-# INLINE unipol #-}

bipol :: Float -> Float
bipol s = s * 2 - 1
{-# INLINE bipol #-}

dB :: Float -> Float
dB s = 20 * logBase 10 s
{-# INLINE dB #-}

pan :: Float -> Sample -> Sample
pan p (l, r) = (l - p * r, r + p * l)
{-# INLINE pan #-}

--------------------------------------------------------------------------------
-- Block Processing
--------------------------------------------------------------------------------

-- processBlock sf ctrl vec =
--   let n = V.length vec
--       (ys, finalSF) = go 0 sf [] n ctrl
--   in (V.fromList (reverse ys), finalSF)
--   where
--     go i currentSF acc n c
--       | i >= n    = (acc, currentSF)
--       | otherwise =
--           let ((c', y), nextSF) = runSF currentSF (c, (vec V.! i))
--           in go (i + 1) nextSF ((c', y) : acc) n c'
-- {-# INLINE processBlock #-}

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

-- data DelayDyn = DelayDyn { ddBuf :: MV.IOVector Float, ddSz :: Int, ddIdx :: Int }

processFdbkDelayDyn :: Delay -> Float -> Float -> Float -> (Float, Delay)
processFdbkDelayDyn st x sz fbGain = unsafePerformIO $ do
  let buf = dBuf st
      idx = dIdx st
      isz = round sz
      nidx = clamp 0 (isz - 1) idx
  y <- MV.read buf idx
  let newVal = x + fbGain * y
  MV.write buf idx newVal
  let newIdx = (nidx + 1) `mod` isz
  return (y, st { dIdx = newIdx })
{-# NOINLINE processFdbkDelayDyn #-}

fdbkDelayDyn :: SF (Float, Float, Float) Float
fdbkDelayDyn = unsafePerformIO $ do
  let maxbuff = 44100 * 10
  buf <- MV.new maxbuff
  MV.set buf 0
  let initDelay = Delay { dBuf = buf, dIdx = 0 }
  return (fdbkDelayDyn' initDelay)
  where
    fdbkDelayDyn' st = SF $ \(sz, fbGain, s) ->
      let (y, st') = processFdbkDelayDyn st s sz fbGain
      in (y, fdbkDelayDyn' st')
{-# NOINLINE fdbkDelayDyn #-}

--------------------------------------------------------------------------------
-- Filters
--------------------------------------------------------------------------------

lpf :: SF (Float, Float, Float) Float
lpf = lpfState 0 0 0 0
  where
    lpfState :: Float -> Float -> Float -> Float -> SF (Float, Float, Float) Float
    lpfState x1 x2 y1 y2 = SF $ \(freq, q, x) ->
      let theta = freq * 2 * pi / 44100
          d     = if q == 0 then 0 else 1 / q
          f'    = 0.5 * d * sin theta
          beta  = if f' == -1 then 0 else 0.5 * (1 - f') / (1 + f')
          gama  = (0.5 + beta) * cos theta
          a0    = 0.5 * (0.5 + beta - gama)
          a1    = 0.5 + beta - gama
          a2    = a0
          b1    = -2 * gama
          b2    = 2 * beta
          y     = a0 * x + a1 * x1 + a2 * x2 - b1 * y1 - b2 * y2
      in (y, lpfState x x1 y y1)

bpf :: SF (Float, Float, Float) Float
bpf = bpfState 0 0 0 0 0
  where
    bpfState :: Float -> Float -> Float -> Float -> Float -> SF (Float, Float, Float) Float
    bpfState x1 x2 y1 y2 y3 = SF $ \(freq, q, x) ->
      let theta = freq * 2 * pi / 44100
          d     = if q == 0 then 0 else 1 / q
          f'    = 0.5 * d * sin theta
          beta  = if f' == -1 then 0 else 0.5 * (1 - f') / (1 + f')
          gama  = (0.5 + beta) * cos theta
          a0    = 0.5 * (0.5 + beta)
          a1    = 0
          a2    = -0.5 * (0.5 + beta)
          b1    = -2 * gama
          b2    = 2 * beta
          y     = a0 * x + a1 * x1 + a2 * x2 - b1 * y1 - b2 * y2
      in (y, bpfState x x1 y y1 y2)

hpf :: SF (Float, Float, Float) Float
hpf = hpfState 0 0 0 0
  where
    hpfState :: Float -> Float -> Float -> Float -> SF (Float, Float, Float) Float
    hpfState x1 x2 y1 y2 = SF $ \(freq, q, x) ->
      let theta = freq * 2 * pi / 44100
          d     = if q == 0 then 0 else 1 / q
          f'    = 0.5 * d * sin theta
          beta  = if f' == -1 then 0 else 0.5 * (1 - f') / (1 + f')
          gama  = (0.5 + beta) * cos theta
          a0    = 0.5 * (0.5 + beta + gama)
          a1    = -0.5 * (0.5 + beta + gama)
          a2    = a0
          b1    = -2 * gama
          b2    = 2 * beta
          y     = a0 * x + a1 * x1 + a2 * x2 - b1 * y1 - b2 * y2
      in (y, hpfState x x1 y y1)

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

-- pkCoefs :: [(Float, Float)]
-- pkCoefs =
--   [ (0.997, 0.029591)  -- z0
--   , (0.985, 0.032534)  -- z1
--   , (0.950, 0.048056)  -- z2
--   , (0.850, 0.090579)  -- z3
--   , (0.620, 0.108990)  -- z4
--   , (0.250, 0.255784)  -- z5
--   ]

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