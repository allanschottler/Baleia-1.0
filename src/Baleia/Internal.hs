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
import Control

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

-- type CV a = (Control -> a)

-- type SYSignal = SF Control (Control, Sample)
-- type Signal = SF (Control, Sample) (Control, Sample)
-- type CVSignal a = SF (Control, a) (Control, a)

-- type CV2 a = SF Control a
-- data CV a = Static a | Dynamic (Control -> a)

-- mapSF :: Arrow arr => (b -> c) -> arr a b -> arr a c
-- mapSF f a = arr f <<< a
-- {-# INLINE mapSF #-}

-- map2SF :: Arrow arr => ((b, c) -> d) -> arr a b -> arr a c -> arr a d
-- map2SF f a b = arr f <<< a &&& b
-- {-# INLINE map2SF #-}

-- map3SF :: Arrow arr => ((b, c, d) -> e) -> arr a b -> arr a c -> arr a d -> arr a e
-- map3SF f a b c = arr (\((x, y), z) -> f (x, y, z)) <<< ((a &&& b) &&& c)
-- {-# INLINE map3SF #-}


-- id_ :: SF a a
-- id_ = arr id
-- {-# INLINE id_ #-}

-- id2_ :: SF ((Control, Sample), (Control, Sample)) ((Control, Sample), (Control, Sample))
-- id2_ = arr id
-- {-# INLINE id2_ #-}

-- const_ :: b -> SF a b
-- const_ b = arr $ const b
-- {-# INLINE const_ #-}

-- id2_ :: SF ((Control, Sample), (Control, Sample)) (Control, Sample)
-- id2_ = arr $ \a -> a
-- {-# INLINE id_ #-}

-- amp :: CV Float -> SF (Control, Sample) (Control, Sample)
-- amp gain = arr $ \(c, (l,r)) -> (c, (l * (gain c), r * (gain c)))
-- {-# INLINE amp #-}

-- liftCV :: CV Float -> SF (Control, Sample) (Control, (Float, Sample))
-- liftCV :: CV Float -> SF (Control, Sample) (Control, (Float, Sample))
-- liftCV f = arr (\(c, s) -> (c, (f c, s)))
-- {-# INLINE liftCV #-}

-- cv = liftCV
-- {-# INLINE cv #-}

-- vc = liftCV . const
-- {-# INLINE vc #-} 

-- amp :: SF (Control, Sample) (Control, Sample) -> SF (Control, Sample) (Control, Sample)
-- amp gain = map2SF (\((c, g), (_, s)) -> (c, g * s)) gain id_ 
-- {-# INLINE amp #-}

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

--------------------------------------------------------------------------------
-- Feedback Delay
--------------------------------------------------------------------------------

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
lfoSin = unipol <<< oscWavetable sinWave
{-# INLINE lfoSin #-}
lfoSqr = unipol <<< oscWavetable sqrWave
{-# INLINE lfoSqr #-}
lfoTri = unipol <<< oscWavetable triWave
{-# INLINE lfoTri #-}
lfoSaw = unipol <<< oscWavetable sawWave
{-# INLINE lfoSaw #-}

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