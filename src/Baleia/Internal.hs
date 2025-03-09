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

type CV a = (Control -> a)
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

id_ :: SF (Control, Sample) (Control, Sample)
id_ = arr $ \a -> a
{-# INLINE id_ #-}

amp :: CV Float -> SF (Control, Sample) (Control, Sample)
amp gain = arr $ \(c, (l,r)) -> (c, (l * (gain c), r * (gain c)))
{-# INLINE amp #-}

clip :: CV Float -> SF (Control, Sample) (Control, Sample)
clip thrsh = arr $ \(c, (l,r)) -> (c, (clipi (thrsh c) l, clipi (thrsh c) r))
  where clipi t v | abs v > t = t * signum v 
                  | otherwise = v
{-# INLINE clip #-}

mix :: CV Float -> SF ((Control, Sample), (Control, Sample)) (Control, Sample)
mix gain = arr $ \((c1, (l1,r1)), (c2, (l2,r2))) -> (c1 {- mix out c1 c2 -}, (l1 * (1 - gain c1) + l2 * (gain c1), r1 * (1 - gain c1) + r2 * (gain c1)))
{-# INLINE mix #-}

switch :: CV Bool -> SF ((Control, Sample), (Control, Sample)) (Control, Sample)
switch on = arr $ \((c1,a), (c2,b)) -> if on c1 then (c1,a) else (c2,b)
{-# INLINE switch #-}

--------------------------------------------------------------------------------
-- Block Processing
--------------------------------------------------------------------------------

processBlock :: forall a b. (Storable a, Storable b)
                => SF a b -> V.Vector a -> (V.Vector b, SF a b)
processBlock sf vec =
  let n = V.length vec
      (ys, finalSF) = go 0 sf [] n
  in (V.fromList (reverse ys), finalSF)
  where
    go :: Int -> SF a b -> [b] -> Int -> ([b], SF a b)
    go i currentSF acc n
      | i >= n    = (acc, currentSF)
      | otherwise =
          let (y, nextSF) = runSF currentSF (vec V.! i)
          in go (i + 1) nextSF (y : acc) n
{-# INLINE processBlock #-}

--------------------------------------------------------------------------------
-- Delay
--------------------------------------------------------------------------------

data Delay = Delay { dBuf :: MV.IOVector Sample, dIdx :: Int, dDelay :: Float, dXFade :: Maybe (Int, Int, Sample) }

processFdbkDelay :: Delay -> Sample -> Float -> (Sample, Delay)
processFdbkDelay st x fbGain = unsafePerformIO $ do
  let buf = dBuf st
      idx = dIdx st
  y <- MV.read buf idx
  let newVal = ( fst x + fbGain * fst y
               , snd x + fbGain * snd y )
  MV.write buf idx newVal
  let newIdx = (idx + 1) `mod` MV.length buf
  return (y, st { dIdx = newIdx })
{-# NOINLINE processFdbkDelay #-}

--------------------------------------------------------------------------------
-- Feedback Delay
--------------------------------------------------------------------------------

fdbkDelay :: Int -> CV Float -> SF (Control, Sample) (Control, Sample)
fdbkDelay n fbGain = unsafePerformIO $ do
  buf <- MV.new n
  MV.set buf (0,0)
  let initDelay = Delay { dBuf = buf, dIdx = 0, dDelay = 0, dXFade = Nothing }
  return (fdbkDelay' initDelay)
  where
    fdbkDelay' st = SF $ \(c, s) ->
      let (y, st') = processFdbkDelay st s $ fbGain c
      in ((c, y), fdbkDelay' st')
{-# NOINLINE fdbkDelay #-}
