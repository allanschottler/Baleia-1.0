{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
module Baleia.Internal where

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Control.Category
import Prelude hiding ((.), id)
import Control.Arrow
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Storable (Storable)
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

instance Functor (SF a) where
  fmap f (SF g) = SF $ \a ->
    let (b, sf') = g a
    in (f b, fmap f sf')
  {-# INLINE fmap #-}

instance Applicative (SF a) where
  pure b = SF $ \_ -> (b, pure b)
  (SF sf) <*> (SF sg) = SF $ \a ->
    let (f, sf') = sf a
        (b, sg') = sg a
    in (f b, sf' <*> sg')
  {-# INLINE (<*>) #-}


type CV a = (Control -> a)
-- data CV a = Static a | Dynamic (Control -> a)

--------------------------------------------------------------------------------
-- Generic Binder (Bound Function Style)
--------------------------------------------------------------------------------

bindParam' :: CV a
          -> SF (a, Sample) Sample
          -> (IORef Control -> SF Sample Sample)
bindParam' cv sf ctrl = SF $ \(s :: Sample) ->
  let p   = cv $ unsafePerformIO $ readIORef ctrl
      inp = (p, s)
      (out, sfNext) = runSF sf inp
  in (out, bindParam' cv sfNext ctrl)

-- | Infix version.
infixl 9 ~~
(~~) :: SF (a, Sample) Sample
      -> CV a
      -> (IORef Control -> SF Sample Sample)
sf ~~ cv = bindParam' cv sf
{-# INLINE (~~) #-}

--------------------------------------------------------------------------------
-- Composition Operator for Bound Functions
--------------------------------------------------------------------------------

infixr 8 <~
(<~) :: (IORef Control -> SF Sample Sample)
       -> (IORef Control -> SF Sample Sample)
       -> (IORef Control -> SF Sample Sample)
f <~ g = \ctrl -> (f ctrl) <<< (g ctrl)
{-# INLINE (<~) #-}

--------------------------------------------------------------------------------
-- Block Processing (Pure)
--------------------------------------------------------------------------------
-- amp 2 <~ lpf _Freq _Q <~ mix _Drywet <~ amp _Gain &~ id_

--  = amp 2 
-- <~ lpf _Freq _Q 
-- <~ mix _Drywet 
--   <~ amp _Gain 
--   &~ id_

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

liftSF0 :: (Sample -> Sample) -> SF Sample Sample
liftSF0 f = sf
  where sf = SF $ \s -> (f s, sf)
{-# INLINE liftSF0 #-}

liftSF1 :: (a -> Sample -> Sample) -> SF (a, Sample) Sample
liftSF1 f = sf
  where sf = SF $ \(v, s) -> (f v s, sf)
{-# INLINE liftSF1 #-}

liftSF2 :: (a -> b -> Sample -> Sample) -> SF (a, b, Sample) Sample
liftSF2 f = sf
  where sf = SF $ \(v1, v2, s) -> (f v1 v2 s, sf)
{-# INLINE liftSF2 #-}

liftSF3 :: (a -> b -> c -> Sample -> Sample) -> SF (a, b, c, Sample) Sample
liftSF3 f = sf
  where sf = SF $ \(v1, v2, v3, s) -> (f v1 v2 v3 s, sf)
{-# INLINE liftSF3 #-}

id_ :: SF Sample Sample
id_ = liftSF0 $ id
{-# INLINE id_ #-}

amp :: SF (Float, Sample) Sample
amp = liftSF1 $ \gain s -> s * Mono gain
{-# INLINE amp #-}

clip :: SF (Float, Sample) Sample
clip = liftSF1 $ \thrsh s -> 
  case s of
    Mono a -> Mono $ clipi thrsh a
    Stereo a b -> Stereo (clipi thrsh a) (clipi thrsh b)
    Silence -> Silence
  where clipi thrsh v | abs v > thrsh = thrsh * signum v 
                      | otherwise     = v
{-# INLINE clip #-}

pan :: SF (Float, Sample) Sample
pan = liftSF1 $ \p -> Baleia.Sample.fpan p
{-# INLINE pan #-}

mix :: SF (Sample, Sample) Sample
mix = liftSF1 $ \a b -> (a + b) * 0.5
{-# INLINE mix #-}