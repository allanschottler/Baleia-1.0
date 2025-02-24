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


--------------------------------------------------------------------------------
-- Generic Binder (Bound Function Style)
--------------------------------------------------------------------------------

-- | bindParam' takes a projection function and an unbound SF
--   (of type SF (Float, (Float,Float)) (Float,Float)) and returns a function
--   from Control to a bound SF (of type SF (Float,Float) (Float,Float)).
bindParam' :: (Control -> a)
           -> SF (a, Sample) Sample
           -> (IORef Control -> SF Sample Sample)
bindParam' proj sf ctrl = SF $ \(s :: Sample) ->
  let ctrl' = unsafePerformIO $ readIORef ctrl
      p   = proj ctrl'
      inp = (p, s)
      (out, sfNext) = runSF sf inp
  in (out, bindParam' proj sfNext ctrl)

-- | Infix version.
infixl 9 ~~
(~~) :: SF (a, Sample) Sample
      -> (Control -> a)
      -> (IORef Control -> SF Sample Sample)
sf ~~ proj = bindParam' proj sf
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

liftSF :: (a -> b -> b) -> SF (a, b) b
liftSF f = sf
  where sf = SF $ \(v, s) -> (f v s, sf)
{-# INLINE liftSF #-}

liftSF2 :: (a -> b -> c -> c) -> SF (a, b, c) c
liftSF2 f = sf
  where sf = SF $ \(v1, v2, s) -> (f v1 v2 s, sf)
{-# INLINE liftSF2 #-}

liftSF3 :: (a -> b -> c -> d -> d) -> SF (a, b, c, d) d
liftSF3 f = sf
  where sf = SF $ \(v1, v2, v3, s) -> (f v1 v2 v3 s, sf)
{-# INLINE liftSF3 #-}

amp :: SF (Float, Sample) Sample
amp = liftSF $ \gain s -> s * Mono gain
{-# INLINE amp #-}

clip :: SF (Float, Sample) Sample
clip = liftSF $ \thrsh s -> 
  case s of
    Mono a -> Mono $ clipi thrsh a
    Stereo a b -> Stereo (clipi thrsh a) (clipi thrsh b)
    Silence -> Silence
  where clipi thrsh v | abs v > thrsh = thrsh * signum v 
                      | otherwise     = v
{-# INLINE clip #-}

mix :: SF (Float, Sample, Sample) Sample
mix = liftSF2 $ \gain a b -> lerp a b $ realToFrac gain
{-# INLINE mix #-}