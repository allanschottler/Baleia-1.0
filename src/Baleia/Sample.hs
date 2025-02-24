module Baleia.Sample
  ( Sample(..)
  , rawToMono
  , rawToStereo
  , monoToStereo
  , stereoToMono
  , stereoToFloat
  , monoToFloat 
  , safeMono 
  , left
  , right
--   , max
  , fpan
  ) where

import Baleia.Math ( pi_c )
import Foreign.Storable
import Foreign.Ptr

data Sample = Silence 
            | Mono Float 
            | Stereo Float Float 
            deriving (Show, Eq)

-- Conversion functions
rawToMono :: Float -> Sample
rawToMono = Mono

rawToStereo :: Float -> Float -> Sample
rawToStereo = Stereo 

monoToStereo :: Sample -> Sample
monoToStereo Silence   = Stereo 0 0
monoToStereo (Mono a)  = Stereo a a
monoToStereo s         = s

stereoToMono :: Sample -> Sample
stereoToMono Silence      = Mono 0
stereoToMono (Stereo a b) = Mono ((a+b) / 2)
stereoToMono s            = s

stereoToFloat :: Sample -> (Float, Float)
stereoToFloat Silence       = (0,0)
stereoToFloat (Mono a)      = (a,a)
stereoToFloat (Stereo a b)  = (a,b)

monoToFloat :: Sample -> Float
monoToFloat Silence   = 0
monoToFloat (Mono a)  = a
monoToFloat (Stereo a _) = a  -- or handle as you see fit

safeMono :: Sample -> Float
safeMono Silence      = 0
safeMono (Mono a)     = a
safeMono (Stereo a b) = (a + b) / 2

left :: Sample -> Sample
left Silence        = Silence
left (Mono a)       = Mono a
left (Stereo a _)   = Mono a

right :: Sample -> Sample
right Silence       = Silence
right (Mono a)      = Mono a
right (Stereo _ b)  = Mono b

max :: Sample -> Sample -> Sample
max s1 s2 = fromStereo (Prelude.max a1 b1, Prelude.max a2 b2)
  where
    (a1, a2) = toStereo s1
    (b1, b2) = toStereo s2

-- Pan functions
panCoef :: Float -> Sample
panCoef p = Stereo (cos pf) (sin pf)
  where pf = p * pi_c * 0.5

fpan :: Float -> Sample -> Sample
fpan p s = panCoef p * s

-- Helper functions for arithmetic
toStereo :: Sample -> (Float, Float)
toStereo Silence      = (0, 0)
toStereo (Mono a)     = (a, a)
toStereo (Stereo a b) = (a, b)

-- Optionally, convert a stereo pair back to a Sample.
-- Here we “compress” (a,a) back to Mono and treat (0,0) as Silence.
fromStereo :: (Float, Float) -> Sample
fromStereo (0, 0) = Silence
fromStereo (a, b)
  | a == b    = Mono a
  | otherwise = Stereo a b

-- Lift a binary operation to operate on Samples via their stereo representations.
liftOp :: (Float -> Float -> Float) -> Sample -> Sample -> Sample
liftOp op s1 s2 = fromStereo (op a1 b1, op a2 b2)
  where
    (a1, a2) = toStereo s1
    (b1, b2) = toStereo s2

instance Num Sample where
  (+) = liftOp (+)
  (-) = liftOp (-)
  (*) = liftOp (*)
  abs s = fromStereo (abs a, abs b)
    where (a, b) = toStereo s
  signum s = fromStereo (signum a, signum b)
    where (a, b) = toStereo s
  fromInteger i = Mono (fromInteger i)

instance Fractional Sample where
  (/) s1 s2 
    | isSilence s2 = Silence
    | otherwise    = fromStereo (a1 / b1, a2 / b2)
    where
      (a1, a2) = toStereo s1
      (b1, b2) = toStereo s2
  fromRational r = Mono (fromRational r)

isSilence :: Sample -> Bool
isSilence Silence = True
isSilence _       = False

instance Storable Sample where
  sizeOf _ = 2 * sizeOf (undefined :: Float)
  alignment _ = alignment (undefined :: Float)
  peek ptr = do
    f1 <- peek (castPtr ptr)
    f2 <- peek (castPtr ptr `plusPtr` sizeOf (undefined :: Float))
    return (Stereo f1 f2)
  poke ptr Silence = do
    poke (castPtr ptr) (0 :: Float)
    poke (castPtr ptr `plusPtr` sizeOf (undefined :: Float)) (0 :: Float)
  poke ptr (Mono f) = do
    poke (castPtr ptr) f
    poke (castPtr ptr `plusPtr` sizeOf (undefined :: Float)) f
  poke ptr (Stereo f1 f2) = do
    poke (castPtr ptr) f1
    poke (castPtr ptr `plusPtr` sizeOf (undefined :: Float)) f2
