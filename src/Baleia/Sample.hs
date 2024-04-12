module Baleia.Sample   
( Sample(Silence,Mono,Stereo)
, fmaps
, rawToMono
, rawToStereo
, monoToStereo
, stereoToMono
, stereoToFloat
, monoToFloat 
, left
, right
, Baleia.Sample.max
, fpan
) where

import Baleia.Math ( pi_c )
import Foreign.Storable
import Foreign.Ptr


data Sample = Silence 
            | Mono Float 
            | Stereo Float Float 
            deriving (Show)

--type Sample = Sample' Float

--TODO: this shouldnt be necessary if Sample becomes Functor instance
fmaps :: (Float -> Float) -> Sample -> Sample
fmaps f (Mono a) = Mono (f a)
fmaps f (Stereo a b) = Stereo (f a) (f b)

rawToMono :: Float -> Sample
rawToMono = Mono

rawToStereo :: Float -> Float -> Sample
rawToStereo = Stereo 

monoToStereo :: Sample -> Sample
monoToStereo Silence = Stereo 0 0
monoToStereo (Mono a) = Stereo a a --let s = a/2 in Stereo s s --stimmt das?
monoToStereo s = s

stereoToMono :: Sample -> Sample
stereoToMono Silence = Mono 0
stereoToMono (Stereo a b) = Mono ((a+b)/2)
stereoToMono s = s

stereoToFloat :: Sample -> (Float, Float)
stereoToFloat Silence = (0,0)
stereoToFloat (Stereo a b) = (a,b)
stereoToFloat (Mono a) = (a,a)

monoToFloat :: Sample -> Float
monoToFloat Silence = 0
monoToFloat (Mono a) = a

left :: Sample -> Sample
left Silence = Silence
left (Stereo a b) = Mono a
left (Mono a) = Mono a

right :: Sample -> Sample
right Silence = Silence
right (Stereo a b) = Mono b
right (Mono a) = Mono a

max :: Sample -> Sample -> Sample
max (Mono a) (Mono b) = Mono (Prelude.max a b)
max (Stereo a b) (Stereo c d) = Stereo (Prelude.max a c) (Prelude.max b d)
max a b = Baleia.Sample.max (monoToStereo a) (monoToStereo b)

panCoef :: Float -> Sample
panCoef p = let pf = p * pi_c * 0.5
            in Stereo (cos pf) (sin pf)

fpan :: Float -> Sample -> Sample
fpan p s = (panCoef p) * s
    
instance Num Sample where
    Mono a + Mono b = Mono (a+b)
    Stereo a b + Mono c = Stereo (a+c) (b+c)
    Mono c + Stereo a b = Stereo (c+a) (c+b)
    {--Mono c + s = fmaps (+c) s
    s + Mono c = fmaps (+c) s--}
    Stereo a b + Stereo c d = Stereo (a+c) (b+d)
    Silence + a = a
    a + Silence = a
    
    Mono a * Mono b = Mono (a*b)
    Stereo a b * Mono c = Stereo (a*c) (b*c)
    Mono c * Stereo a b = Stereo (c*a) (c*b)
    Stereo a b * Stereo c d = Stereo (a*c) (b*d)
    Silence * a = Silence
    a * Silence = Silence
    
    Mono a - Mono b = Mono (a-b)
    Stereo a b - Mono c = Stereo (a-c) (b-c)
    Mono c - Stereo a b = Stereo (c-a) (c-b)
    Stereo a b - Stereo c d = Stereo (a-c) (b-d)
    Silence - a = -a
    a - Silence = a
    
    abs (Mono a) = Mono (abs a)
    abs (Stereo a b) = Stereo (abs a) (abs b)  
    abs Silence = Silence
    
    signum (Mono a) = Mono (signum a)
    signum (Stereo a b) = Stereo (signum a) (signum b)
    signum Silence = Silence
    
    fromInteger i = Mono (fromInteger i)

instance Fractional Sample where
    Mono a / Mono b = Mono (a/b)
    Stereo a b / Mono c = Stereo (a/c) (b/c)
    Mono c / Stereo a b = Stereo (c/a) (c/a)
    Stereo a b / Stereo c d = Stereo (a/c) (b/d)
    Silence / a = Silence
    a / Silence = Silence
    
    fromRational r = Mono (fromRational r)

instance Storable Sample where
    sizeOf _ = sizeOf (undefined :: Float) * 2
    alignment _ = alignment (undefined :: Float)
    peek ptr = do
        f1 <- peek (castPtr ptr)
        f2 <- peek (castPtr ptr `plusPtr` sizeOf (undefined :: Float))
        return (Stereo f1 f2)
    poke ptr Silence = return ()
    poke ptr (Mono f) = poke (castPtr ptr) f
    poke ptr (Stereo f1 f2) = do
        poke (castPtr ptr) f1
        poke (castPtr ptr `plusPtr` sizeOf (undefined :: Float)) f2