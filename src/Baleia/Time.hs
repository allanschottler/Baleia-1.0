{-# LANGUAGE FlexibleInstances #-}
module Baleia.Time where

instance Num (Integer -> Integer) where
    fromInteger n = \scale -> n * scale
    (+) = undefined
    (-) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined    

sampleRate, ms, seconds, minutes, tempo, beats, bars :: Integer

sampleRate = 44100 --ffi import
tempo = 150 --ffi import
ms = sampleRate `div` 1000
seconds = 1000 ms
minutes = 60 seconds
beats = (1 minutes) `div` tempo
bars = 4 beats

--fromHz hz = seconds $ 1.0/hz