module Baleia.Math where

pi_c :: Float
pi_c = 3.14159265358979323846

lerp a b f = a * (1-f) + b * f

getDecimal :: Float -> Float
getDecimal = snd . properFraction
