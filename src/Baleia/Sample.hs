{-# LANGUAGE FlexibleInstances #-}
module Baleia.Sample where

type Sample = (Float,Float)

instance Num (Float, Float) where
  (a1, b1) + (a2, b2) = (a1 + a2, b1 + b2)
  (a1, b1) - (a2, b2) = (a1 - a2, b1 - b2)
  (a1, b1) * (a2, b2) = (a1 * a2, b1 * b2)
  negate (a, b)       = (negate a, negate b)
  abs (a, b)          = (abs a, abs b)
  signum (a, b)       = (signum a, signum b)
  fromInteger n       = (fromInteger n, fromInteger n)

instance Fractional (Float, Float) where
  (a1, b1) / (a2, b2) = (a1 / a2, b1 / b2)
  recip (a, b)        = (recip a, recip b)
  fromRational r      = (fromRational r, fromRational r)
  
monos :: Float -> Sample
monos x = (x, x)