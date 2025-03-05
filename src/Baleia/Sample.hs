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
