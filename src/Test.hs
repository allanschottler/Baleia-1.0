{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude
import Test.Hspec
import Baleia.Internal    -- for oscSin, lfoSin, amp, lin, runSF, etc.
import Baleia.Sample      -- for mono and sample arithmetic
import Control            (initControl)

-- A simple helper to “simulate” an SF over a list of inputs.
simulate :: SF a b -> [a] -> [b]
simulate _ []     = []
simulate sf (x:xs) =
  let (y, sf') = runSF sf x
  in y : simulate sf' xs

main :: IO ()
main = hspec $ do

  describe "amp" $ do
    it "multiplies the input sample by the gain" $ do
      -- Here we treat the second component as (gain, sample)
      let ctrl = initControl
          gain = 0.5
          input = (ctrl, (gain, (1,1)))
          (outCtrl, outSample) = fst (runSF amp input)
      outSample `shouldBe` (mono gain * (1,1))

  describe "lin" $ do
    it "linearly interpolates between a given min and max" $ do
      let ctrl   = initControl
          minVal = 60
          maxVal = 120
          -- Feed a normalized factor 0.5 with an arbitrary sample (0,0)
          input = (ctrl, (0.5, (0,0)))
          ((_, (linVal, sample)), _) = runSF (lin minVal maxVal) input
      linVal `shouldBe` (minVal + (maxVal - minVal) * 0.5)
      sample `shouldBe` (0,0)

  describe "oscSin" $ do
    it "produces a zero output for a zero frequency input" $ do
      let input = (0.0, (0,0))  -- frequency = 0 → no phase increment
          ((oscVal, _), _) = runSF oscSin input
      -- sinWave[0] is 0, so we expect a near-zero value.
      oscVal `shouldSatisfy` (\x -> abs x < 1e-6)

    it "changes output when using a nonzero frequency" $ do
      let inputs = replicate 10 (440.0, (0,0))  -- use 440 Hz as the test frequency
          outputs = simulate oscSin inputs
          samples = map (\(v, _) -> v) outputs
      -- With a nonzero frequency the output should vary.
      (minimum samples) `shouldSatisfy` (< maximum samples)
    
    it "oscillates between -1 and 1" $ do
      let inputs = replicate 10 (10000.0, (0,0))  -- use 440 Hz as the test frequency
          outputs = simulate oscSin inputs
          samples = map (\(v, _) -> v) outputs
      -- The output should oscillate between -1 and 1.
      minimum samples `shouldSatisfy` (\x -> x >= -1.0)
      maximum samples `shouldSatisfy` (\x -> x <= 1.0)

  describe "lfoSin" $ do
    it "returns a base value of 0.5 when frequency is zero" $ do
      let input = (0.0, (0,0))
          ((lfoVal, _), _) = runSF lfoSin input
      lfoVal `shouldSatisfy` (\x -> abs (x - 0.5) < 1e-6)

    it "oscillates when using a nonzero frequency" $ do
      let inputs = replicate 10 (10000.0, (0,0))  -- using a high frequency
          outputs = simulate lfoSin inputs
          lfoVals = map (\(v, _) -> v) outputs
      minimum lfoVals `shouldSatisfy` (< maximum lfoVals)

    it "oscillates between 0 and 1" $ do 
      let inputs = replicate 10 (10000.0, (0,0))
          outputs = simulate lfoSin inputs
          lfoVals = map (\(v, _) -> v) outputs
      minimum lfoVals `shouldSatisfy` (>= 0.0)
      maximum lfoVals `shouldSatisfy` (<= 1.0)
      

