{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
module Start where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.StablePtr
import Data.List.Split
import Foreign.Storable.Tuple
import qualified Data.Vector.Storable as V
import Baleia.Sample    -- For stereo/sample helpers
import Baleia.Meta      -- Provides Control, initControl, etc.
import Baleia.Time
import Baleia.Internal
import Control.Arrow hiding (left,right)
import Prelude hiding (length)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe
import Control

--------------------------------------------------------------------------------
-- Control Introspection and Conversion
--------------------------------------------------------------------------------

ctrlFields' = $(introspect ''Control)

pairEvery' :: [String] -> [(String, String)]
pairEvery' (x:y:xs) = (x,y) : pairEvery' xs
pairEvery' []         = []

ctrlFields = pairEvery' $ split (dropBlanks . dropDelims $ oneOf "()[],\"") ctrlFields'

foreign export ccall controlInMeta :: IO CString
controlInMeta = newCString $ go ctrlFields
  where
    go [] = ""
    go ((n,t):xs) =
      n ++ " " ++ t ++ " " ++ p ++ " " ++ go xs
      where p | t == "Bool" = show (getField n initControl :: Bool)
              | otherwise   = show (getField n initControl :: Float)

raw2ctrl' :: [Float] -> Control
raw2ctrl' values = foldr f initControl zs
  where
    zs = zipWith (\(a,b) v -> (a,b,v)) ctrlFields values
    f (n,t,v) s | t == "Bool" = setField n (v >= 0.5) s
                | otherwise   = setField n v s

poke2 :: (Storable a) => Ptr a -> Ptr a -> V.Vector (a,a) -> IO ()
poke2 ptra ptrb vec = go vec 0
  where
    go v n | V.null v = return ()
           | otherwise = do
               let (a,b) = V.head v
               pokeElemOff ptra n a
               pokeElemOff ptrb n b
               go (V.tail v) (n+1)

--------------------------------------------------------------------------------
-- Main process
--------------------------------------------------------------------------------

process :: SF (Control, Sample) (Control, Sample)
process = clip _Clip <<< amp _Gain <<< mix _DryWet <<< id_ &&& (fdbkDelay (3 beats `div` 4) _Fdbk1 <<< fdbkDelay (18 ms) _Fdbk2)
-- process = clip _Clip <<< amp _Gain <<< mix _DryWet <<< id_ &&& (fdbkDelay (3 beats `div` 4) _Fdbk1 <<< fdbkDelayVarInterpCrossFade 44100 _Fdbk2 ((\x -> 1 + 1000 * x) . _Time))
{-# INLINE process #-}

--------------------------------------------------------------------------------
-- FFI Exports: startCtx and freeCtx
--------------------------------------------------------------------------------

type DSPState = SF (Control, Sample) (Control, Sample)

foreign export ccall startCtx :: IO (StablePtr DSPState)
startCtx :: IO (StablePtr DSPState)
startCtx = newStablePtr process

foreign export ccall freeCtx :: StablePtr DSPState -> IO ()
freeCtx :: StablePtr DSPState -> IO ()
freeCtx = freeStablePtr

--------------------------------------------------------------------------------
-- FFI Export: main2
--------------------------------------------------------------------------------

foreign export ccall main2 :: StablePtr DSPState -> Int -> Int ->
                              Ptr Float -> Ptr Float ->
                              Ptr Float -> Ptr Float ->
                              Ptr Float -> Ptr Float -> IO (StablePtr DSPState)
main2 dspPtr ns np ci _co il ir ol or = do
  oldSF <- deRefStablePtr dspPtr
  ctrlList <- peekArray np ci
  let newCtrl = raw2ctrl' (map realToFrac ctrlList)
  leftList  <- peekArray ns il
  rightList <- peekArray ns ir
  let leftIn   = V.fromList (map realToFrac leftList) :: V.Vector Float
      rightIn  = V.fromList (map realToFrac rightList) :: V.Vector Float
      stereoIn = V.zipWith (,) leftIn rightIn :: V.Vector Sample
      (stereoOut, sfNext) = processBlock oldSF ((V.zipWith (,) (V.replicate ns newCtrl) stereoIn) :: V.Vector (Control,Sample))
      leftOut  = V.map (fst . snd) stereoOut :: V.Vector Float
      rightOut = V.map (snd . snd) stereoOut :: V.Vector Float
  poke2 ol or (V.fromList $ zip (V.toList leftOut) (V.toList rightOut))
  newStablePtr sfNext
