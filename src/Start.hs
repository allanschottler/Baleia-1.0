{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Start where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.StablePtr
import Data.List.Split
import Foreign.Storable.Tuple

-- We use Data.Vector.Storable for vector operations.
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

import Control           -- Provides Control, raw2ctrl', poke2, initControl, etc.
import Baleia.Sample    -- For stereo/sample helpers
import Baleia.Meta
import Baleia.SFPrototype  -- Provides SF, processVectorSFBufStereo, exampleChainStereo, etc.
import Control.Arrow
import Prelude hiding (length)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- Introspect control fields.
ctrlFields' = $(introspect ''Control)

pairEvery' :: [String] -> [(String, String)]
pairEvery' (x:y:xs) = (x, y) : pairEvery' xs
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

poke2 :: (Storable a) => Ptr a -> Ptr a -> V.Vector (a, a) -> IO ()
poke2 ptra ptrb vec = go vec 0
  where
    go v n | V.null v = return ()
           | otherwise = do
               let (a, b) = V.head v
               pokeElemOff ptra n a
               pokeElemOff ptrb n b
               go (V.tail v) (n+1)

--------------------------------------------------------------------------------
-- Helper: vzipWith
--------------------------------------------------------------------------------
vzipWith :: (Storable a, Storable b, Storable c)
         => (a -> b -> c) -> V.Vector a -> V.Vector b -> V.Vector c
vzipWith f va vb = V.fromList $ zipWith f (V.toList va) (V.toList vb)

--------------------------------------------------------------------------------
-- DSPState and SF Creation (Stereo)
--------------------------------------------------------------------------------

-- | DSPState now holds a Control, a stereo-processing SF, and an IORef for a reusable buffer.
type DSPState = (Control, SF (Float, Float) (Float, Float))

-- | Build an SF from the current control.
processSFStereo :: Control -> SF (Float, Float) (Float, Float)
processSFStereo ctrl = stereoDelaySFPure 44100 <<< stereoDelaySFPure 44100  -- Replace with your actual chain

-- | A placeholder stereo SF that passes through its input.
exampleChainStereo :: SF (Float, Float) (Float, Float)
exampleChainStereo = arr id
{-# INLINE exampleChainStereo #-}


processVectorSFPure :: forall a b. (Storable a, Storable b)
                    => SF a b -> V.Vector a -> (V.Vector b, SF a b)
processVectorSFPure sf vec =
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
{-# INLINE processVectorSFPure #-}


--------------------------------------------------------------------------------
-- FFI Exports: startCtx and freeCtx
--------------------------------------------------------------------------------

foreign export ccall startCtx :: IO (StablePtr DSPState)
startCtx :: IO (StablePtr DSPState)
startCtx = do
  newStablePtr (initControl, processSFStereo initControl)

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
main2 :: StablePtr DSPState -> Int -> Int ->
         Ptr Float -> Ptr Float ->
         Ptr Float -> Ptr Float ->
         Ptr Float -> Ptr Float -> IO (StablePtr DSPState)
main2 dspPtr ns np ci _co il ir ol or = do
  (oldCtrl, oldSF) <- deRefStablePtr dspPtr
  let n = ns
  leftList  <- peekArray n il
  rightList <- peekArray n ir
  let leftIn   = V.fromList (map realToFrac leftList) :: V.Vector Float
      rightIn  = V.fromList (map realToFrac rightList) :: V.Vector Float
      stereoIn = vzipWith (,) leftIn rightIn :: V.Vector (Float, Float)
  ctrlList <- peekArray np ci
  let newCtrl = raw2ctrl' (map realToFrac ctrlList)
      (stereoOut, sfNext) = processVectorSFPure oldSF stereoIn
      leftOut  = V.map (realToFrac . fst) stereoOut :: V.Vector Float
      rightOut = V.map (realToFrac . snd) stereoOut :: V.Vector Float
  poke2 ol or (vzipWith (,) leftOut rightOut)
  newStablePtr (newCtrl, sfNext)
