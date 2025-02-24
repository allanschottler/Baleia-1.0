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
import Baleia.Delay
import Control.Arrow (arr, (<<<))
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

vzipWith :: (Storable a, Storable b, Storable c)
         => (a -> b -> c) -> V.Vector a -> V.Vector b -> V.Vector c
vzipWith f va vb = V.fromList $ zipWith f (V.toList va) (V.toList vb)

--------------------------------------------------------------------------------
-- Main process
--------------------------------------------------------------------------------

process = do
  d1 <- fdbkDelay (3 beats `div` 4)
  d2 <- fdbkDelay (18 ms)
  return $ clip ~~ _Clip 
        <~ amp ~~ _Gain 
        <~ d2 ~~ _Freq 
        <~ d1 ~~ _Feedback
{-# INLINE process #-}

--------------------------------------------------------------------------------
-- FFI Exports: startCtx and freeCtx
--------------------------------------------------------------------------------

-- | Our external DSP state is a pair:
--   (IORef Control, SF (Float,Float) (Float,Float))
type DSPState = (IORef Control, SF Sample Sample)

foreign export ccall startCtx :: IO (StablePtr DSPState)
startCtx :: IO (StablePtr DSPState)
startCtx = do
  chain <- process
  ctrlRef <- newIORef initControl
  newStablePtr (ctrlRef, chain ctrlRef)

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
  (ctrlRef, oldSF) <- deRefStablePtr dspPtr
  ctrlList <- peekArray np ci  -- [Float]
  let newCtrl = raw2ctrl' (map realToFrac ctrlList)
  writeIORef ctrlRef newCtrl
  leftList  <- peekArray ns il
  rightList <- peekArray ns ir
  let leftIn   = V.fromList (map realToFrac leftList) :: V.Vector Float
      rightIn  = V.fromList (map realToFrac rightList) :: V.Vector Float
      stereoIn = V.fromList $ zipWith Stereo (V.toList leftIn) (V.toList rightIn) :: V.Vector Sample
      (stereoOut, sfNext) = processBlock oldSF stereoIn
      leftOut  = V.map (realToFrac . monoToFloat . left) stereoOut :: V.Vector Float
      rightOut = V.map (realToFrac . monoToFloat . right) stereoOut :: V.Vector Float
  poke2 ol or (V.fromList $ zip (V.toList leftOut) (V.toList rightOut))
  newStablePtr (ctrlRef, sfNext)
