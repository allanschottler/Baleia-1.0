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
-- (No need for Data.Vector as a separate import now)
import Baleia.Sample    -- For stereo/sample helpers
import Baleia.Meta      -- Provides Control, initControl, etc.
import Baleia.Time
import Baleia.Internal
import Control.Arrow hiding (left, right)
import Prelude hiding (length)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe
import Control

--------------------------------------------------------------------------------
-- Control Introspection and Conversion
--------------------------------------------------------------------------------

deriveStorable ''Control

ctrlFields' = $(introspect ''Control)

-- pairEvery: Convert a list of Strings (control metadata) into a
-- V.Vector of (CString, CString)
{-# NOINLINE pairEvery #-}
pairEvery :: [String] -> V.Vector (CString, CString)
pairEvery xs = unsafePerformIO $ do
  pairs <- go xs
  return $ V.fromList pairs
  where
    go (x:y:xs) = do
      xst <- newCString x
      yst <- newCString y
      rest <- go xs
      return $ (xst, yst) : rest
    go [] = return []

{-# NOINLINE ctrlFields #-}
ctrlFields :: V.Vector (CString, CString)
ctrlFields = pairEvery $ split (dropBlanks . dropDelims $ oneOf "()[],\"") ctrlFields'

-- Import for C's strcmp
foreign import ccall unsafe "string.h strcmp" c_strcmp :: CString -> CString -> CInt

{-# NOINLINE boolStr #-}
boolStr :: CString
boolStr = unsafePerformIO $ newCString "Bool"

-- Precompute control field info as a V.Vector pairing each field's name with
-- a flag indicating if its type is "Bool".  (This avoids repeatedly converting
-- the type field from a CString to a Haskell String.)
{-# NOINLINE ctrlFieldsInfo #-}
ctrlFieldsInfo :: V.Vector (CString, Bool)
ctrlFieldsInfo = V.map (\(name, typeStr) -> (name, c_strcmp typeStr boolStr == 0)) ctrlFields

-- Convert a vector of Float control values into a Control.
-- We use an index-based fold over the storable vector (which contains the new float
-- values) and look up the corresponding (name, isBool) flag from ctrlFieldsInfo.
raw2ctrl :: V.Vector Float -> Control
raw2ctrl values = V.ifoldl' (\s i v ->
    let (nameCString, isBool) = ctrlFieldsInfo V.! i
        name = unsafePerformIO (peekCString nameCString)
    in if isBool then setField name (v >= 0.5) s else setField name v s
  ) initControl values

-- poke2: an index-based loop for writing paired elements into two memory regions.
poke2 :: (Storable a) => Ptr a -> Ptr a -> V.Vector (a, a) -> IO ()
poke2 ptra ptrb vec = go 0
  where
    len = V.length vec
    go i | i >= len = return ()
         | otherwise = do
             let (a, b) = vec V.! i
             pokeElemOff ptra i a
             pokeElemOff ptrb i b
             go (i + 1)

--------------------------------------------------------------------------------
-- Main process
--------------------------------------------------------------------------------

process :: SF (Control, Sample) (Control, Sample)
process = clip _Clip <<< amp _Gain <<< mix _DryWet <<< id_ &&& (fdbkDelay (3 beats `div` 4) _Fdbk1 <<< fdbkDelay (18 ms) _Fdbk2)
{-# INLINE process #-}

--------------------------------------------------------------------------------
-- FFI Exports: startCtx and freeCtx using an IORef for state updates
--------------------------------------------------------------------------------

type DSPState = SF (Control, Sample) (Control, Sample)
type DSPStateRef = IORef DSPState

foreign export ccall startCtx :: IO (StablePtr DSPStateRef)
startCtx :: IO (StablePtr DSPStateRef)
startCtx = newStablePtr =<< newIORef process

foreign export ccall freeCtx :: StablePtr DSPStateRef -> IO ()
freeCtx :: StablePtr DSPStateRef -> IO ()
freeCtx = freeStablePtr

--------------------------------------------------------------------------------
-- FFI Export: main2 using IORef state update
--------------------------------------------------------------------------------

foreign export ccall main2 :: StablePtr DSPStateRef -> Int -> Int ->
                              Ptr Float -> Ptr Float ->
                              Ptr Float -> Ptr Float ->
                              Ptr Float -> Ptr Float -> IO ()
main2 dspPtr ns np ci _co il ir ol or = do
  ref <- deRefStablePtr dspPtr
  oldSF <- readIORef ref
  cptr <- newForeignPtr_ ci
  let newCtrl = raw2ctrl $ V.unsafeFromForeignPtr0 cptr np
  lptr <- newForeignPtr_ il
  rptr <- newForeignPtr_ ir
  let leftIn   = V.unsafeFromForeignPtr0 lptr ns
      rightIn  = V.unsafeFromForeignPtr0 rptr ns
      stereoIn = V.zipWith (,) leftIn rightIn
      (stereoOut, sfNext) = processBlock oldSF $ V.zipWith (,) (V.replicate ns newCtrl) stereoIn
      leftOut  = V.map (fst . snd) stereoOut
      rightOut = V.map (snd . snd) stereoOut
  poke2 ol or $ V.zipWith (,) leftOut rightOut
  writeIORef ref sfNext

-- FFI Export: controlInMeta now uses ctrlFieldsInfo.
-- It constructs a string that, for each control field, shows its name, its type
-- (determined from the Bool flag), and its current value in initControl.
foreign export ccall controlInMeta :: IO CString
controlInMeta :: IO CString
controlInMeta = newCString (go 0)
  where
    len = V.length ctrlFieldsInfo
    go i
      | i >= len  = ""
      | otherwise =
          let (nameCString, isBool) = ctrlFieldsInfo V.! i
              name = unsafePerformIO (peekCString nameCString)
              typeStr = if isBool then "Bool" else "Float"
              pStr = if isBool
                        then show (getField name initControl :: Bool)
                        else show (getField name initControl :: Float)
          in name ++ " " ++ typeStr ++ " " ++ pStr ++ " " ++ go (i + 1)
