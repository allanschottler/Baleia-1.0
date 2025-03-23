{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE Arrows #-}
module Start where

import Control.Exception (try, SomeException)
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.StablePtr
import Data.List.Split
import Foreign.Storable.Tuple
import qualified Data.Vector.Storable as V
import Baleia.Sample
import Baleia.Meta
import Baleia.Time
import Baleia.Internal
import Control.Arrow hiding (left, right)
import Prelude hiding (length)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe
import Control.Monad.IO.Class (liftIO)
import System.IO (appendFile)
import System.Directory (createDirectoryIfMissing)
import Control.Exception ( bracket )
import System.IO ( hPutStrLn, stderr )
import MyModule as MM

--------------------------------------------------------------------------------
-- Debug Logging (to file), creating the directory first
--------------------------------------------------------------------------------

-- logPath :: FilePath
-- logPath = "C:\\temp\\mydll.log"

-- debugLog :: String -> IO ()
-- debugLog s = do
--   createDirectoryIfMissing True "C:\\temp"
--   appendFile logPath (s ++ "\n")

--------------------------------------------------------------------------------
-- Control Introspection and Conversion
--------------------------------------------------------------------------------

deriveStorable ''Control

ctrlFields' = $(introspect ''Control)

{-# NOINLINE pairEvery #-}
pairEvery :: [String] -> V.Vector (CString, CString)
pairEvery xs = unsafePerformIO $ do
  pairs <- go xs
  return $ V.fromList pairs
  where
    go (x:y:xs) = do
      xst <- newCString x
      yst <- newCString (take 1 y)
      rest <- go xs
      return $ (xst, yst) : rest
    go [] = return []

{-# NOINLINE ctrlFields #-}
ctrlFields :: V.Vector (CString, CString)
ctrlFields = pairEvery $ split (dropBlanks . dropDelims $ oneOf "()[],\"") ctrlFields'

foreign import ccall unsafe "string.h strcmp" c_strcmp :: CString -> CString -> CInt

{-# NOINLINE boolStr #-}
boolStr :: CString
boolStr = unsafePerformIO $ newCString "B"

{-# NOINLINE ctrlFieldsInfo #-}
ctrlFieldsInfo :: V.Vector (CString, Bool)
ctrlFieldsInfo = V.map (\(name, typeStr) -> (name, c_strcmp typeStr boolStr == 0)) ctrlFields

raw2ctrl :: V.Vector Float -> Control
raw2ctrl values = V.ifoldl' (\s i v ->
    let (nameCString, isBool) = ctrlFieldsInfo V.! i
        name = unsafePerformIO (peekCString nameCString)
    in if isBool then setField name (v >= 0.5) s else setField name v s
  ) MM.initControl values
{-# INLINE raw2ctrl #-}

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
{-# INLINE poke2 #-}

--------------------------------------------------------------------------------
-- FFI Exports
--------------------------------------------------------------------------------

type DSPState = IORef (SF (Control, Sample) (Control, Sample))

foreign export ccall startCtx :: IO (StablePtr DSPState)
startCtx :: IO (StablePtr DSPState)
startCtx = newStablePtr =<< newIORef MM.process 

foreign export ccall freeCtx :: StablePtr DSPState -> IO ()
freeCtx :: StablePtr DSPState -> IO ()
freeCtx = freeStablePtr

foreign export ccall main2 :: StablePtr DSPState -> Int -> Int ->
                              Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float ->
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
      (stereoOut, sfNext) = processBlock oldSF newCtrl stereoIn
      leftOut  = V.map (fst . snd) stereoOut
      rightOut = V.map (snd . snd) stereoOut
  poke2 ol or $ V.zipWith (,) leftOut rightOut
  writeIORef ref sfNext

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
                        then show (getField name MM.initControl :: Bool)
                        else show (getField name MM.initControl :: Float)
          in name ++ " " ++ typeStr ++ " " ++ pStr ++ " " ++ go (i + 1)
