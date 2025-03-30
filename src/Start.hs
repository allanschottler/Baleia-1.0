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

raw2ctrl :: V.Vector Float -> Control
raw2ctrl values = V.ifoldl' (\s i v ->
    let nameCString = fst $ ctrlFields V.! i
        name = unsafePerformIO (peekCString nameCString)
    in setField name v s
  ) MM.initControl values
{-# INLINE raw2ctrl #-}

ctrl2raw :: Control -> V.Vector Float
ctrl2raw ctrl = V.generate (V.length ctrlFields) $ \i ->
  let nameCString = fst $ ctrlFields V.! i
      name = unsafePerformIO (peekCString nameCString)
  in getField name ctrl
{-# INLINE ctrl2raw #-}

--------------------------------------------------------------------------------
-- Block Processing
--------------------------------------------------------------------------------

processBlock sf ctrl vecIn ol or = go 0 sf (V.length vecIn) ctrl
  where
    go i currentSF n c
      | i >= n    = return (c, currentSF)
      | otherwise = do
          let ((c', (l,r)), nextSF) = runSF currentSF (c, (vecIn V.! i))
          pokeElemOff ol i l
          pokeElemOff or i r
          go (i + 1) nextSF n c'
{-# INLINE processBlock #-}

--------------------------------------------------------------------------------
-- FFI Exports
--------------------------------------------------------------------------------

type Context = IORef (SF (Control, Sample) (Control, Sample))

foreign export ccall startCtx :: IO (StablePtr Context)
startCtx :: IO (StablePtr Context)
startCtx = newStablePtr =<< newIORef MM.process 

foreign export ccall freeCtx :: StablePtr Context -> IO ()
freeCtx :: StablePtr Context -> IO ()
freeCtx = freeStablePtr

foreign export ccall main2 :: StablePtr Context -> Int -> Int ->
                              Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float ->
                              Ptr Float -> Ptr Float -> IO ()
main2 ctx ns np ci co il ir ol or = do
  ref <- deRefStablePtr ctx
  oldSF <- readIORef ref
  cptr <- newForeignPtr_ ci
  lptr <- newForeignPtr_ il
  rptr <- newForeignPtr_ ir
  let newCtrl = raw2ctrl $ V.unsafeFromForeignPtr0 cptr np
      leftIn   = V.unsafeFromForeignPtr0 lptr ns
      rightIn  = V.unsafeFromForeignPtr0 rptr ns
      stereoIn = V.zipWith (,) leftIn rightIn
  (ctrlOut, sfNext) <- processBlock oldSF newCtrl stereoIn ol or
  V.unsafeWith (ctrl2raw ctrlOut) $ \ptr' -> copyArray co ptr' np
  writeIORef ref sfNext

foreign export ccall controlInMeta :: IO CString
controlInMeta :: IO CString
controlInMeta = newCString (go 0)
  where
    len = V.length ctrlFields
    go i
      | i >= len  = ""
      | otherwise =
          let nameCString = fst $ ctrlFields V.! i
              name = unsafePerformIO (peekCString nameCString)
              pStr = show (getField name MM.initControl :: Float)
          in name ++ " " ++ "NA" ++ " " ++ pStr ++ " " ++ go (i + 1)
