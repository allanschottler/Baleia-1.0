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
import Baleia.Sample    -- For stereo/sample helpers
import Baleia.Meta      -- Provides Control, initControl, etc.
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

import Control

--------------------------------------------------------------------------------
-- Debug Logging (to file), creating the directory first
--------------------------------------------------------------------------------

logPath :: FilePath
logPath = "C:\\temp\\mydll.log"

debugLog :: String -> IO ()
debugLog s = do
  createDirectoryIfMissing True "C:\\temp"
  appendFile logPath (s ++ "\n")

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
      yst <- newCString y
      rest <- go xs
      return $ (xst, yst) : rest
    go [] = return []

{-# NOINLINE ctrlFields #-}
ctrlFields :: V.Vector (CString, CString)
ctrlFields = pairEvery $ split (dropBlanks . dropDelims $ oneOf "()[],\"") ctrlFields'

foreign import ccall unsafe "string.h strcmp" c_strcmp :: CString -> CString -> CInt

{-# NOINLINE boolStr #-}
boolStr :: CString
boolStr = unsafePerformIO $ newCString "Bool"

{-# NOINLINE ctrlFieldsInfo #-}
ctrlFieldsInfo :: V.Vector (CString, Bool)
ctrlFieldsInfo = V.map (\(name, typeStr) -> (name, c_strcmp typeStr boolStr == 0)) ctrlFields

raw2ctrl :: V.Vector Float -> Control
raw2ctrl values = V.ifoldl' (\s i v ->
    let (nameCString, isBool) = ctrlFieldsInfo V.! i
        name = unsafePerformIO (peekCString nameCString)
    in if isBool then setField name (v >= 0.5) s else setField name v s
  ) initControl values

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
-- Dynamic Loading of the Process Function
--------------------------------------------------------------------------------


-- | Load the MyModule plugin and retrieve the 'process' symbol.
-- loadMyModuleProcess :: IO ProcessFunction
-- loadMyModuleProcess = do
--   mod <- loadModule "MyModule" (Just "D:\\Baleia-1.0\\dist-newstyle\\build\\x86_64-windows\\ghc-9.12.1\\Baleia-1.0\\f\\Baleia\\build\\Baleia\\Baleia-tmp") Nothing 
--   processFun <- loadFunction mod "process" 
--   return processFun

-- extract :: SF (Control, (Float, Sample)) Float
-- extract = arr (\(c, (v, _)) -> v)


-- minSF :: SF (Control, Sample) Float
-- minSF = cv ((\s -> 60 + 480*s)._Freq) >>> extract

-- maxSF :: SF (Control, Sample) Float
-- maxSF = cv ((\s -> 240 + 960*s)._Gain) >>> extract

-- modSF :: SF (Control, Sample) Float
-- modSF = cv ((\s -> 2 + 58*s)._Time) >>> lfoSqr >>> extract

-- m :: SF (Control, Sample) (Control, (Float, Sample))
-- m = proc (c, s) -> do
--     minVal <- minSF -< (c, s)
--     maxVal <- maxSF -< (c, s)
--     f      <- modSF -< (c, s)
--     o      <- second oscSin -< (f, s)
--     i    <- lin'' -< (minVal, maxVal, f, s)
--     out <- amp -< (i, o)
--     returnA -< (c, (out, s))

-- lerp a b t = a + (b - a) * t

process :: SF (Control, Sample) (Control, Sample)
process = proc (c, _) -> do
    minVal   <- lgr       -< (60,  8000, _Freq1 c)
    maxVal   <- lgr       -< (60,  8000, _Freq2 c)
    lfoSpeed <- lgr       -< (0.1, 20,   _Time c)
    l        <- lfoSin    -< lfoSpeed
    f        <- lgr       -< (minVal, maxVal, l)
    o        <- oscSin    -< f
    -- n        <- oscWNoise -< ()
    a        <- amp       -< (_Gain c, o)
    d        <- fdbkDelay (3 beats `div` 4) -< (_Fdbk1 c, a)
    x        <- lin       -< (a, d, _DryWet c)
    m        <- mono      -< x
    returnA  -< (c, m)
{-# INLINE process #-}


--------------------------------------------------------------------------------
-- FFI Exports
--------------------------------------------------------------------------------

type DSPState = IORef (SF (Control, Sample) (Control, Sample))

foreign export ccall startCtx :: IO (StablePtr DSPState)
startCtx :: IO (StablePtr DSPState)
startCtx = newStablePtr =<< newIORef process 

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
                        then show (getField name initControl :: Bool)
                        else show (getField name initControl :: Float)
          in name ++ " " ++ typeStr ++ " " ++ pStr ++ " " ++ go (i + 1)
