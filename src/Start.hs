{-# LANGUAGE TemplateHaskell #-}
module Start where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.StablePtr
import Foreign.C.String
import Foreign.Storable
import Foreign.Storable.Tuple

import Data.IORef
import Data.List.Split
-- import qualified Data.StorableVector as V
import qualified Data.StorableVector as V

import Language.Haskell.TH
import qualified Language.Haskell.Interpreter as Hint

import System.Directory
import System.IO.Unsafe

import Baleia
import Baleia.Meta
import Baleia.Sample
import Baleia.DSP
import Baleia.Time
import Control

{---------------------------------------------------------------------
| META
---------------------------------------------------------------------}

pairEvery' :: [String] -> V.Vector (CString, CString)
pairEvery' (x:y:xs) = unsafePerformIO $ do
    cx <- newCString x
    cy <- newCString y
    return $ V.cons (cx,cy) $ pairEvery' xs
pairEvery' [] = V.empty

ctrlFields' = $(introspect ''Control)
ctrlFields = pairEvery' $ split (dropBlanks . dropDelims $ oneOf "()[],\"") ctrlFields' 

foreign export ccall controlInMeta :: IO(CString)
controlInMeta = newCString ctrlFields'

raw2ctrl' :: V.Vector (CString, CString) -> V.Vector Float -> Control
raw2ctrl' fields values = r
    where zs = V.zipWith (,) fields values
          r = V.foldr f initControl zs
          cs2s = unsafePerformIO . peekCString
          f ((n,t),v) s | (cs2s t) == "Bool" = setField (cs2s n) (v >= 0.5) s
                        | otherwise = setField (cs2s n) v s

poke2 :: (Storable a) => Ptr a -> Ptr a -> V.Vector (a, a) -> IO ()
poke2 ptra ptrb v = go v 0
    where go v' n | V.null v' = return ()
                  | otherwise = do 
                        let (a, b) = V.head v' 
                        pokeElemOff ptra n a
                        pokeElemOff ptrb n b
                        go (V.tail v') (n + 1)

{---------------------------------------------------------------------
| CUSTOM
---------------------------------------------------------------------}

-- process :: IO(Control -> V.Vector Sample -> IO(V.Vector Sample))
process = do 
    fd <- fdbkdelay $ 1 seconds
    pure $ \ctrl v -> mix 0.5 v =<< fd (vGain ctrl) v

-- process = do
--     d <- delay2 22050
--     pure $ \ctrl v -> amp2 2 =<< mix2 (1-(vGain ctrl)) v =<< d v

{---------------------------------------------------------------------
| CTX
---------------------------------------------------------------------}

type Context = (Control -> V.Vector Sample -> IO (V.Vector Sample))--(Control -> V.Vector Sample -> IO(V.Vector Sample))

foreign export ccall startCtx :: IO(StablePtr Context)
startCtx = process >>= newStablePtr 

foreign export ccall freeCtx :: StablePtr Context -> IO()
freeCtx = freeStablePtr

{---------------------------------------------------------------------
 | main2
 | Main function being called from C/C++ side.
 | Responsible for marshaling types and converting ptrs into vectors.
---------------------------------------------------------------------}

foreign export ccall main2 :: StablePtr Context -> Int -> Int -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> IO()
main2 pf ns np ci co il ir ol or = do
    f <- deRefStablePtr pf
    ilv <- V.peek ns il
    irv <- V.peek ns ir
    cip <- V.peek np ci
    let ctrl = raw2ctrl' ctrlFields cip
    let i = V.zipWith Stereo ilv irv
    out <- f ctrl i
    poke2 ol or $ V.map stereoToFloat out
