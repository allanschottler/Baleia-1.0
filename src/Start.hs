{-# LANGUAGE TemplateHaskell #-}
module Start where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.StablePtr
import Foreign.C.String
import Foreign.Storable
import Foreign.Storable.Tuple

import System.Directory
import System.IO.Unsafe
import Data.IORef
import Data.List.Split
import qualified Data.StorableVector as V

import Language.Haskell.TH

import Baleia
import Baleia.Meta
import Baleia.Sample
import Baleia.DSP
import Baleia.Time
import Control

{---------------------------------------------------------------------
| META
---------------------------------------------------------------------}

pairEvery' :: [String] -> [(String, String)]
pairEvery' (x:y:xs) = (x,y) : pairEvery' xs
pairEvery' [] = []

ctrlFields' = $(introspect ''Control)
ctrlFields = pairEvery' $ split (dropBlanks . dropDelims $ oneOf "()[],\"") ctrlFields' 

foreign export ccall controlInMeta :: IO(CString)
controlInMeta = newCString ctrlFields'

raw2ctrl' :: [(String, String)] -> [Float] -> Control
raw2ctrl' fields values = foldr f initControl zs
    where zs = zipWith (,) fields values
          f ((n,t),v) s | t == "Bool" = setField n (v >= 0.5) s
                        | otherwise = setField n v s

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
-- process = do 
--     fd <- fdbkdelay $ 1 seconds
--     pure $ \ctrl v -> mix 0.5 v =<< fd (vGain ctrl) v

process = do 
    f <- lp2FilterV2
    d <- fdbkdelay (3 beats `div` 4)
    pure $ \c v -> do
        f' <- f (lgr 30 18500 $ kP1 c) (lin 0.707 20 $ kP2 c) v
        dd <- d (kP3 c) f'
        amp (kGain c) =<< clip (kP5 c) =<< mix (kP4 c) dd f'

-- process = do
--     d <- delay2 22050
--     pure $ \ctrl v -> amp2 2 =<< mix2 (1-(vGain ctrl)) v =<< d v

{---------------------------------------------------------------------
| CTX
---------------------------------------------------------------------}

type Context = (Control -> V.Vector Sample -> IO (V.Vector Sample))

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
    cip <- peekArray np ci
    let ctrl = raw2ctrl' ctrlFields cip
    let i = V.zipWith Stereo ilv irv
    out <- f ctrl i
    poke2 ol or $ V.map stereoToFloat out
