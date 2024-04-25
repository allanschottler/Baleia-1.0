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
import qualified Data.StorableVector as V

import Language.Haskell.TH
import qualified Language.Haskell.Interpreter as Hint

import System.Directory
import System.IO.Unsafe

import Baleia
import Baleia.Meta
import Baleia.Sample
import Baleia.DSP
import Control

{---------------------------------------------------------------------
| META
---------------------------------------------------------------------}

pairEvery :: [a] -> [(a,a)]
pairEvery (x:y:xs) = [(x,y)] ++ pairEvery xs
pairEvery [] = []

controlInFields' = $(introspect ''Control)
controlInFields = pairEvery $ split (dropBlanks . dropDelims $ oneOf "()[],\"") controlInFields' 
controlInMeta = newCString controlInFields'

-- | Auxiliary function
floatToBool :: Float -> Bool
floatToBool f = f >= 0.5

floatToIntList :: Int -> Float -> Int
floatToIntList n f = Prelude.floor (f * (fromIntegral n))

raw2ctrl :: [(String, String)] -> [Float] -> Control
raw2ctrl fields values = r
    where zs = zip fields values
          r = foldr f initControl zs
          f ((n,t),v) s | t == "Bool" = setField n (floatToBool v) s
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

process :: Control -> V.Vector Sample -> V.Vector Sample
process ctrl i = clip 0.5 i

{---------------------------------------------------------------------
| CTX
---------------------------------------------------------------------}

type Context = IORef(Control -> V.Vector Sample -> V.Vector Sample)

startCtx :: IO(StablePtr Context)
startCtx = do newStablePtr =<< newIORef process

freeCtx :: StablePtr Context -> IO()
freeCtx = freeStablePtr

{---------------------------------------------------------------------
 | main2
 | Main function being called from C/C++ side.
 | Responsible for marshaling types and converting ptrs into vectors.
---------------------------------------------------------------------}

main2 :: StablePtr Context -> Int -> Int -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> IO()
main2 pf ns np ci co il ir ol or = do
    f <- readIORef =<< deRefStablePtr pf
    ilv <- V.peek ns il
    irv <- V.peek ns ir
    cip <- peekArray np ci
    let ctrl = raw2ctrl controlInFields cip
    let out = f ctrl $ V.zipWith Stereo ilv irv
    poke2 ol or $ V.map stereoToFloat out

foreign export ccall controlInMeta :: IO(CString)
foreign export ccall startCtx :: IO(StablePtr Context)
foreign export ccall freeCtx :: StablePtr Context -> IO()
foreign export ccall main2 :: StablePtr Context -> Int -> Int -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> IO()