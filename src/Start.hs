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

delay2 :: Int -> IO (V.Vector Sample -> IO(V.Vector Sample))
delay2 n = do
    r <- newIORef (V.replicate n 0)
    pure $ \v -> atomicModifyIORef r $ \v_delayed ->
        let (v1, v2) = V.splitAt (V.length v - n) v
            (v3, v4) = V.splitAt (V.length v) v_delayed
        in if V.length v >= n then (v2,  V.append v_delayed v1) else (V.append v4 v, v3)

amp2 :: Float -> V.Vector Sample -> IO(V.Vector Sample)
amp2 g v = return $ amp g v

mix2 :: Float -> V.Vector Sample -> V.Vector Sample -> IO(V.Vector Sample)
mix2 m a b = return $ mix m a b

fdbk' x f1 f2 buf = (bak, out)
    where out = V.head $ unsafePerformIO $ f1 $ V.singleton $ x + buf
          bak = V.head $ unsafePerformIO $ f2 $ V.singleton out

fdbk :: IO((V.Vector Sample -> IO(V.Vector Sample)) -> (V.Vector Sample -> IO(V.Vector Sample)) -> V.Vector Sample -> IO(V.Vector Sample))
fdbk = do
    r <- newIORef 0
    pure $ \f1 f2 v -> do
        let g x = unsafePerformIO $ atomicModifyIORef r $ fdbk' x f1 f2
        -- let g = id
        return $ V.map g v    

fdbkdelay :: Int -> IO(Float -> V.Vector Sample -> IO(V.Vector Sample))
fdbkdelay n = do
    d <- delay2 n
    f <- fdbk
    pure $ \fb v -> f d (amp2 fb) v

process :: IO(Control -> V.Vector Sample -> IO(V.Vector Sample))
process = do 
    fd <- fdbkdelay 22050
    pure $ \ctrl v -> fd (vGain ctrl) v
    --pure $ \ctrl v -> amp2 2 =<< mix2 (1-(vGain ctrl)) v =<< d v

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
    -- out <- f i
    poke2 ol or $ V.map stereoToFloat out
