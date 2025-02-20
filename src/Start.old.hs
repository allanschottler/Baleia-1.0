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
import Data.Maybe
import Text.Printf (printf)
import Control.Exception
-- import qualified Data.StorableVector as V
import qualified Data.Vector.Storable as V

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

ctrlFields' = $(introspect ''Control)

pairEvery' :: [String] -> [(String, String)]
pairEvery' (x:y:xs) = (x, y) : pairEvery' xs
pairEvery' []         = []

ctrlFields = pairEvery' $ split (dropBlanks . dropDelims $ oneOf "()[],\"") ctrlFields' 

foreign export ccall controlInMeta :: IO CString
controlInMeta = newCString $ go ctrlFields
    where go v | v == [] = ""
               | otherwise = n ++ " " ++ t ++ " " ++ p ++ " " ++ (go $ tail v)
                   where (n,t) = head v
                         p | t == "Bool" = show (getField n initControl :: Bool)
                           | otherwise = show (getField n initControl :: Float)

raw2ctrl' :: [Float] -> Control
raw2ctrl' values = foldr f initControl zs
    where zs = zipWith (\(a,b) v -> (a,b,v)) ctrlFields values
          f (n,t,v) s | t == "Bool" = setField n (v >= 0.5) s
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
    -- d <- delayFeedback 22050
    pure $ \c v -> do
        f' <- f (lgr 30 18500 $ _Freq c) (lin 0.707 20 $ _Q c) v
        let dd = delayFeedback 22050 (_Feedback c) f'
        -- dd <- d (22050 + 22050 * (_Time c)) f'
        -- dd <- d (22050 + 22050 * (_Time c)) (_Feedback c) f'
        amp (_Gain c) =<< clip (_Clip c) =<< mix (1-(_DryWet c)) dd f'

-- process = pure $ \_ -> return

{---------------------------------------------------------------------
| CTX
---------------------------------------------------------------------}

type Context = (Control -> V.Vector Sample -> IO (V.Vector Sample))

foreign export ccall startCtx :: IO(StablePtr Context)
startCtx = process >>= newStablePtr 

foreign export ccall freeCtx :: StablePtr Context -> IO()
freeCtx = freeStablePtr

foreign export ccall main2 :: StablePtr Context -> Int -> Int -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> IO()
main2 pf ns np ci co il ir ol or = do
    f <- deRefStablePtr pf
    ilv <- V.peek ns il
    irv <- V.peek ns ir
    cip <- peekArray np ci
    let ctrl = raw2ctrl' cip
    out <- f ctrl $ V.zipWith Stereo ilv irv
    poke2 ol or $ V.map stereoToFloat out
