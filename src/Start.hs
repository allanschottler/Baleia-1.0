{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
module Start where

import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Marshal.Array (pokeArray)
import Foreign.C.String
import qualified Data.StorableVector as V
import Foreign.Storable
import Foreign.Storable.Tuple

import Data.IORef
import Data.List.Split
import Language.Haskell.TH

import Baleia
import Baleia.Meta
import Baleia.Sample


import MyModule

data Context = Context {  
    somthing :: Int 
}

initContext = Context {  
    somthing = 0 
}

pairEvery :: [a] -> [(a,a)]
pairEvery (x:y:xs) = [(x,y)] ++ pairEvery xs
pairEvery [] = []

controlInFields' = $(introspect ''ControlIn)
controlInFields = pairEvery $ split (dropBlanks . dropDelims $ oneOf "()[],\"") controlInFields' 
(values',types') = unzip controlInFields
controlInMeta = newCString controlInFields'

-- | Auxiliary function
floatToBool :: Float -> Bool
floatToBool f = f >= 0.5

floatToIntList :: Int -> Float -> Int
floatToIntList n f = Prelude.floor (f * (fromIntegral n))

rawToControlInNew :: [String] -> [String] -> [Float] -> ControlIn
rawToControlInNew types names values = r''
    where zs = zip types $ zip names values
          r = MyModule.initControlIn
          r'' = foldr f r zs
          f (t,(n,v)) s = if t == "Bool" then setField n (floatToBool v) s
                          else setField n v s

pokeArray2 :: (Storable a) => Ptr a -> Ptr a -> [(a,a)] -> IO ()
pokeArray2 ptra ptrb vals0 = go vals0 0
    where go [] _          = return ()
          go ((a,b):vals) n = do pokeElemOff ptra n a; pokeElemOff ptrb n b; go vals (n + 1)

{---------------------------------------------------------------------
| CTX
---------------------------------------------------------------------}

startCtx :: IO(StablePtr (IORef Context))
startCtx = do
    ctx <- newIORef initContext
    newStablePtr ctx

freeCtx :: StablePtr (IORef Context) -> IO()
freeCtx = freeStablePtr

{---------------------------------------------------------------------
| CUSTOM
---------------------------------------------------------------------}

famp :: (Fractional a) => Float -> a -> a
famp v s = s * va where va = realToFrac v

amp :: (Fractional a, Storable a) => Float -> V.Vector a -> V.Vector a
amp g = V.map $ famp g

pan :: Float -> V.Vector Sample -> V.Vector Sample
pan p = V.map $ Baleia.Sample.fpan p

doProcess :: ControlIn -> V.Vector Sample -> V.Vector Sample
doProcess ctrlIn i = amp (1-g) $ pan p i
    where g = vGain ctrlIn
          p = vPan ctrlIn

{---------------------------------------------------------------------
 | main2
 | Main function being called from C/C++ side.
 | Responsible for marshaling types and converting ptrs into lists.
---------------------------------------------------------------------}
main2 
    :: StablePtr (IORef Context)
    -> Int        -- | ns: number of samples in buffers
    -> Int        -- | np: number of parameters in parameters buffer
    -> Ptr Float  -- | ci: buffer of control-in parameters
    -> Ptr Float  -- | co: buffer of control-out parameters
    -> Ptr Float  -- | il: input left
    -> Ptr Float  -- | ir: input right 
    -> Ptr Float  -- | ol: output left 
    -> Ptr Float  -- | or: output right 
    -> IO() -- IO( StablePtr (IORef Context) )
main2 ptr ns np ci co il ir ol or = do
    civ <- V.peek np ci
    cov <- V.peek np co
    ilv <- V.peek ns il
    irv <- V.peek ns ir
    cip <- V.peek np ci
    let cin = rawToControlInNew types' values' $ V.unpack cip
    let out = doProcess cin $ V.zipWith Stereo ilv irv
    --unpacking is slow, maybe use handle?
    let olr = V.unpack $ V.map stereoToFloat out
    pokeArray2 ol or olr

foreign export ccall controlInMeta :: IO(CString)
foreign export ccall startCtx :: IO(StablePtr (IORef Context))
foreign export ccall freeCtx :: StablePtr (IORef Context) -> IO()
foreign export ccall main2 
    :: StablePtr (IORef Context)
    -> Int       -- | nSamples
    -> Int       -- | nParameters
    -> Ptr Float -- | control in
    -> Ptr Float -- | control out
    -> Ptr Float -- | input left
    -> Ptr Float -- | input right
    -> Ptr Float -- | output left
    -> Ptr Float -- | output right
    -> IO() -- IO( StablePtr (IORef Context) ) 