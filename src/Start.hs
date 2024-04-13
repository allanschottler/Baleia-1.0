{-# LANGUAGE TemplateHaskell #-}
module Start where

import Foreign.Ptr
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

import Baleia
import Baleia.Meta
import Baleia.Sample
import Baleia.DSP
import Control

pairEvery :: [a] -> [(a,a)]
pairEvery (x:y:xs) = [(x,y)] ++ pairEvery xs
pairEvery [] = []

controlInFields' = $(introspect ''Control)
controlInFields = pairEvery $ split (dropBlanks . dropDelims $ oneOf "()[],\"") controlInFields' 
(values',types') = unzip controlInFields
controlInMeta = newCString controlInFields'

-- | Auxiliary function
floatToBool :: Float -> Bool
floatToBool f = f >= 0.5

floatToIntList :: Int -> Float -> Int
floatToIntList n f = Prelude.floor (f * (fromIntegral n))

rawToControlIn :: [String] -> [String] -> [Float] -> Control
rawToControlIn types names values = r''
    where zs = zip types $ zip names values
          r = initControl
          r'' = foldr f r zs
          f (t,(n,v)) s = if t == "Bool" then setField n (floatToBool v) s
                          else setField n v s

pokeArray2 :: (Storable a) => Ptr a -> Ptr a -> [(a,a)] -> IO ()
pokeArray2 ptra ptrb vals0 = go vals0 0
    where go [] _          = return ()
          go ((a,b):vals) n = do pokeElemOff ptra n a; pokeElemOff ptrb n b; go vals (n + 1)

{---------------------------------------------------------------------
| CUSTOM
---------------------------------------------------------------------}

process :: Control -> V.Vector Sample -> V.Vector Sample
process ctrl i = clip 0.5 $ amp (1+g*10) $ pan p i
    where g = vGain ctrl
          p = vPan ctrl

{---------------------------------------------------------------------
| CTX
---------------------------------------------------------------------}

data Context = Context {
    doProcess :: IORef(Control -> V.Vector Sample -> V.Vector Sample) 
}

initProcess :: Control -> V.Vector Sample -> V.Vector Sample
initProcess ctrl = id

startCtx :: IO(StablePtr (IORef Context))
startCtx = do
    f <- newIORef process
    ctx <- newIORef $ Context f
    newStablePtr ctx

-- interpretPlugin :: Hint.Interpreter (Control -> V.Vector Sample -> V.Vector Sample)
-- interpretPlugin = do
--     Hint.set [Hint.languageExtensions Hint.:= [Hint.TemplateHaskell]]
--     Hint.loadModules ["MyModule.hs"]
--     Hint.setImportsQ [("Prelude", Nothing), ("MyModule", Nothing), ("Baleia", Nothing), ("Control", Nothing), ("Data.StorableVector", Just "V")]
--     Hint.interpret "process" (Hint.as :: Control -> V.Vector Sample -> V.Vector Sample)

-- startCtx' :: IO(StablePtr (IORef Context))
-- startCtx' = do
--     setCurrentDirectory "D:\\VST_SDK\\my_plugins\\baleia\\Baleia-1.0\\plugins"
--     res <- Hint.runInterpreter $ Hint.interpret "4" (Hint.as :: Int)
--     -- f <- newIORef initProcess
--     -- ctx <- newIORef $ Context f
--     -- newStablePtr ctx
--     case res of
--         Left err -> error $ show err
--         Right a -> do
--             f <- newIORef initProcess
--             ctxRef <- newIORef $ Context f
--             newStablePtr ctxRef

freeCtx :: StablePtr (IORef Context) -> IO()
freeCtx = freeStablePtr

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
    ctx' <- deRefStablePtr ptr
    ctx <- readIORef ctx'
    processFunc <- readIORef $ doProcess ctx
    civ <- V.peek np ci
    cov <- V.peek np co
    ilv <- V.peek ns il
    irv <- V.peek ns ir
    cip <- V.peek np ci
    --dont unpack
    let cin = rawToControlIn types' values' $ V.unpack cip
    let out = processFunc cin $ V.zipWith Stereo ilv irv
    --unpacking is slow, maybe use handle or implement V.poke2?
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