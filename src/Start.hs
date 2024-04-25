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

-- raw2ctrl :: V.Vector String -> V.Vector String -> V.Vector Float -> Control
-- raw2ctrl types names values = r''
--     where zs = V.zipWith (,) types $ V.zipWith (,) names values
--           r = initControl
--           r'' = V.foldr f r zs
--           f (t,(n,v)) s = if t == "Bool" then setField n (floatToBool v) s
--                           else setField n v s

-- pokeArray2 :: (Storable a) => Ptr a -> Ptr a -> [(a,a)] -> IO ()
-- pokeArray2 ptra ptrb vals0 = go vals0 0
--     where go [] _           = return ()
--           go ((a,b):vals) n = do pokeElemOff ptra n a; pokeElemOff ptrb n b; go vals (n + 1)

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
process ctrl i = clip 0.5 $ delay 1 $ pan (vPan ctrl) $ delay 1 $ amp (vGain ctrl) $ delay 1 i

{---------------------------------------------------------------------
| CTX
---------------------------------------------------------------------}

type Process = IORef(Control -> V.Vector Sample -> V.Vector Sample)

-- interpretPlugin :: Hint.Interpreter (Control -> V.Vector Sample -> V.Vector Sample)
-- interpretPlugin = do
--     Hint.loadModules ["MyModule.hs"]
--     Hint.setTopLevelModules ["MyModule"]
--     Hint.setImportsQ [("Prelude", Nothing), ("MyModule", Nothing), ("Baleia", Nothing), ("Baleia.Sample", Nothing), ("Baleia.DSP", Nothing), ("Control", Nothing), ("Data.StorableVector", Just "V")]
--     Hint.interpret "process" (Hint.as :: Control -> V.Vector Sample -> V.Vector Sample)

-- interpProcess :: (Control -> V.Vector Sample -> V.Vector Sample)
-- interpProcess = unsafePerformIO $ do
--     setCurrentDirectory "D:\\VST_SDK\\my_plugins\\baleia\\Baleia-1.0\\src"
--     res <- Hint.runInterpreter interpretPlugin
--     case res of
--         Left err -> error $ show err
--         Right f -> return f

startCtx :: IO(StablePtr Process)
startCtx = do newStablePtr =<< newIORef process

freeCtx :: StablePtr Process -> IO()
freeCtx = freeStablePtr

{---------------------------------------------------------------------
 | main2
 | Main function being called from C/C++ side.
 | Responsible for marshaling types and converting ptrs into vectors.
---------------------------------------------------------------------}

main2 :: StablePtr Process -> Int -> Int -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> IO()
main2 pf ns np ci co il ir ol or = do
    f <- readIORef =<< deRefStablePtr pf
    civ <- V.peek np ci
    cov <- V.peek np co
    ilv <- V.peek ns il
    irv <- V.peek ns ir
    cip <- peekArray np ci
    let ctrl = raw2ctrl controlInFields cip
    let out = f ctrl $ V.zipWith Stereo ilv irv
    poke2 ol or $ V.map stereoToFloat out

foreign export ccall controlInMeta :: IO(CString)
foreign export ccall startCtx :: IO(StablePtr Process)
foreign export ccall freeCtx :: StablePtr Process -> IO()
foreign export ccall main2 :: StablePtr Process -> Int -> Int -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> IO()