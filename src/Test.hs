module Main where

-- import qualified Data.StorableVector as V
-- import Baleia
-- import Baleia.Meta
-- import Baleia.Sample
-- import Baleia.DSP
-- import Baleia.Time
-- import Control

-- ctrl = initControl
-- ir = V.pack [1,0,0,0,0,0]

-- run (f, expect) input = do
--     t <- f
--     out <- t ctrl input
--     return $ if out == expect then Nothing else Just out

-- -- test1 :: (IO (Control -> V.Vector Sample -> IO (V.Vector Sample)), V.Vector Sample)
-- test1 = (pure $ \_ -> amp 2, V.pack [2,0,0,0,0,0])

-- test2' = do
--     d <- delay 1
--     pure $ \_ -> d
-- test2 = (test2', V.pack [0,1,0,0,0,0])

-- test3' = do
--     vec <- amp 2 ir
--     pure $ \_ -> mix 0.5 vec
-- test3 = (test3', V.pack [1.5,0,0,0,0,0])

-- test4' = do
--     f <- ffFilter
--     pure $ \_ _ -> f 0.5 0.5 $ V.pack [1,-1,1,-1,1,-1]
-- test4 = (test4', V.pack [0.5,0,0,0,0,0])

-- test5' = do
--     f <- ffFilter
--     pure $ \_ _ -> f 0.5 0.5 $ V.pack [-1,1,-1,1,-1,1]
-- test5 = (test5', V.pack [-0.5,0,0,0,0,0])

-- test6' = do
--     f <- ffFilter
--     pure $ \_ _ -> f 0.5 0.5 $ V.pack [0,1,0,0,0,0]
-- test6 = (test6', V.pack [0,0.5,0.5,0,0,0])

-- test7' = do
--     f <- fdbk
--     pure $ \_ _ -> f pure pure ir
-- test7 = (test7', V.pack [1,1,1,1,1,1])

-- test8' = do
--     f <- fdbk
--     pure $ \_ _ -> f pure (amp (-1)) ir
-- test8 = (test8', V.pack [1,-1,1,-1,1,-1])

-- test9' = do
--     f <- fdbk
--     pure $ \_ _ -> f (amp (-0.5)) (amp (0.5)) ir
-- test9 = (test9', V.pack [1,0,0,0,0,0])



-- tests = [test1,test2,test3,test4,test5,test6,test7,test8,test9]

-- main :: IO ()
-- main = do
--     results <- mapM (\test -> run test ir) tests
--     mapM_ printResult (zip [1..] results)
--   where
--     printResult (n, Nothing)  = putStrLn $ "Test " ++ show n ++ ": PASSED"
--     printResult (n, Just a) = putStrLn $ "Test " ++ show n ++ ": FAILED => " ++ show a

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
import Control
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe
import Control.Monad.IO.Class (liftIO)
import System.IO (appendFile)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Plugins.DynamicLoader

-- logPath :: FilePath
-- logPath = "C:\\temp\\mydll.log"

-- debugLog :: String -> IO ()
-- debugLog s = do
--   createDirectoryIfMissing True "C:\\temp"
--   appendFile logPath (s ++ "\n")

-- loadProcess :: IO (SF (Control, Sample) (Control, Sample))
-- loadProcess = do
--   debugLog "Starting dynamic load of MyModule..."
--   eResult <- try $ runInterpreter $ do
--       set [ searchPath := ["D:\\Baleia-1.0\\src", "D:\\Baleia-1.0\\src\\Baleia"] ]
--       liftIO (debugLog "Search path set to D:\\Baleia-1.0\\src, D:\\Baleia-1.0\\src\\Baleia.")
--       unsafeSetGhcOption  "-package template-haskell"
--       unsafeSetGhcOption  "-package syb"
--       unsafeSetGhcOption  "-package storable-tuple"
--       loadModules ["D:\\Baleia-1.0\\src\\MyModule.hs"]
--       liftIO (debugLog "Module MyModule.hs loaded.")
--       setTopLevelModules ["MyModule"]
--       liftIO (debugLog "Top-level module set to MyModule.")
--       interpret "process" (as :: SF (Control, Sample) (Control, Sample))
--     :: IO (Either SomeException (Either InterpreterError (SF (Control, Sample) (Control, Sample))))
--   case eResult of
--       Left ex -> do
--           debugLog ("Interpreter exception: " ++ show ex)
--           error $ "Dynamic load failed: " ++ show ex
--       Right (Left err) -> do
--           debugLog ("Interpreter error: " ++ show err)
--           error $ "Dynamic load failed: " ++ show err
--       Right (Right proc) -> do
--           debugLog "Process function loaded successfully."
--           return proc

loadMyModuleProcess :: IO (SF (Control, Sample) (Control, Sample))
loadMyModuleProcess = do
  mod <- loadModuleFromPath "D:\\Baleia-1.0\\dist-newstyle\\build\\x86_64-windows\\ghc-9.12.1\\Baleia-1.0\\x\\BaleiaTest\\build\\BaleiaTest\\BaleiaTest-tmp\\MyModule.o" (Just "D:\\Baleia-1.0\\dist-newstyle\\build\\x86_64-windows\\ghc-9.12.1\\Baleia-1.0\\x\\BaleiaTest\\build\\BaleiaTest\\BaleiaTest-tmp\\") 
  resolveFunctions
  processFun <- loadFunction mod "process" 
  return processFun

main :: IO ()
main = do
  sf <- loadMyModuleProcess
  putStrLn "Successfully loaded MyModule process in an EXE"