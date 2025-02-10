module Baleia.Interpreter where

import Control
import Baleia.Sample
import qualified Data.StorableVector as V
import Language.Haskell.Interpreter as Hint
import System.Directory

interpretPlugin :: Hint.Interpreter (IO(Control -> V.Vector Sample -> IO(V.Vector Sample)))
interpretPlugin = do
    Hint.loadModules ["MyModule.hs"]
    Hint.setTopLevelModules ["MyModule"]
    Hint.setImportsQ [("Prelude", Nothing), ("MyModule", Nothing), ("Baleia", Nothing), ("Baleia.Sample", Nothing), ("Baleia.DSP", Nothing), ("Control", Nothing), ("Data.StorableVector", Just "V")]
    Hint.interpret "process" (Hint.as :: IO(Control -> V.Vector Sample -> IO(V.Vector Sample)))

interpretF :: Hint.Interpreter (Float)
interpretF = do
    Hint.loadModules ["MyFunc.hs"]
    Hint.setTopLevelModules ["MyFunc"]
    Hint.setImportsQ [("Prelude", Nothing), ("MyFunc", Nothing)]
    Hint.interpret "just2" (Hint.as :: Float)

interpProcess ::IO (Control -> V.Vector Sample -> IO(V.Vector Sample))
interpProcess = do
    setCurrentDirectory "D:\\Baleia-1.0\\src"
    res <- Hint.runInterpreter interpretPlugin
    case res of
        Left err -> error $ show err
        Right f -> f

interp :: IO(Float)
interp = do
    setCurrentDirectory "D:\\Baleia-1.0\\src"
    res <- Hint.runInterpreter interpretF
    case res of
        Left err -> error $ show err
        Right f -> pure f