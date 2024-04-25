
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