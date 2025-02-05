module Main where

import qualified Data.StorableVector as V
import Baleia
import Baleia.Meta
import Baleia.Sample
import Baleia.DSP
import Baleia.Time
import Control

ctrl = initControl
ir = V.pack [1,0,0,0,0,0]

run (f, expect) input = do
    t <- f
    out <- t ctrl input
    return $ if out == expect then Nothing else Just out

-- test1 :: (IO (Control -> V.Vector Sample -> IO (V.Vector Sample)), V.Vector Sample)
test1 = (pure $ \_ -> amp 2, V.pack [2,0,0,0,0,0])

test2' = do
    d <- delay 1
    pure $ \_ -> d
test2 = (test2', V.pack [0,1,0,0,0,0])

test3' = do
    vec <- amp 2 ir
    pure $ \_ -> mix 0.5 vec
test3 = (test3', V.pack [1.5,0,0,0,0,0])

test4' = do
    f <- ffFilter
    pure $ \_ _ -> f 0.5 0.5 $ V.pack [1,-1,1,-1,1,-1]
test4 = (test4', V.pack [0.5,0,0,0,0,0])

test5' = do
    f <- ffFilter
    pure $ \_ _ -> f 0.5 0.5 $ V.pack [-1,1,-1,1,-1,1]
test5 = (test5', V.pack [-0.5,0,0,0,0,0])

test6' = do
    f <- ffFilter
    pure $ \_ _ -> f 0.5 0.5 $ V.pack [0,1,0,0,0,0]
test6 = (test6', V.pack [0,0.5,0.5,0,0,0])

test7' = do
    f <- fdbk
    pure $ \_ _ -> f pure pure ir
test7 = (test7', V.pack [1,1,1,1,1,1])

test8' = do
    f <- fdbk
    pure $ \_ _ -> f pure (amp (-1)) ir
test8 = (test8', V.pack [1,-1,1,-1,1,-1])

test9' = do
    f <- fdbk
    pure $ \_ _ -> f (amp (-0.5)) (amp (0.5)) ir
test9 = (test9', V.pack [1,0,0,0,0,0])



tests = [test1,test2,test3,test4,test5,test6,test7,test8,test9]

main :: IO ()
main = do
    results <- mapM (\test -> run test ir) tests
    mapM_ printResult (zip [1..] results)
  where
    printResult (n, Nothing)  = putStrLn $ "Test " ++ show n ++ ": PASSED"
    printResult (n, Just a) = putStrLn $ "Test " ++ show n ++ ": FAILED => " ++ show a
