module Baleia.DSP where

import Foreign.Storable
import qualified Data.StorableVector as V
import qualified Data.Vector.Storable.Mutable as MV
import Data.IORef
import System.IO.Unsafe

import Baleia.Sample
import Baleia.Math
import Control

-- | 0.5 meaning half mix of each V.Vector
fmix :: Float -> Sample -> Sample -> Sample
fmix m a b = lerp a b $ realToFrac m

mix' :: Float -> V.Vector Sample -> V.Vector Sample -> V.Vector Sample
mix' m = V.zipWith $ fmix m

mix :: Float -> V.Vector Sample -> V.Vector Sample -> IO(V.Vector Sample)
mix m a b = return $ mix' m a b

famp :: Float -> Sample -> Sample
famp v s = s * (realToFrac v) 

amp' :: Float -> V.Vector Sample -> V.Vector Sample
amp' g = V.map $ famp g

amp :: Float -> V.Vector Sample -> IO(V.Vector Sample)
amp g v = return $ amp' g v

pan :: Float -> V.Vector Sample -> V.Vector Sample
pan p = V.map $ Baleia.Sample.fpan p

clipi :: Float -> Float -> Float
clipi thrsh v | abs v > thrsh = thrsh * signum v 
              | otherwise     = v

fclip :: Float -> Sample -> Sample
fclip thrsh (Mono s) = Mono $ clipi thrsh s
fclip thrsh (Stereo a b) = Stereo (clipi thrsh a) (clipi thrsh b)

clip :: Float -> V.Vector Sample -> V.Vector Sample
clip thrsh = V.map $ fclip thrsh

lin :: Float -> Float -> V.Vector Float -> V.Vector Float
lin min max = V.map $ lerp min max

lgr :: Float -> Float -> V.Vector Float -> V.Vector Float
lgr min max = V.map $ \v -> min + (max-min) * logBase 10 v

delay :: Int -> IO (V.Vector Sample -> IO(V.Vector Sample))
delay n = do
    r <- newIORef (V.replicate n 0)
    pure $ \v -> atomicModifyIORef r $ \v_delayed ->
        let (v1, v2) = V.splitAt (V.length v - n) v
            (v3, v4) = V.splitAt (V.length v) v_delayed
        in if V.length v >= n then (v2,  V.append v_delayed v1) else (V.append v4 v, v3)

fdbk' :: Sample -> (V.Vector Sample -> IO(V.Vector Sample)) -> (V.Vector Sample -> IO(V.Vector Sample)) -> Sample -> IO((Sample,Sample))
fdbk' x f1 f2 buf = do
    out <- f1 $ V.singleton $ x + buf
    bak <- f2 $ V.singleton $ V.head out
    return (V.head bak, V.head out) 

fdbk :: IO((V.Vector Sample -> IO(V.Vector Sample)) -> (V.Vector Sample -> IO(V.Vector Sample)) -> V.Vector Sample -> IO(V.Vector Sample))
fdbk = do
    r <- newIORef 0
    pure $ \f1 f2 v -> do
        let g x = unsafePerformIO $ atomicModifyIORef r $ \buf -> unsafePerformIO $ fdbk' x f1 f2 buf
        return $ V.map g v 

fdbkdelay :: Int -> IO(Float -> V.Vector Sample -> IO(V.Vector Sample))
fdbkdelay n = do
    d <- delay n
    f <- fdbk
    pure $ \fb v -> f d (amp fb) v