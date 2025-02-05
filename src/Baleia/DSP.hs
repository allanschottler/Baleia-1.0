module Baleia.DSP where

import Foreign.Storable
import qualified Data.StorableVector as V
import qualified Data.Vector.Storable.Mutable as MV
import Data.IORef
import System.IO.Unsafe

import Baleia.Sample
import Baleia.Math
import Baleia.Time
import Control

-- | 0.5 meaning half mix of each V.Vector
fmix :: (Storable a, Fractional a) => Float -> a -> a -> a
fmix m a b = lerp a b $ realToFrac m

mix' :: (Storable a, Fractional a) => Float -> V.Vector a -> V.Vector a -> V.Vector a
mix' m = V.zipWith $ fmix m

mix :: (Storable a, Fractional a) => Float -> V.Vector a -> V.Vector a -> IO(V.Vector a)
mix m a b = return $ mix' m a b

famp :: (Storable a, Fractional a) => Float -> a -> a
famp v s = s * (realToFrac v) 

amp' :: (Storable a, Fractional a) => Float -> V.Vector a -> V.Vector a
amp' g = V.map $ famp g

amp :: (Storable a, Fractional a) => Float -> V.Vector a -> IO(V.Vector a)
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

lgr :: Float -> Float -> Float -> Float
lgr min max v = min + (max-min) * logBase 10 (v+1)

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
    pure (V.head bak, V.head out) 

fdbk :: IO((V.Vector Sample -> IO(V.Vector Sample)) -> (V.Vector Sample -> IO(V.Vector Sample)) -> V.Vector Sample -> IO(V.Vector Sample))
fdbk = do
    r <- newIORef 0
    pure $ \f1 f2 v -> do
        let g x = unsafePerformIO $ atomicModifyIORef r $ \buf -> unsafePerformIO $ fdbk' x f1 f2 buf
        pure $ V.map g v 

fdbkdelay :: Int -> IO(Float -> V.Vector Sample -> IO(V.Vector Sample))
fdbkdelay n = do
    d <- delay n
    f <- fdbk
    pure $ \fb v -> f d (amp fb) v

biFilter :: IO(Float -> Float -> V.Vector Sample -> IO(V.Vector Sample))
biFilter = do
    d1 <- delay 1
    d2 <- delay 1
    pure $ \a1 a2 v -> do
        let s1 = d1 v
        let i1 = amp a1 =<< s1
        let i2 = amp a2 =<< d2 =<< s1
        V.zipWith (+) <$> i1 <*> i2

biquadFilter :: IO(Float -> Float -> Float -> Float -> Float -> V.Vector Sample -> IO(V.Vector Sample))
biquadFilter = do
    fb <- fdbk
    bf1 <- ffFilter
    bf2 <- ffFilter
    pure $ \a0 a1 a2 b1 b2 v -> do
        let i0 = amp a0 v
        let i1 = V.zipWith (+) <$> i0 <*> bf1 a1 a2 v
        let f = bf2 (-b1) (-b2)
        fb pure f =<< i1

-- biquadFilter :: IO(Float -> Float -> Float -> Float -> Float -> V.Vector Sample -> IO(V.Vector Sample))
-- biquadFilter = do
--     fb <- fdbk
--     bi1 <- biFilter
--     bi2 <- biFilter
--     pure $ \a0 a1 a2 b1 b2 v -> do
--         let i0 = amp a0 v
--         let s1 = V.zipWith (+) <$> i0 <*> bi1 a1 a2 v
--         fb pure (bi2 (-b1) (-b2)) =<< s1

lp1Filter :: IO(Float -> V.Vector Sample -> IO(V.Vector Sample))
lp1Filter = do
    biq <- biquadFilter
    pure $ \freq v -> do
        let theta = freq * 2 * pi_c / 44100
        let gama = if Prelude.sin theta == -1 then 0 else (Prelude.cos theta) / (1 + Prelude.sin theta)
        let a0 = (1 - gama)/2
        let a1 = a0 
        let a2 = 0
        let b1 = -gama
        let b2 = 0
        biq a0 a1 a2 b1 b2 v

lp2Filter :: IO(Float -> Float -> V.Vector Sample -> IO(V.Vector Sample))
lp2Filter = do
    biq <- biquadFilter
    pure $ \freq q v -> do
        let theta = freq * 2 * pi_c / 44100
        let d = if q == 0 then 0 else 1 / q
        let f = 0.5 * d * (Prelude.sin theta)
        let beta = if f == -1 then 0 else 0.5 * (1 - f) / (1 + f)
        let gama = (0.5 + beta) * (Prelude.cos theta)
        let a0 = 0.5 * (0.5 + beta - gama)
        let a1 = 0.5 + beta - gama 
        let a2 = a0
        let b1 = -2 * gama
        let b2 = 2 * beta
        biq a0 a1 a2 b1 b2 v

ffFilter :: IO(Float -> Float -> V.Vector Sample -> IO(V.Vector Sample))
ffFilter = do
    d <- delay 1
    pure $ \a0 a1 v -> do
        let i1 = amp a0 v
        let i2 = amp a1 =<< d v
        V.zipWith (+) <$> i1 <*> i2

fdbk2 :: IO(Float -> Float -> Float -> V.Vector Sample -> IO(V.Vector Sample))
fdbk2 = do
    d1 <- delay 1
    d2 <- delay 1
    fb <- fdbk
    pure $ \a0 b1 b2 v -> do
        let i0 = amp a0 v
        let s1 = d1 v
        let i1 = amp (-b1) =<< s1
        let s2 = d2 =<< s1
        let i2 = amp (-b2) =<< s2
        let f1 = \v -> V.zipWith (+) <$> i1 <*> i2
        fb pure f1 v

resonator :: IO(Float -> Float -> V.Vector Sample -> IO(V.Vector Sample))
resonator = do
    fb <- fdbk2
    pure $ \f q v -> do
        let th = 2 * pi_c * f / 44100
        let bw = f / q
        let b2 = exp (-2 * pi_c * bw / 44100)
        let b1 = -4 * b2 * (cos th) / (1 + b2) 
        let a0 = (1 - b2) * sqrt (1 - b1 * b1 / (4 * b2))
        fb a0 b1 b2 v

fbFilter :: IO(Float -> Float -> V.Vector Sample -> IO(V.Vector Sample))
fbFilter = do
    fb <- fdbk
    d <- delay 1
    pure $ \a0 b1 v -> do
        let i1 = (\v' -> amp (-b1) =<< d v')
        fb pure i1 =<< amp a0 v
