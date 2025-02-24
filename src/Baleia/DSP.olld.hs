module Baleia.DSP where

import Foreign.Storable
import qualified Data.StorableVector as V
import qualified Data.Vector as VV
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

clip :: Float -> V.Vector Sample -> IO(V.Vector Sample)
clip thrsh v = return $ V.map (fclip thrsh) v

lin :: Float -> Float -> Float -> Float
lin = lerp

lgr :: Float -> Float -> Float -> Float
lgr min max v =
    let logMin = logBase 2 min
        logMax = logBase 2 max
        logV = logMin + (logMax - logMin) * v
    in 2 ** logV

unipol :: Float -> Float -- -1 to 1 => 0 to 1
unipol x = (x + 1) / 2

bipol :: Float -> Float -- 0 to 1 => -1 to 1
bipol x = 2 * x - 1

delay :: Int -> IO (V.Vector Sample -> IO(V.Vector Sample))
delay n = do
    r <- newIORef (V.replicate n 0)
    pure $ \v -> atomicModifyIORef r $ \v_delayed ->
        let (v1, v2) = V.splitAt (V.length v - n) v
            (v3, v4) = V.splitAt (V.length v) v_delayed
        in if V.length v >= n then (v2,  V.append v_delayed v1) else (V.append v4 v, v3)

-- | A dynamic delay with fractional delay time using linear interpolation.
--   The delay time (in samples) is given as a Float (must be <= 44100).
--   The returned function takes a delay time and an input vector,
--   updates the internal fixed-length (maxDelay) buffer, and returns an
--   output vector where each sample is read using interpolation.
dydelay :: IO (Float -> V.Vector Sample -> IO (V.Vector Sample))
dydelay = do
    let maxDelay        = 44100
        smoothingFactor = 0.01  -- adjust this value: smaller = slower smoothing
    bufRef <- newIORef (V.replicate maxDelay (rawToMono 0))
    dtRef  <- newIORef 0  -- current (smoothed) delay time
    pure $ \desiredDelayTime v -> do
        -- Smooth the delayTime parameter
        currentDelayTime <- readIORef dtRef
        let newDelayTime = (1 - smoothingFactor) * currentDelayTime + smoothingFactor * desiredDelayTime
        writeIORef dtRef newDelayTime
        -- Update the delay buffer
        atomicModifyIORef bufRef $ \buf ->
            let combined    = buf `V.append` v
                combinedLen = V.length combined
                newBuf      = V.drop (combinedLen - maxDelay) combined
                outLength   = V.length v
                out         = V.sample outLength $ \i ->
                                let pos   = fromIntegral maxDelay - newDelayTime + fromIntegral i
                                    j     = floor pos          -- integer part of pos
                                    frac  = pos - fromIntegral j -- fractional part of pos
                                    -- Use circular indexing
                                    j'    = j `mod` maxDelay
                                    jNext = (j + 1) `mod` maxDelay
                                    s1    = V.index newBuf j'
                                    s2    = V.index newBuf jNext
                                    interp = (rawToMono (1 - frac)) * s1 + (rawToMono frac) * s2
                                in interp
            in (newBuf, out)

fdbk' :: IORef Sample
     -> V.Vector Sample 
     -> Int
     -> (V.Vector Sample -> IO (V.Vector Sample)) 
     -> (V.Vector Sample -> IO (V.Vector Sample))
     -> V.Vector Sample 
     -> IO (V.Vector Sample)
fdbk' ref acc idx f1 f2 input
    | idx >= V.length input = return acc
    | otherwise = do
        let x = V.index input idx
        buf <- readIORef ref
        f1Result <- f1 $ V.singleton (x + buf)
        let out = V.head f1Result
        f2Result <- f2 $ V.singleton out
        let feedback = V.head f2Result
        writeIORef ref feedback
        fdbk' ref (V.snoc acc out) (idx + 1) f1 f2 input

fdbk :: IO((V.Vector Sample -> IO (V.Vector Sample)) 
     -> (V.Vector Sample -> IO (V.Vector Sample))
     -> V.Vector Sample 
     -> IO (V.Vector Sample))
fdbk = do
    ref <- newIORef 0
    pure $ fdbk' ref V.empty 0

fdbkdelay :: Int -> IO(Float -> V.Vector Sample -> IO(V.Vector Sample))
fdbkdelay n = do
    f <- fdbk
    d <- delay n
    pure $ \fb -> f d (amp fb)

delayFeedback :: Int -> Float -> V.Vector Sample -> V.Vector Sample
delayFeedback n fb xs = ys
  where
    -- The output ys is defined recursively:
    ys = V.zipWith (+) xs (V.map (* rawToMono fb) (V.append (V.replicate n 0) ys))

dyfdbkdelay :: IO(Float -> Float -> V.Vector Sample -> IO(V.Vector Sample))
dyfdbkdelay = do
    f <- fdbk
    d <- dydelay
    pure $ \n fb -> f (d n) (amp fb)

ffFilter :: IO(Float -> Float -> V.Vector Sample -> IO(V.Vector Sample))
ffFilter = do
    d <- delay 1
    pure $ \a0 a1 v -> do
        let i1 = amp a0 v
        let i2 = amp a1 =<< d v
        V.zipWith (+) <$> i1 <*> i2

ff2Filter :: IO(Float -> Float -> Float -> V.Vector Sample -> IO(V.Vector Sample))
ff2Filter = do
    d1 <- delay 1
    d2 <- delay 1
    pure $ \a0 a1 a2 v -> do
        let i1 = amp a0 v
        dd <- d1 v
        let i2 = amp a1 dd
        let i3 = amp a2 =<< d2 dd
        V.zipWith (+) <$> i2 <*> i3

fbFilter :: IO(Float -> Float -> V.Vector Sample -> IO(V.Vector Sample))
fbFilter = do
    fb <- fdbk
    d <- delay 1
    pure $ \a0 b1 v -> do
        let i1 = (\v' -> amp (-b1) =<< d v')
        fb pure i1 =<< amp a0 v

fb2Filter' d1 d2 b1 b2 dd = do    
    let i2 = amp (-b1) dd
    let i3 = amp (-b2) =<< d2 dd
    V.zipWith (+) <$> i2 <*> i3

fb2Filter :: IO(Float -> Float -> V.Vector Sample -> IO(V.Vector Sample))
fb2Filter = do
    fb <- fdbk
    d1 <- delay 1
    d2 <- delay 1
    pure $ \b1 b2 v -> fb pure (fb2Filter' d1 d2 b1 b2) =<< d1 v

ffquadFilter :: IO(Float -> Float -> Float -> Float -> Float -> V.Vector Sample -> IO(V.Vector Sample))
ffquadFilter = do
    fb <- fdbk
    bf1 <- ffFilter
    bf2 <- ffFilter
    pure $ \a0 a1 a2 b1 b2 v -> do
        let i0 = amp a0 v
        let i1 = V.zipWith (+) <$> i0 <*> bf1 a1 a2 v
        let f = bf2 (-b1) (-b2)
        fb pure f =<< i1

biquadFilter :: IO(Float -> Float -> Float -> Float -> Float -> V.Vector Sample -> IO(V.Vector Sample))
biquadFilter = do
    ff2 <- ff2Filter
    fb2 <- fb2Filter  
    pure $ \a0 a1 a2 b1 b2 v -> fb2 b1 b2 =<< ff2 a0 a1 a2 v

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

lp2FilterV2 :: IO(Float -> Float -> V.Vector Sample -> IO(V.Vector Sample))
lp2FilterV2 = do
    biq <- ffquadFilter
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
