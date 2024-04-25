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

mix :: Float -> V.Vector Sample -> V.Vector Sample -> V.Vector Sample
mix m = V.zipWith $ fmix m

famp :: Float -> Sample -> Sample
famp v s = s * (realToFrac v) 

amp :: Float -> V.Vector Sample -> V.Vector Sample
amp g = V.map $ famp g

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

delay :: Int -> V.Vector Sample -> V.Vector Sample
delay d inp = unsafePerformIO $ do
    init <- MV.replicate d (Silence :: Sample)
    buff <- newIORef init
    inx <- newIORef 0
    let f v = unsafePerformIO $ do i <- readIORef inx; b <- readIORef buff; r <- MV.unsafeExchange b i v; writeIORef buff b; writeIORef inx ((i+1) `mod` d); return r
    return $ V.map f inp

