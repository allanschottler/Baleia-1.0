module Baleia.DSP where

import Foreign.Storable
import qualified Data.StorableVector as V

import Baleia.Sample
import Baleia.Math
import Control

famp :: (Fractional a) => Float -> a -> a
famp v s = s * va where va = realToFrac v

amp :: (Fractional a, Storable a) => Float -> V.Vector a -> V.Vector a
amp g = V.map $ famp g

pan :: Float -> V.Vector Sample -> V.Vector Sample
pan p = V.map $ Baleia.Sample.fpan p

clipi :: Float -> Float -> Float
clipi thrsh v = if abs v > thrsh then thrsh * signum v else v

fclip :: Float -> Sample -> Sample
fclip thrsh (Mono s) = Mono $ clipi thrsh s
fclip thrsh (Stereo a b) = Stereo (clipi thrsh a) (clipi thrsh b)

clip :: Float -> V.Vector Sample -> V.Vector Sample
clip thrsh = V.map $ fclip thrsh

lin :: Float -> Float -> V.Vector Float -> V.Vector Float
lin min max = V.map $ lerp min max

lgr :: Float -> Float -> V.Vector Float -> V.Vector Float
lgr min max = V.map $ \v -> min + (max-min) * logBase 10 v