module MyModule where
import Baleia.Internal
import Baleia.Sample
import Baleia.Time
import Control
import Control.Arrow

-- process :: SF (Control, Sample) (Control, Sample)
-- process = clip _Clip <<< amp _Gain <<< mix _DryWet <<< id_ &&& (fdbkDelay (3 beats `div` 4) (lin 0 0.5 _Fdbk1) <<< fdbkDelay (18 ms) (lin 0 0.5 _Fdbk2))
-- {-# INLINE process #-}