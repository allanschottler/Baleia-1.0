module MyModule where

import Control
import qualified Data.StorableVector as V
import Baleia.Sample
import Baleia.DSP

process :: Control -> V.Vector Sample -> V.Vector Sample
process ctrl i = amp (1-g) $ pan p i
    where g = vGain ctrl
          p = vPan ctrl