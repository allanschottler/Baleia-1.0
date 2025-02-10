module MyModule where

import Control
import qualified Data.StorableVector as V
import Baleia.Sample
import Baleia.DSP

process :: IO(Control -> V.Vector Sample -> IO(V.Vector Sample))
process = pure $ \_ -> amp 10