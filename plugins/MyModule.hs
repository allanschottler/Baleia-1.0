module MyModule where

process :: Control -> V.Vector Sample -> V.Vector Sample
process ctrl i = amp (1-g) $ pan p i
    where g = vGain ctrl
          p = vPan ctrl