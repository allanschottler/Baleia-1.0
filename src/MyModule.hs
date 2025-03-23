{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveDataTypeable #-}
module MyModule where
import Baleia

data Control = Control
  { _Gain     :: Float
  , _Freq1    :: Float
  , _Freq2    :: Float
  , _Fdbk     :: Float
  , _DryWet   :: Float
  , _Time     :: Float
  , _Filter   :: Float
  , _Q        :: Float
  } deriving (Data)

initControl = Control 0.1 0 0.5 0.45 0.45 0.5 1 0
{-# NOINLINE initControl #-}

process = proc (c, _) -> do
    minVal   <- lgr     -< (60,  8000, _Freq1 c)
    maxVal   <- lgr     -< (60,  8000, _Freq2 c)
    lfoSpeed <- lgr     -< (0.1, 20,   _Time c)
    l        <- lfoRamp -< lfoSpeed
    f        <- lgr     -< (minVal, maxVal, l)
    o        <- oscSin  -< f
    -- lp       <- lowpass -< (_Filter c, _Q c, o)
    a        <- amp     -< (_Gain c, o)
    d        <- delay   -< (_Fdbk c, a)
    x        <- lin     -< (a, d, _DryWet c)
    m        <- mono    -< x
    returnA  -< (c, m)
    where delay = fdbkDelay (3 beats `div` 4) 
{-# INLINE process #-}