{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SineFM where
import Baleia

data Control = Control
    { _Gain     :: Float
    , _Freq1    :: Float
    , _Freq2    :: Float
    , _Fdbk     :: Float
    , _DryWet   :: Float
    , _Time     :: Float
    , _Bypass   :: Float
    } deriving Data

initControl = Control 0.1 0 0.5 0.45 0.45 0.5 0
{-# NOINLINE initControl #-}

process = proc (c, s) -> do
    minVal   <- lgr     -< (60,  8000, _Freq1 c)
    maxVal   <- lgr     -< (60,  8000, _Freq2 c)
    lfoSpeed <- lgr     -< (0.1, 20,   _Time c)
    l        <- lfoRamp -< lfoSpeed
    f        <- lgr     -< (minVal, maxVal, l)
    o        <- oscSin  -< f
    a        <- amp     -< (_Gain c, o)
    d        <- delay   -< (_Fdbk c, a)
    x        <- lin     -< (a, d, _DryWet c)
    y        <- lin     -< (x, fst s, snap $ _Bypass c)
    m        <- mono    -< y
    returnA  -< (c, m)
    where delay = fdbkDelay (3 beats `div` 4) 
{-# INLINE process #-}