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
    , _Bypass   :: Float
    , _Freq3    :: Float
    , _Q        :: Float
    , _Mix      :: Float
    , _Peak     :: Float
    , _Pan      :: Float
    , _Delay    :: Float
    } deriving Data

initControl = Control 0.1 0 0.5 0.45 0.45 0.5 0 1 0 0 0 0.5 0
{-# NOINLINE initControl #-}

process = proc (c, _) -> do
    let minFreq  = logr 60 8000   $ _Freq1 c
        maxFreq  = logr 60 8000   $ _Freq2 c
        lfoSpeed = logr 0.1 20    $ _Time c
        ff       = logr 60 22000  $ _Freq3 c
        qq       = lerp 0.707 20  $ _Q c
        pp       = bipol          $ _Pan c
        tt       = lerp 200 44100 $ _Delay c
    l <- oscSqr -< lfoSpeed
    let f = logr minFreq maxFreq $ unipol l
    o <- oscSaw -< f
    pf <- lpf -< (ff, qq, o)
    d <- fdbkDelayDyn -< (tt, _Fdbk c, pf) 
    let x = lerp pf d $ _DryWet c
        a = x * _Gain c
        p = pan pp $ mono a
        pk = if l > 0.9 then 1 else 0
    returnA -< (c { _Peak = pk }, p)
{-# INLINE process #-}


-- initControl2 = [
--     In "Gain" 0.1 0 1
--   , In "Freq1" 0 60 8000
--   , In "Freq2" 0.5 60 8000
--   , In "Fdbk" 0.45 0.1 0.9
--   , In "DryWet" 0.45 0 1
--   , In "Time" 0.5 0.1 20
--   , In "Bypass" 0 0 1
--   , In "Freq3" 1 60 22000
--   , In "Q" 0.707 -1.4142 1.4142
--   , In "Pan" 0 -1 1
--   , Out "Peak" 0 0 1
-- ]