{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveDataTypeable #-}
module MyModule where
import Baleia
import Baleia.Note as N

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
    l <- oscSqr -< N.a3
    r <- oscSaw -< N.a2
    returnA -< (c, mono $ (l + r)*0.5*_Gain c)
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