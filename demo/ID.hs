{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveDataTypeable #-}
module ID where
import Baleia

data Control = Control
  { _Gain :: Float
  } deriving Data

initControl = Control 0.1
{-# NOINLINE initControl #-}

process = proc (c, s) -> do
  returnA -< (c, s)
{-# INLINE process #-}