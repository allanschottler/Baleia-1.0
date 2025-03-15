{-# LANGUAGE DeriveDataTypeable #-}
module Control where
import Data.Data

data Control = Control
  { _Gain     :: Float
  , _Pan      :: Float
  , _Freq     :: Float
  , _Q        :: Float
  , _Fdbk1    :: Float
  , _Fdbk2    :: Float
  , _Clip     :: Float
  , _DryWet   :: Float
  , _Time     :: Float
  } deriving (Data, Show)

initControl :: Control
initControl = Control 1 0.5 0 0.7 0 0.5 1 0.5 1
