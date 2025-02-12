{-# LANGUAGE DeriveDataTypeable #-}
module Control where
import Baleia

data Control = Control 
{ _Gain  :: Float
, _Freq  :: Float
, _Q  :: Float
, _Feedback  :: Float
, _Clip  :: Float
, _DryWet  :: Float
} deriving(Data)

initControl = Control 0.5 1 0 0.5 1 0.5

-- kP1 = extern "P1" 1::Float
--presets = [Control 0.5 1 0 0.5 1 0.5,  --default
--           Control 0.5 1 0 0.5 1 1]