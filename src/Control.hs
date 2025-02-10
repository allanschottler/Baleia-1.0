{-# LANGUAGE DeriveDataTypeable #-}
module Control where
import Baleia

data Control = Control 
    { kByps :: Bool
    , kGain  :: Float
    , kP1  :: Float
    , kP2  :: Float
    , kP3  :: Float
    , kP4  :: Float
    , kP5  :: Float
    , kP6  :: Float
    , kP7  :: Float
    , kP8  :: Float
    , kP9  :: Float
    } deriving(Data)

initControl = Control False 1 1 1 1 1 1 1 1 1 1

-- kP1 = extern "P1" 1::Float