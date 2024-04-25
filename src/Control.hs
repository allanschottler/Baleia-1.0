{-# LANGUAGE DeriveDataTypeable #-}
module Control where
import Baleia

data Control = Control 
    { isByps :: Bool
    , isMono :: Bool 
    , vGain  :: Float
    , vPan   :: Float
    , vFreq  :: Float 
    } deriving(Data)

initControl = Control False False 1 1 0
