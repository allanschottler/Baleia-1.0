{-# LANGUAGE DeriveDataTypeable #-}
module Control where
import Baleia

-- data Control = Control 
--     { isByps :: Bool
--     , isMono :: Bool 
--     , vGain  :: Float
--     , vPan   :: Float
--     , vFreq  :: Float 
--     } deriving(Data)

data Control = Control 
    { vGain  :: Float
    , isByps :: Int 
    } deriving(Data)

initControl = Control 1 0
