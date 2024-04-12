{-# LANGUAGE DeriveDataTypeable #-}
module MyModule where
import Baleia

data ControlIn = ControlIn 
    { isByps :: Bool
    , isMono :: Bool 
    , vGain  :: Float
    , vPan   :: Float
    , vFreq  :: Float 
    } deriving(Data)

initControlIn = ControlIn False False 1 1 0

--downRamp = Custom $ genWT 1024 (\x -> 1 - x/1024.0) --Custom [i/1024 | i <- [1023..0]]
--razor = Saw --Custom $ fromTxt "C:\\Users\\allan\\OneDrive\\Desktop\\razor.txt"

--process c i = (\a -> 0.5*a)<~ i