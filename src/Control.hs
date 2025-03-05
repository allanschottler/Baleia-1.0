{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Control where

import Foreign.Storable
import Data.Data
import DeriveStorable (deriveStorable)

data Control = Control
  { _Gain     :: Float
  , _Pan      :: Float
  , _Q        :: Float
  , _Fdbk1    :: Float
  , _Fdbk2    :: Float
  , _Clip     :: Float
  , _DryWet   :: Float
  , _Time     :: Float
  } deriving (Data, Show)

-- Automatically derive the Storable instance.
deriveStorable ''Control

initControl :: Control
initControl = Control 1 0.5 0.7 0 0.5 1 0.5 1
