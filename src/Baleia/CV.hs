module Baleia.CV where
import Control

data CV a = Static a 
          | Dynamic (Control -> a) 
          deriving (Show, Eq)