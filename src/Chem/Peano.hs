module Chem.Peano
( Peano (..)
, placement
) where

import Chem
import Color
import Geometry.Vector
import Wall
import StructLibrary
import Struct
import GHC.Generics
import Enumer

data Peano = No | Yes | Tail | Wrap Peano deriving (Show, Eq, Ord, Generic, Enumer)

wrap :: Int -> Peano -> Peano
wrap 0 = id
wrap n = Wrap . wrap (n-1)

instance Chem Peano where
  chemColor No = grey
  chemColor Yes = red
  chemColor Tail = magenta
  chemColor (Wrap _) = green

instance InnerChem Peano where
  innerReact (No, Tail) = InExchange (No, No)
  innerReact (Yes, Tail) = InExchange (Yes, No)
  innerReact (No, Wrap c) = InExchange (c, Tail)
  innerReact cs = InExchange cs

placement :: Struct Peano
placement = string <> walls
  where
    size = 66.666
    slack = 30
    left = size |* leftV
    right = size |* rightV
    sig = wrap 50 Yes
    string = cappedLinChainExcl slack left right [sig] No []
    walls = mconcat $ fmap (\p -> wallStruct (rock p 1)) [left, right]
