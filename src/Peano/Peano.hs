module Peano.Peano
( Peano (..)
, placementModel
) where

import Chem
import Color
import Model
import Utils
import Geometry.Vector
import Wall
import StructLibrary
import Struct

data Peano = No | Yes | Tail | Wrap Peano deriving (Show, Eq, Ord)

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

placementModel :: R (Model Peano)
placementModel = do
  let size = 66.666
  let speed = 3
  let slack = 30
  let left = size |* leftV
  let right = size |* rightV
  let sig = wrap 50 Yes
  let string = cappedLinChainExcl slack left right [sig] No []
  let walls = mconcat $ fmap (\p -> wallStruct (Circle p 1)) [left, right]
  buildModel speed $ string <> walls
