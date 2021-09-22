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
import Form
import FormLibrary

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
  let size = 1000
  let rad = 15
  let speed = rad*3
  let slack = 30
  let left = size |* leftV
  let right = size |* rightV
  let sig = wrap 50 Yes
  string <- cappedLinChainFormExcl rad speed slack left right [sig] No []
  let walls = mconcat $ fmap (\p -> wallForm (Circle p rad)) [left, right]
  pure $ buildModel rad $ string <> walls
