module Chem
( Chem
, Chems
, react, prereact, chemColor
, Sided
) where

import Space
import Graphics.Gloss

type Sided a = (Side, (a, a))

type Chems a = (a, a)

class Chem a where
  react :: Sided a -> Sided a
  prereact :: Sided a -> (a, a)
  chemColor :: a -> Color
