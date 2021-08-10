module Chem
( Chem
, react, prereact, chemColor
) where

import Space
import Graphics.Gloss
import Pallet
import Pair

class Chem a where
  react :: Sided a -> Sided a
  prereact :: Sided a -> P a
  chemColor :: a -> Pallet -> Color
