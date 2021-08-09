module Chemy
( Chemy
, react, prereact, chemColor
, Sided
, Chemies
) where

import Space
import Graphics.Gloss

type Sided a = (Side, Chemies a)

type Chemies a = (a, a)

class Chemy a where
  react :: Sided a -> Sided a
  prereact :: Sided a -> (a, a)
  chemColor :: a -> Color
