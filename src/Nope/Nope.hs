module Nope.Nope
( Nope (..)
) where

import Chem
import Color

data Nope = Nope deriving (Show, Eq, Ord)

instance Chem Nope where
  chemColor Nope = grey

instance InnerChem Nope
