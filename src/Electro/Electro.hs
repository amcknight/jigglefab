module Electro.Electro
( Electro (Dormant, Active)
) where

import Chem
import Graphics.Gloss

data Electro = Dormant | Active

instance Chem Electro where
  react = id
  prereact (_, es) = es
  chemColor Dormant = greyN 0.5
  chemColor Active = blue
