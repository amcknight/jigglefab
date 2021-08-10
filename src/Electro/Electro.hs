module Electro.Electro
( Electro (Dormant, Active)
) where

import Chem
import Graphics.Gloss
import Pallet

data Electro = Dormant | Active

instance Chem Electro where
  react (s, (e1, e2)) = (s, (e2, e1))
  prereact (_, es) = es
  chemColor Dormant p = getNeutral p
  chemColor Active p = getHot p
