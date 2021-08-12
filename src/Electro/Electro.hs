module Electro.Electro
( Electro (Dormant, Active)
) where

import Chem
import Graphics.Gloss
import Pallet
import Data.Tuple

data Electro = Dormant | Active deriving Show

instance Chem Electro where
  react (es, s) = (swap es, s)
  prereact (es, _) = es
  chemColor Dormant p = getNeutral p
  chemColor Active p = getHot p
