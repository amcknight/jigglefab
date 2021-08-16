module Electro.Electro
( Electro (Dormant, Active)
) where

import Chem
import Graphics.Gloss
import Pallet
import Data.Tuple
import Space

data Electro = Dormant | Active deriving Show

instance Chem Electro where
  react se = Exchange $ swapPair se
  prereact (es, _) = es
  chemColor Dormant p = getNeutral p
  chemColor Active p = getHot p
