module Core.Core
( Core (..)
) where

import Chem
import Pallet
import Geometry.Space

data Core = Dormant | Active | Sensor | Creator | Destroyer deriving (Show, Eq, Ord)

instance Chem Core where
  chemColor Dormant = getNeutral
  chemColor Active = getHot
  chemColor Sensor = getCold
  chemColor Creator = getWarm
  chemColor Destroyer = getCool

instance InnerChem Core where
  innerReact (Dormant, Active) = InExchange (Active, Dormant)
  innerReact (Active, Sensor) = InExchange (Dormant, Active)
  innerReact (Active, Creator) = InBirth (Dormant, Dormant) Dormant
  innerReact (Active, Destroyer) = InRightOnly Destroyer
  innerReact cs = InExchange cs
  
  allowThru ((Active, Sensor), Out) = True
  allowThru sc = False
