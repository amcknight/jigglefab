module Core.Core
( Core (..)
, Sig (..)
, Active (..)
) where

import Chem
import Pallet
import Geometry.Space

data Sig = Red | Blue deriving (Show, Eq, Ord)
data Active = Off | On Sig deriving (Show, Eq, Ord)

data Core = Wire Active | Sensor | Creator | Destroyer deriving (Show, Eq, Ord)

instance Chem Core where
  chemColor (Wire Off) = getNeutral
  chemColor (Wire (On Red)) = getHot
  chemColor (Wire (On Blue)) = getCold
  chemColor Sensor = getCool
  chemColor Creator = getWarm
  chemColor Destroyer = getWarm

instance InnerChem Core where
  innerReact (Wire Off, Wire a) = InExchange (Wire a, Wire Off)
  innerReact (Wire a, Sensor) = InExchange (Wire Off, Wire a)
  innerReact (Wire a, Creator) = InBirth (Wire Off, Wire Off) (Wire Off)
  innerReact (Wire a, Destroyer) = InRightOnly Destroyer
  innerReact cs = InExchange cs
  
  allowThru ((Wire a, Sensor), Out) = True
  allowThru sc = False
