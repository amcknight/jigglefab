module Core.Core
( Core (..)
, Sig (..)
, Active (..)
) where

import Chem
import Pallet
import Geometry.Space

data Sig = Red | Blue deriving (Show, Eq, Ord)
data Active a = Off | On a deriving (Show, Eq, Ord)

data Core = Wire (Active Sig) | Port Side (Active Sig) | Sensor | Creator | Destroyer deriving (Show, Eq, Ord)

instance Chem Core where
  chemColor (Wire Off) = getNeutral
  chemColor (Wire (On Red)) = getHot
  chemColor (Wire (On Blue)) = getCold
  chemColor (Port _ Off) = getNeutral
  chemColor (Port _ (On Red)) = getHot
  chemColor (Port _ (On Blue)) = getCold
  chemColor Sensor = getCool
  chemColor Creator = getWarm
  chemColor Destroyer = getWarm

instance InnerChem Core where
  innerReact (Wire Off, Wire a) = InExchange (Wire a, Wire Off)
  innerReact (Wire Off, Port Out a) = InExchange (Wire a, Port Out Off)
  innerReact (Wire a, Port In Off) = InExchange (Wire Off, Port In a)
  innerReact (Wire a, Sensor) = InExchange (Wire Off, Wire a)
  innerReact (Wire a, Creator) = InBirth (Wire Off, Wire Off) (Wire Off)
  innerReact (Wire a, Destroyer) = InRightOnly Destroyer
  innerReact cs = InExchange cs
  
  allowThru ((Wire a, Sensor), Out) = True
  allowThru sc = False
