module Core.Core
( Core (..)
, Sig (..)
, Active (..)
) where

import Chem
import Geometry.Space
import Color

data Sig = Red | Blue deriving (Show, Eq, Ord)
data Active a = Off | On a deriving (Show, Eq, Ord)

data Core = Wire (Active Sig) | Port Side (Active Sig) | Sensor | Creator | Destroyer deriving (Show, Eq, Ord)

instance Chem Core where
  chemColor (Wire Off) = Grey 0.5
  chemColor (Wire (On Red)) = red
  chemColor (Wire (On Blue)) = cyan
  chemColor (Port Out Off) = dark yellow
  chemColor (Port Out (On Red)) = mix (dark yellow) red
  chemColor (Port Out (On Blue)) = mix (dark yellow) cyan
  chemColor (Port In Off) = dark green
  chemColor (Port In (On Red)) = mix (dark green) red
  chemColor (Port In (On Blue)) = mix (dark green) cyan
  chemColor Sensor = white
  chemColor Creator = green
  chemColor Destroyer = dark magenta

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
