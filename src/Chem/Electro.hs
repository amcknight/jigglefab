module Chem.Electro
( Electro (..)
, wireStruct
) where

import Chem
import Color
import Model.Struct
import Model.Orb
import Model.Wall
import StructLibrary
import GHC.Generics

data Electro = Dormant | Active | Tired Int deriving (Show, Eq, Ord, Generic)

instance Chem Electro where
  chemColor Dormant = grey
  chemColor Active = red
  chemColor (Tired _) = blue

instance InnerChem Electro where
  innerReact (Dormant, Active) = InExchange (Active, Tired 3)
  innerReact (Dormant, Tired 0) = InExchange (Dormant, Dormant)
  innerReact (Dormant, Tired n) = InExchange (Dormant, Tired (n-1))
  innerReact (Tired 0, Tired 0) = InExchange (Dormant, Dormant)
  innerReact (Tired 0, Tired n) = InExchange (Dormant, Tired (n-1))
  innerReact (Tired m, Tired n) = InExchange (Tired (m-1), Tired (n-1))
  innerReact es = InExchange es
  allowThru _ = False

wireStruct :: Struct Electro
wireStruct = walls <> chain <> signal
  where
    walls = wallStruct (rock v1 20) <> wallStruct (rock v2 20)
    chain = linChainIncl 20 v1 v2 Dormant
    signal = orbStruct (Orb (x2, y2) Active)
    x1 = -12
    y1 = 12
    x2 = 12
    y2 = -12
    v1 = (x1,y1)
    v2 = (x2,y2)
 