module Electro.Electro
( Electro (Dormant, Active)
) where

import Chem
import Color
import Struct
import Geometry.Vector
import Model
import Utils
import Orb
import Wall
import StructLibrary

data Electro = Dormant | Active | Tired Int deriving (Show, Eq, Ord)

instance Chem Electro where
  chemColor Dormant = Grey 0.5
  chemColor Active = red
  chemColor (Tired n) = blue

instance InnerChem Electro where
  innerReact (Dormant, Active) = InExchange (Active, Tired 3)
  innerReact (Dormant, Tired 0) = InExchange (Dormant, Dormant)
  innerReact (Dormant, Tired n) = InExchange (Dormant, Tired (n-1))
  innerReact (Tired 0, Tired 0) = InExchange (Dormant, Dormant)
  innerReact (Tired 0, Tired n) = InExchange (Dormant, Tired (n-1))
  innerReact (Tired m, Tired n) = InExchange (Tired (m-1), Tired (n-1))
  innerReact es = InExchange es
  allowThru _ = False

wireModel :: R (Model Electro)
wireModel = buildModel 3 $ walls <> chain <> signal
  where
    walls = wallStruct (Circle v1 20) <> wallStruct (Circle v2 20)
    chain = linChainIncl 20 v1 v2 Dormant
    signal = orbStruct (Orb (x2, y2) Active)
    x1 = -12
    y1 = 12
    x2 = 12
    y2 = -12
    v1 = (x1,y1)
    v2 = (x2,y2)
    damp x = case compare x 0 of
      LT -> x + 1
      EQ -> x
      GT -> x - 1
 