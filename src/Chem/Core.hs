module Chem.Core
( Core (..)
, Sig (..)
, Active (..)
, andGate, meshStruct
) where

import qualified Data.Vector as V

import Chem
import Geometry.Space
import Color
import Model.Struct
import Geometry.Vector
import Pair
import Geometry.Angle
import Model.Wall
import Model.Orb
import StructLibrary
import GHC.Generics
import Enumer

data Sig = Red | Blue deriving (Show, Eq, Ord, Generic, Enumer)
data Active a = Off | On a deriving (Show, Eq, Ord, Generic, Enumer)

data Core = Wire (Active Sig) | Port Side (Active Sig) | Sensor | Creator | Destroyer deriving (Show, Eq, Ord, Generic, Enumer)

instance Chem Core where
  chemColor (Wire Off) = grey
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
  innerReact (Wire _, Creator) = InBirth (Wire Off, Wire Off) (Wire Off)
  innerReact (Wire _, Destroyer) = InRightOnly Destroyer
  innerReact cs = InExchange cs
  
  allowThru ((Wire _, Sensor), Out) = True
  allowThru _ = False


gateStruct :: Int -> Core -> Struct Core
gateStruct slack c = rocks <> signals <> chains <> gate
  where
    rocks = wallStruct (rock in1V rad) <> wallStruct (rock in2V rad) <> wallStruct (rock outV rad)
    s1 = orbStruct $ Orb in1V $ Wire $ On Red
    s2 = orbStruct $ Orb in2V $ Wire $ On Red
    signals = s1 <> s2
    ch1 = linChainExcl slack in1V gateV $ Wire Off
    ch2 = linChainExcl slack in2V gateV $ Wire Off
    ch3 = linChainExcl slack outV gateV $ Wire Off
    chains = ch1 <> ch2 <> ch3
    gate = orbStruct $ Orb gateV c
    in1V = (-d,-d)
    in2V = (d,-d)
    gateV = (0, 0)
    outV = (0, d)
    d = 500
    rad = 50

meshStruct :: Int -> [(Position, Core)] -> [P Int] -> [(Radian, P Int)] -> Struct Core
meshStruct slack preBalls bbi abbi = pegs <> linCs <> arcCs
  where
    pegs = scatterStruct preBalls
    linCs = linChainsStruct slack (V.fromList preBalls) bbi
    arcCs = arcChainsStruct slack (V.fromList preBalls) abbi

scatterStruct :: [(Position, Core)] -> Struct Core
scatterStruct [] = mempty
scatterStruct ((p,c):pcs) = orbStruct (Orb p c) <> scatterStruct pcs

linChainsStruct :: Int -> V.Vector (Position, Core) -> [P Int] -> Struct Core
linChainsStruct _ _ [] = mempty
linChainsStruct slack preBalls ((i,j):is) = c <> cs
  where
    (p1,_) = preBalls V.! i
    (p2,_) = preBalls V.! j
    c = linChainExcl slack p1 p2 $ Wire Off
    cs = linChainsStruct slack preBalls is

arcChainsStruct :: Int -> V.Vector (Position, Core) -> [(Radian, P Int)] -> Struct Core
arcChainsStruct _ _ [] = mempty
arcChainsStruct slack preBalls ((a,(i,j)):is) = c <> cs
  where
    (p1,_) = preBalls V.! i
    (p2,_) = preBalls V.! j
    c = arcChainExcl a slack p1 p2 $ Wire Off
    cs = arcChainsStruct slack preBalls is

-------------------

andGate :: Struct Core
andGate = gateStruct 3 Destroyer
