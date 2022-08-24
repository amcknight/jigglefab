module Chem.Gate
( Gate (..)
, Logic (..)
, gate
) where

import Chem
import Geometry.Space
import Wall
import StructLibrary
import Color
import Struct
import Orb
import Enumer
import GHC.Generics

data Sig = Red | Blue deriving (Show, Eq, Ord, Generic, Enumer)
data Active = Off | On Sig deriving (Show, Eq, Ord, Generic, Enumer)
data Part = Wait Active | Go Active deriving (Show, Eq, Ord, Generic, Enumer)
data Logic = And | Fst | Snd deriving (Show, Eq, Ord, Generic, Enumer)

logic :: Logic -> Sig -> Sig -> Active
logic l newS oldS = case l of
  And -> if newS == oldS then On newS else Off
  Fst -> On oldS
  Snd -> On newS

data Gate = Wire Active | Port Side Active | Gate Part Logic deriving (Show, Eq, Ord, Generic, Enumer)

instance Chem Gate where
  chemColor (Wire (On Red)) = red
  chemColor (Wire (On Blue)) = cyan
  chemColor (Port In (On Red)) = light red
  chemColor (Port In (On Blue)) = light cyan
  chemColor (Port Out (On Red)) = dark red
  chemColor (Port Out (On Blue)) = dark cyan
  chemColor _ = grey

instance InnerChem Gate where
  innerReact (Wire Off, Wire a) = InExchange (Wire a, Wire Off)
  innerReact (Wire Off, Port Out a) = InExchange (Wire a, Port Out Off)
  innerReact (Wire a, Port In Off) = InExchange (Wire Off, Port In a)
  
  innerReact (Port In a, Gate (Wait Off) log) = InExchange (Port In Off, Gate (Wait a) log)
  innerReact (Port Out Off, Gate (Go a) log) = InExchange (Port Out a, Gate (Wait Off) log)
  innerReact (Port In (On s1), Gate (Wait (On s2)) log) = InExchange (Port In Off, Gate (Go (logic log s1 s2)) log)

  innerReact cs = InExchange cs
  allowThru _ = False

gateStruct :: Int -> Gate -> Struct Gate
gateStruct slack c = rocks <> signals <> chains <> ports <> gate
  where
    d = 10
    in1V = (-d,-d)
    in2V = (d,-d)
    gateV = (0, 0)
    outV = (0, d)
    gap = 1/2
    inPort1V = (-gap,-gap)
    inPort2V = ( gap,-gap)
    outPortV = (0, gap)

    rocks = wallStruct (rock in1V 1) <> wallStruct (rock in2V 1) <> wallStruct (rock outV 1)
    s1 = orbStruct $ Orb in1V $ Wire (On Red)
    s2 = orbStruct $ Orb in2V $ Wire (On Blue)
    signals = s1 <> s2
    ch1 = linChainExcl slack in1V inPort1V $ Wire Off
    ch2 = linChainExcl slack in2V inPort2V $ Wire Off
    ch3 = linChainExcl slack outV outPortV $ Wire Off
    chains = ch1 <> ch2 <> ch3
    gate = orbStruct $ Orb gateV c
    in1 = orbStruct $ Orb inPort1V $ Port In Off
    in2 = orbStruct $ Orb inPort2V $ Port In Off
    out = orbStruct $ Orb outPortV $ Port Out Off
    ports = in1 <> in2 <> out

-------

gate :: Logic -> Struct Gate
gate = gateStruct 6 . Gate (Wait Off)
