module Gate.Gate
( Gate (..)
, Logic (..)
, gateModel
) where

import Chem
import Form
import Utils
import Model
import Geometry.Space
import Wall
import FormLibrary
import Color
import Geometry.Vector

data Sig = Red | Blue deriving (Show, Eq, Ord)
data Active = Off | On Sig deriving (Show, Eq, Ord)
data Part = Wait Active | Go Active deriving (Show, Eq, Ord)
data Logic = And | Fst | Snd deriving (Show, Eq, Ord)

logic :: Logic -> Sig -> Sig -> Active
logic log newS oldS = case log of
  And -> if newS == oldS then On newS else Off
  Fst -> On oldS
  Snd -> On newS

data Gate = Wire Active | Port Side Active | Gate Part Logic deriving (Show, Eq, Ord)

instance Chem Gate where
  chemColor (Wire (On Red)) = red
  chemColor (Wire (On Blue)) = cyan
  chemColor (Port In (On Red)) = light red
  chemColor (Port In (On Blue)) = light cyan
  chemColor (Port Out (On Red)) = dark red
  chemColor (Port Out (On Blue)) = dark cyan
  chemColor _ = Grey 0.5

instance InnerChem Gate where
  innerReact (Wire Off, Wire a) = InExchange (Wire a, Wire Off)
  innerReact (Wire Off, Port Out a) = InExchange (Wire a, Port Out Off)
  innerReact (Wire a, Port In Off) = InExchange (Wire Off, Port In a)
  
  innerReact (Port In a, Gate (Wait Off) log) = InExchange (Port In Off, Gate (Wait a) log)
  innerReact (Port Out Off, Gate (Go a) log) = InExchange (Port Out a, Gate (Wait Off) log)
  innerReact (Port In (On s1), Gate (Wait (On s2)) log) = InExchange (Port In Off, Gate (Go (logic log s1 s2)) log)

  innerReact cs = InExchange cs
  allowThru sc = False

gateForm :: Float -> Int -> Gate -> R (Form Gate)
gateForm speed slack c = do
  let rocks = wallForm (Circle in1V rad) <> wallForm (Circle in2V rad) <> wallForm (Circle outV rad)
  s1 <- ballFormAt speed in1V $ Wire (On Red)
  s2 <- ballFormAt speed in2V $ Wire (On Blue)
  let signals = s1 <> s2
  ch1 <- linChainFormExcl rad speed slack in1V inPort1V $ Wire Off
  ch2 <- linChainFormExcl rad speed slack in2V inPort2V $ Wire Off
  ch3 <- linChainFormExcl rad speed slack outV outPortV $ Wire Off
  let chains = ch1 <> ch2 <> ch3
  gate <- ballFormAt speed gateV c
  in1 <- ballFormAt speed inPort1V $ Port In Off
  in2 <- ballFormAt speed inPort2V $ Port In Off
  out <- ballFormAt speed outPortV $ Port Out Off
  let ports = in1 <> in2 <> out
  pure $ rocks <> signals <> chains <> ports <> gate
  where
    in1V = (-d,-d)
    in2V = (d,-d)
    inPort1V = (-gap,-gap)
    inPort2V = ( gap,-gap)
    outPortV = (0, gap)
    gap = rad/2
    gateV = (0, 0)
    outV = (0, d)
    d = 500
    rad = 50

gateModel :: Radius -> Logic -> R (Model Gate)
gateModel rad log = do
  andGate <- gateForm 100 6 $ Gate (Wait Off) log
  pure $ buildModel rad andGate
