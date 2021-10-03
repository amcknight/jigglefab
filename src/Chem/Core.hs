module Chem.Core
( Core (..)
, Sig (..)
, Active (..)
) where

import qualified Data.Vector as V

import Chem
import Geometry.Space
import Color
import Struct
import Geometry.Vector
import Pair
import Geometry.Angle
import Wall
import Orb
import StructLibrary

data Sig = Red | Blue deriving (Show, Eq, Ord)
data Active a = Off | On a deriving (Show, Eq, Ord)

data Core = Wire (Active Sig) | Port Side (Active Sig) | Sensor | Creator | Destroyer deriving (Show, Eq, Ord)

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
  innerReact (Wire a, Creator) = InBirth (Wire Off, Wire Off) (Wire Off)
  innerReact (Wire a, Destroyer) = InRightOnly Destroyer
  innerReact cs = InExchange cs
  
  allowThru ((Wire a, Sensor), Out) = True
  allowThru sc = False


gateStruct :: Int -> Core -> Struct Core
gateStruct slack c = rocks <> signals <> chains <> gate
  where
    rocks = wallStruct (Circle in1V rad) <> wallStruct (Circle in2V rad) <> wallStruct (Circle outV rad)
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

gen :: Struct Core
gen = signal <> sense <> chain <> gen
  where
    speed = 3
    gap = pair $ 1/2
    v1 = pair 300
    v2 = pair (-300)
    signal = orbStruct $ Orb (v1 |+ (4 |* gap)) (Wire (On Red))
    sense = orbStruct $ Orb (v1 |+ gap) Sensor
    chain = linChainIncl 1 v1 v2 $ Wire Off
    gen = orbStruct $ Orb (v2 |- gap) Creator

innerBump :: Core -> Core -> Struct Core
innerBump c1 c2 = orbStruct (Orb zeroV c1) <> orbStruct (Orb upLeftV c2)

andGate :: Struct Core
andGate = gateStruct 3 Destroyer

----------

mesh :: Struct Core
mesh = meshStruct 1
  [ ((   0,  0), Wire (On Red))
  , ((-8,4), Wire (On Red))
  , ((-12,4), Wire (On Red))
  , ((-8,6), Wire (On Red))
  , ((-4,12), Wire (On Red))
  , ((-4,14), Wire (On Red))
  , (( 6,8), Wire (On Red))
  ]
  [ (0,1)
  , (1,2)
  , (1,3)
  , (4,5)
  ]
  [ (toRadian 0.25, (0,6))
  , (toRadian 0.10, (3,6))
  , (toRadian 0.15, (4,3))
  , (toRadian 0.20, (6,4))
  ]

headStruct :: Struct Core
headStruct = pegs <> tether <> tool <> packet <> hose
  where
    boxRad = 12
    gap = 0.95
    up = gap |* upV
    right = gap |* rightV
    tethSlack = 1
    toolMid = boxRad |* upV
    toolLeft = toolMid
    toolRight = toolMid
    toolBottom = toolMid |- up
    toolTop = toolMid |+ up
    leftPeg  = boxRad |* upLeftV
    rightPeg = boxRad |* upRightV
    backPeg = boxRad |* downV
    sigs = [Wire (On Red), Wire (On Red), Wire (On Blue), Wire (On Red)]
    sigTopPos = backPeg |+ (fromIntegral (length sigs - 1) |* up)
    pegs = mconcat $ fmap (\peg -> wallStruct (Circle peg 1)) [leftPeg, rightPeg, backPeg]
    inPort = orbStruct $ Orb toolBottom $ Port In Off
    outPort = orbStruct $ Orb toolTop $ Port Out Off
    ports = inPort <> outPort
    receiver = orbStruct $ Orb toolMid $ Wire Off
    tip = linChainExcl 1 toolTop (toolTop |+ (3 |* up)) (Wire Off)
    tool = ports <> receiver <> tip
    leftTether = linChainExcl tethSlack leftPeg toolLeft $ Wire Off
    rightTether = linChainExcl tethSlack rightPeg toolRight $ Wire Off
    tether = leftTether <> rightTether
    packet = sig backPeg sigTopPos sigs
    hose = linChainExcl 5 sigTopPos toolBottom $ Wire Off
