module Core.CoreModels
( genModel
, innerBumpModel
, andGateModel
, meshModel
, headModel
) where

import Utils
import Model
import Geometry.Vector
import StructLibrary
import Geometry.Space
import Core.Core
import Core.CoreStruct
import Ball
import Point
import Pair
import Geometry.Angle
import Wall
import Time
import Orb
import Struct

genModel :: R (Model Core)
genModel = do
  let speed = 3
  let gap = pair $ 1/2
  let v1 = pair 300
  let v2 = pair (-300)
  let signal = orbStruct $ Orb (v1 |+ (4 |* gap)) (Wire (On Red))
  let sense = orbStruct $ Orb (v1 |+ gap) Sensor
  let chain = linChainIncl 1 v1 v2 $ Wire Off
  let gen = orbStruct $ Orb (v2 |- gap) Creator
  buildModel speed $ signal <> sense <> chain <> gen

innerBumpModel :: Core -> Core -> R (Model Core)
innerBumpModel c1 c2 = buildModel (1/4) $ orbStruct (Orb zeroV c1) <> orbStruct (Orb upLeftV c2)

andGateModel :: R (Model Core)
andGateModel = buildModel 3 $ gateStruct 3 Destroyer

meshModel :: R (Model Core)
meshModel = buildModel 8 $ meshStruct 1
  [ ((   0,  0), Wire (On Red))
  , ((-400,200), Wire (On Red))
  , ((-600,200), Wire (On Red))
  , ((-400,300), Wire (On Red))
  , ((-200,600), Wire (On Red))
  , ((-200,700), Wire (On Red))
  , (( 300,400), Wire (On Red))
  ]
  [ (0,1)
  , (1,2)
  , (1,3)
  , (4,5)
  ]
  [ (turn 0.25, (0,6))
  , (turn 0.10, (3,6))
  , (turn 0.15, (4,3))
  , (turn 0.20, (6,4))
  ]

headModel :: R (Model Core)
headModel = buildModel 4 $ pegs <> tether <> tool <> packet <> hose
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
