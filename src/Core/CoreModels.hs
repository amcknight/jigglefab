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
import FormLibrary
import Form
import Geometry.Space
import Core.Core
import Core.CoreForm
import Ball
import Point
import Pair
import Geometry.Angle
import Wall

genModel :: Radius -> R (Model Core)
genModel rad = do
  let signal = ballForm $ Ball (Point (v1 |+ (4 |* gap)) (5 |* (-speed,-speed))) (Wire (On Red))
  let sense = ballForm $ Ball (Point (v1 |+ gap) (pair 1)) Sensor
  chain <- linChainFormIncl rad speed 1 v1 v2 $ Wire Off
  let gen = ballForm $ Ball (Point (v2 |- gap) (pair (-1))) Creator
  pure $ buildModel rad $ signal <> sense <> chain <> gen
  where
    speed = 150
    gap = pair $ rad/2
    v1 = pair 300
    v2 = pair (-300)

innerBumpModel :: Radius -> Core -> Core -> Model Core
innerBumpModel rad c1 c2 = buildModel rad $
  ballForm (Ball (Point (pair 0) (speed, 0)) c1) <>
  ballForm (Ball (Point (pair 1) (0, speed)) c2)
  where speed = rad/4

andGateModel :: Radius -> R (Model Core)
andGateModel rad = do
  andGate <- gateForm 150 3 Destroyer
  pure $ buildModel rad andGate

meshModel :: R (Model Core)
meshModel = do
  let rad = 25
  form <- meshForm rad 200 1
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
  pure $ buildModel rad form

headModel :: R (Model Core)
headModel = do
  let speed = 200
  let boxRad = 600
  let rad = 99
  let space = 0.95 * rad
  let up = space |* upV
  let right = space |* rightV
  let tethSlack = 1
  let toolMid = boxRad |* upV
  let toolLeft = toolMid
  let toolRight = toolMid
  let toolBottom = toolMid |- up
  let toolTop = toolMid |+ up
  let leftPeg  = boxRad |* upLeftV
  let rightPeg = boxRad |* upRightV
  let backPeg = boxRad |* downV
  let sigs = [Wire (On Red), Wire (On Red), Wire (On Blue), Wire (On Red)]
  let sigTopPos = backPeg |+ (fromIntegral (length sigs - 1) |* up)
  let pegs = mconcat $ fmap (\peg -> wallForm (Circle peg rad)) [leftPeg, rightPeg, backPeg]
  inPort <- ballFormAt speed toolBottom $ Port In Off
  outPort <- ballFormAt speed toolTop $ Port Out Off
  let ports = inPort <> outPort
  receiver <- ballFormAt speed toolMid $ Wire Off
  tip <- linChainFormExcl rad speed 0 toolTop (toolTop |+ (3 |* up)) (Wire Off)
  let tool = ports <> receiver <> tip
  leftTether <- linChainFormExcl rad speed tethSlack leftPeg toolLeft $ Wire Off
  rightTether <- linChainFormExcl rad speed tethSlack rightPeg toolRight $ Wire Off
  let tether = leftTether <> rightTether
  packet <- sigForm speed backPeg sigTopPos sigs
  hose <- linChainFormExcl rad speed 5 sigTopPos toolBottom $ Wire Off
  pure $ buildModel rad $ pegs <> tether <> tool <> packet <> hose
