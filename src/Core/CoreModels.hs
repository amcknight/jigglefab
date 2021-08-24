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
  let signal = ballForm $ Ball (Point (v1 |+ (4 |* gap)) (5 |* (-speed,-speed))) Active
  let sense = ballForm $ Ball (Point (v1 |+ gap) (pair 1)) Sensor
  chain <- linChainFormIncl rad speed 1 v1 v2 Dormant
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
    [ ((   0,  0), Active)
    , ((-400,200), Active)
    , ((-600,200), Active)
    , ((-400,300), Active)
    , ((-200,600), Active)
    , ((-200,700), Active)
    , (( 300,400), Active)
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
  let rad = 30
  let space = rad-1
  let speed = 100
  let tethSlack = 3
  let toolLeft = (0,0)
  let toolBottom = (0,0)
  let toolRight = (0,0)
  let leftPeg = (-500,0)
  let rightPeg = (500,0)
  let backPeg = (0,-1000)
  let sigs = [Active, Active, Active, Active]
  let up = (0,space)
  let sigTopPos = backPeg |+ (fromIntegral (length sigs -1) |* up)
  let pegs = wallForm (Circle leftPeg rad) <> wallForm (Circle rightPeg rad) <> wallForm (Circle backPeg rad)
  port <- ballFormAt speed toolBottom Sensor
  head <- ballFormAt speed (toolBottom |+ up) Creator
  let tool = port <> head
  leftTether <- linChainFormExcl rad speed tethSlack leftPeg toolLeft Dormant
  rightTether <- linChainFormExcl rad speed tethSlack rightPeg toolRight Dormant
  let tether = leftTether <> rightTether
  packet <- sigForm speed backPeg sigTopPos sigs
  hose <- linChainFormExcl rad speed 5 sigTopPos toolBottom Dormant
  pure $ buildModel rad $ pegs <> tether <> tool <> packet <> hose
