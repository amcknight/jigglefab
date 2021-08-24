module Core.CoreModels
( genModel
, innerBumpModel
, andGateModel
, meshModel
, arcModel
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

arcModel :: Radius -> Float -> Angle -> Position -> Position -> R (Model Core)
arcModel rad speed a p1 p2 = do
  let walls = wallForm (Circle p1 (rad*2)) <> wallForm (Circle p2 (rad*2)) 
  form <- arcChainFormIncl rad speed a 2 p1 p2 Dormant
  pure $ buildModel rad (form <> walls)
  