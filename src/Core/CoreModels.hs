module Core.CoreModels
( genModel
, innerBumpModel
) where

import Utils
import Model
import Vector
import FormLibrary
import Form
import Space
import Core.Core
import Core.CoreForm
import Ball
import Point
import Pair

genModel :: Radius -> R (Model Core)
genModel rad = do
  let signal = ballForm $ Ball (Point (v1 |+ (4 |* gap)) (5 |* (-speed,-speed))) Active
  let sense = ballForm $ Ball (Point (v1 |+ gap) (pair 1)) Sensor
  chain <- chainForm rad speed 1 v1 v2 Dormant
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