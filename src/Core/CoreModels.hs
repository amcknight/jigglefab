module Core.CoreModels
( genModel
) where

import Utils
import Model
import Vector
import FormLibrary
import Wall
import Form
import Space
import Core.Core
import Core.CoreForm
import Ball
import Point

genModel :: Radius -> R (Model Core)
genModel rad = do
  let signal = ballForm $ Ball (Point (v1 |+ (4 |* gap)) (5 |* (-speed,-speed))) Active
  let sense = ballForm $ Ball (Point (v1 |+ gap) (1,1)) Sensitized
  chain <- chainForm rad speed 1 v1 v2 Dormant
  let gen = ballForm $ Ball (Point (v2 |- gap) (-1,-1)) Creator
  pure $ buildModel rad $ signal <> sense <> chain <> gen
  where
    speed = 50
    gap = (rad/2, rad/2)
    v1 = (300,300)
    v2 = (-300,-300)
