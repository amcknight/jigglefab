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
  let signal = ballForm $ Ball (Point (500, 500) (-100,-100)) Active
  let sense = ballForm $ Ball (Point (350, 350) (1,1)) Sensitized
  chain <- chainForm rad speed 1 v1 v2 Dormant
  let gen = ballForm $ Ball (Point (-350, -350) (-1,-1)) Creator
  pure $ buildModel rad $ signal <> sense <> chain <> gen
  where
    speed = 50
    x1 = 300
    y1 = 300
    x2 = -300
    y2 = -300
    v1 = (x1,y1)
    v2 = (x2,y2)
