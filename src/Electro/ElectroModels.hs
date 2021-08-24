module Electro.ElectroModels
( wireModel
) where

import Utils
import Model
import Electro.Electro
import FormLibrary
import Wall
import Form
import Electro.ElectroForm
import Geometry.Space

wireModel :: Radius -> R (Model Electro)
wireModel rad = do
  let walls = wallForm (Circle v1 20) <> wallForm (Circle v2 20)
  chain <- linChainFormIncl rad speed 20 v1 v2 Dormant
  signal <- signalForm (x2, y2) speed
  pure $ buildModel rad $ walls <> chain <> signal
  where
    speed = 150
    x1 = -500
    y1 = 500
    x2 = 500
    y2 = -500
    v1 = (x1,y1)
    v2 = (x2,y2)
    damp x = case compare x 0 of
      LT -> x + 1
      EQ -> x
      GT -> x - 1
 