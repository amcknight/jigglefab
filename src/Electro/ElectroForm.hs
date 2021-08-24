module Electro.ElectroForm
( signalForm
) where

import Electro.Electro
import Form
import Utils
import Point
import Ball
import Geometry.Vector

signalForm :: Position -> Float -> R (Form Electro)
signalForm pos speed = do
  vel <- randomV speed
  pure $ ballForm (Ball (Point pos vel) Active)
