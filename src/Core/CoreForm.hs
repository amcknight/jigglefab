module Core.CoreForm
( gateForm
) where

import Utils
import Form
import Core.Core
import Ball
import Wall
import Point
import FormLibrary

gateForm :: Float -> Int -> Core -> R (Form Core)
gateForm speed slack c = do
  let rocks = wallForm (Rock in1V rad) <> wallForm (Rock in2V rad) <> wallForm (Rock outV rad)
  s1 <- ballFormAt speed in1V Active
  s2 <- ballFormAt speed in2V Active
  let signals = s1 <> s2
  ch1 <- chainFormExcl rad speed slack in1V gateV Dormant
  ch2 <- chainFormExcl rad speed slack in2V gateV Dormant
  ch3 <- chainFormExcl rad speed slack outV gateV Dormant
  let chains = ch1 <> ch2 <> ch3
  gate <- ballFormAt speed gateV c
  pure $ rocks <> signals <> chains <> gate
  where
    in1V = (-d,-d)
    in2V = (d,-d)
    gateV = (0, 0)
    outV = (0, d)
    d = 500
    rad = 50