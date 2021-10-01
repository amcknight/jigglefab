module Valence.ValenceModels
( ballWall
, twoBallModel
, twoBallModelInner
, threeBallModel
, fourBallModel
) where

import Model
import Geometry.Vector
import Point
import Ball
import Form
import Wall
import Utils
import StructLibrary
import Valence.Valence
import Struct
import Orb

ballWall :: R (Model Valence)
ballWall = do
  let walls = box (-10,-10) (10, 10)
  let orb = orbStruct $ Orb zeroV $ vale 1
  buildModel 3 $ walls <> orb

twoBallModel :: R (Model Valence)
twoBallModel = buildModel 3 $
  orbStruct (Orb zeroV (vale 1)) <>
  orbStruct (Orb (20, 0.6) (vale 1))

twoBallModelInner :: R (Model Valence)
twoBallModelInner = buildModel 3 $
  wallStruct (VLine 0) <>
  orbStruct (Orb zeroV (vale 1)) <>
  orbStruct (Orb (0.2, 0.6) (vale 1))

threeBallModel :: R (Model Valence)
threeBallModel = buildModel 3 $
  wallStruct (VLine 1200) <>
  orbStruct (Orb (0,-2) (vale 1)) <>
  orbStruct (Orb (20,-3) (vale 1)) <>
  orbStruct (Orb (10, 20) (vale 1))

fourBallModel :: R (Model Valence)
fourBallModel = buildModel 3 $
  orbStruct (Orb (0,-2) (vale 1)) <>
  orbStruct (Orb (20,-3) (vale 1)) <>
  orbStruct (Orb (10, 20) (vale 1)) <>
  orbStruct (Orb (14, 20) (vale 1))
