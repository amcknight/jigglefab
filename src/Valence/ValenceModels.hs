module Valence.ValenceModels
( ballWall
, twoBallModel
, twoBallModelInner
, threeBallModel
, fourBallModel
, randomModel
) where

import Model
import Valence.Valence
import Vector
import Point
import Ball
import Form
import Wall
import Utils
import FormLibrary
import Valence.ValenceForms

ballWall :: Model Valence
ballWall = buildModel 200 $
  box (-500,-500) (-500,-500) <>
  ballForm (Ball (Point zeroV (120, 40)) (vale 1))

twoBallModel :: Model Valence
twoBallModel = buildModel 250 $
  ballForm (Ball (Point zeroV (120, 40)) (vale 1)) <>
  ballForm (Ball (Point (1000, 30) (-200, 40)) (vale 1))

twoBallModelInner :: Model Valence
twoBallModelInner = buildModel 250 $
  wallForm (VLine 0) <>
  ballForm (Ball (Point zeroV (90, 30)) (vale 1)) <>
  ballForm (Ball (Point (10, 30) (-150, 30)) (vale 1))

threeBallModel :: Model Valence
threeBallModel = buildModel 250 $
  wallForm (VLine 1200) <>
  ballForm (Ball (Point (0,-100) (180, 60)) (vale 1)) <>
  ballForm (Ball (Point (1000,-150) (-150, 30)) (vale 1)) <>
  ballForm (Ball (Point (500, 1000) (0,-90)) (vale 1))

fourBallModel :: Model Valence
fourBallModel = buildModel 250 $
  ballForm (Ball (Point (0,-100) (60, 20)) (vale 1)) <>
  ballForm (Ball (Point (1000,-150) (-50, 10)) (vale 1)) <>
  ballForm (Ball (Point (500, 1000) (0,-30)) (vale 1)) <>
  ballForm (Ball (Point (700, 1000) (0,-20)) (vale 1))

randomModel :: Float -> Int -> R (Model Valence)
randomModel size num = do 
  f <- randomForm 50 size num
  pure $ buildModel 20 f
