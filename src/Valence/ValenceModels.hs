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
  box (V (-500) (-500)) (V (-500) (-500)) <>
  ballForm (Ball (Point zeroV (V 120 40)) (vale 1))

twoBallModel :: Model Valence
twoBallModel = buildModel 250 $
  wallForm (wallV 300) <>
  ballForm (Ball (Point zeroV (V 120 40)) (vale 1)) <>
  ballForm (Ball (Point (V 1000 30) (V (-200) 40)) (vale 1))

twoBallModelInner :: Model Valence
twoBallModelInner = buildModel 250 $
  wallForm (wallV 0) <>
  ballForm (Ball (Point zeroV (V 90 30)) (vale 1)) <>
  ballForm (Ball (Point (V 10 30) (V (-150) 30)) (vale 1))

threeBallModel :: Model Valence
threeBallModel = buildModel 250 $
  wallForm (wallV 1200) <>
  ballForm (Ball (Point (V 0 (-100)) (V 180 60)) (vale 1)) <>
  ballForm (Ball (Point (V 1000 (-150)) (V (-150) 30)) (vale 1)) <>
  ballForm (Ball (Point (V 500 1000) (V 0 (-90))) (vale 1))

fourBallModel :: Model Valence
fourBallModel = buildModel 250 $
  ballForm (Ball (Point (V 0 (-100)) (V 60 20)) (vale 1)) <>
  ballForm (Ball (Point (V 1000 (-150)) (V (-50) 10)) (vale 1)) <>
  ballForm (Ball (Point (V 500 1000) (V 0 (-30))) (vale 1)) <>
  ballForm (Ball (Point (V 700 1000) (V 0 (-20))) (vale 1))

randomModel :: Float -> Int -> R (Model Valence)
randomModel size num = do 
  f <- randomForm 50 size num
  pure $ buildModel 20 f
