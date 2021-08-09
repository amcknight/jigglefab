module ModelLibrary
( ballWall
, twoBallModel
, twoBallModelInner
, threeBallModel
, fourBallModel
, chainModel
, randomLinearModel
, randomModel
) where

import System.Random
import Model
import Ball
import Point
import Chem
import Vector
import Vectors
import Space
import Wall
import FormLibrary
import Form

randomModel :: StdGen -> Float -> Int -> Model
randomModel seed size num = buildModel 20 $ randomForm seed 50 size num

chainModel :: StdGen -> Radius -> Position -> Position -> Model
chainModel seed rad from to = randomLinearModel seed rad from to $ round $ dist (from,to) / (rad-0.001) + 1

randomLinearModel :: StdGen -> Radius -> Position -> Position -> Int -> Model
randomLinearModel seed rad from to num = buildModel rad $ box (V (-300) (-1000), V 1000 500) <> randomLinearForm seed 150 from to num

ballWall :: Model 
ballWall = buildModel 200 $
  box (V (-500) (-500), V (-500) (-500)) <>
  ballForm (Ball (Point zeroV (V 120 40)) chem1)

twoBallModel :: Model
twoBallModel = buildModel 250 $
  wallForm (wallV 300) <>
  ballForm (Ball (Point zeroV (V 120 40)) chem1) <>
  ballForm (Ball (Point (V 1000 30) (V (-200) 40)) chem1)

twoBallModelInner :: Model
twoBallModelInner = buildModel 250 $
  wallForm (wallV 0) <>
  ballForm (Ball (Point zeroV (V 90 30)) chem1) <>
  ballForm (Ball (Point (V 10 30) (V (-150) 30)) chem1)

threeBallModel :: Model
threeBallModel = buildModel 250 $
  wallForm (wallV 1200) <>
  ballForm (Ball (Point (V 0 (-100)) (V 180 60)) chem1) <>
  ballForm (Ball (Point (V 1000 (-150)) (V (-150) 30)) chem1) <>
  ballForm (Ball (Point (V 500 1000) (V 0 (-90))) chem1)

fourBallModel :: Model
fourBallModel = buildModel 250 $
  ballForm (Ball (Point (V 0 (-100)) (V 60 20)) chem1) <>
  ballForm (Ball (Point (V 1000 (-150)) (V (-50) 10)) chem1) <>
  ballForm (Ball (Point (V 500 1000) (V 0 (-30))) chem1) <>
  ballForm (Ball (Point (V 700 1000) (V 0 (-20))) chem1)
