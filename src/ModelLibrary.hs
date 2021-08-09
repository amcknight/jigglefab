module ModelLibrary
( ballWall
, twoBallModel
, twoBallModelInner
, threeBallModel
, fourBallModel
, chainModel
, fourChains
, randomLinearModel
, randomModel
, wireModel
) where

import Model
import Ball
import Point
import Chem
import Vector
import Space
import Wall
import FormLibrary
import Form
import Utils

randomModel :: Float -> Int -> R Model
randomModel size num = do 
  f <- randomForm 50 size num
  pure $ buildModel 20 f

fourChains :: Radius -> R Model
fourChains rad = do
  f1 <- chainForm rad 150 1 (V (-800)   800)  (V (-100)   100)
  f2 <- chainForm rad 150 1 (V   800    800)  (V   100    100)
  f3 <- chainForm rad 150 1 (V   800  (-800)) (V   100  (-100))
  f4 <- chainForm rad 150 1 (V (-800) (-800)) (V (-100) (-100))
  pure $ buildModel rad $ box (V (-1000) (-1000)) (V 1000 1000) <> f1 <> f2 <> f3 <> f4

chainModel :: Radius -> Position -> Position -> R Model
chainModel rad from to = do
  c <- chainForm rad 150 1 from to
  pure $ buildModel rad $ box (V (-300) (-1000)) (V 1000 500) <> c

randomLinearModel :: Radius -> Position -> Position -> Int -> R Model
randomLinearModel rad from to num = do
  f <- randomLinearForm 150 from to num
  pure $ buildModel rad $ box (V (-300) (-1000)) (V 1000 500) <> f

ballWall :: Model 
ballWall = buildModel 200 $
  box (V (-500) (-500)) (V (-500) (-500)) <>
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

wireModel :: Radius -> Float -> Vector -> Vector -> R Model
wireModel rad speed (V x1 y1) (V x2 y2) = do
  let walls = wallForm (wallV (damp x1)) <> wallForm (wallH (damp y1)) <> wallForm (wallV (damp x2)) <> wallForm (wallH (damp y2))
  chain <- chainForm rad speed 4 (V x1 y1) (V x2 y2)
  pure $ buildModel rad $ walls <> chain
  where 
    damp x = case compare x 0 of
      LT -> x + 1
      EQ -> x
      GT -> x - 1
