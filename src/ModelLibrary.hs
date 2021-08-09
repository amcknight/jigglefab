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
import Control.Monad.State  

randomModel :: Float -> Int -> StdGen -> (StdGen, Model)
randomModel size num seed = (newSeed, buildModel 20 f)
  where (newSeed, f) = randomForm 50 size num seed

fourChains :: StdGen -> Radius -> (Model, StdGen)
fourChains s0 rad = (buildModel rad $ box (V (-1000) (-1000), V 1000 1000) <> f1 <> f2 <> f3 <> f4, s4)
  where
    (s1, f1) = chainForm rad 150 (V (-800)   800)  (V (-200)   200)  s0
    (s2, f2) = chainForm rad 150 (V   800    800)  (V   200    200)  s1
    (s3, f3) = chainForm rad 150 (V   800  (-800)) (V   200  (-200)) s2
    (s4, f4) = chainForm rad 150 (V (-800) (-800)) (V (-200) (-200)) s3

chainModel :: Radius -> Position -> Position -> StdGen -> (StdGen, Model)
chainModel rad from to seed = (newSeed, buildModel rad $ box (V (-300) (-1000), V 1000 500) <> c)
  where (newSeed, c) = chainForm rad 150 from to seed

randomLinearModel :: Radius -> Position -> Position -> Int -> StdGen -> (StdGen, Model)
randomLinearModel rad from to num seed = (newSeed, buildModel rad $ box (V (-300) (-1000), V 1000 500) <> f)
  where (newSeed, f) = randomLinearForm 150 from to num seed

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