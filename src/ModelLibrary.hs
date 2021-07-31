module ModelLibrary
( twoBallModel
, twoBallModelInner
, threeBallModel
, fourBallModel
, randomLinearModel
, randomModel
) where

import Model
import Link
import Chem
import Vector
import System.Random

randomModel :: StdGen -> Float -> Int -> Model
randomModel seed size num = buildModel 20 $ randomModel' seed size num
randomModel' :: StdGen -> Float -> Int -> [Link]
randomModel' _ _ 0 = []
randomModel' seed size num = Link (pos, vel) (buildChem valence) : randomModel' newSeed size (num-1)
  where
    (valence, pSeed) = randomR (1, 3) seed :: (Int, StdGen)
    (pos, vSeed) = randomVIn pSeed size
    (vel, newSeed) = randomV vSeed 50

randomLinearModel :: StdGen -> Int -> Model
randomLinearModel seed num = buildModel 20 $ randomLinearModel' seed num
randomLinearModel' :: StdGen -> Int -> [Link]
randomLinearModel' _ 0 = []
randomLinearModel' seed n = Link ((x, 0), v) (buildChem want) : randomLinearModel' newSeed (n-1)
  where
    (want, vSeed) = randomR (2, 3) seed :: (Int, StdGen)
    x = fromIntegral n * 18.0
    (v, newSeed) = randomV vSeed 100

twoBallModel :: Model
twoBallModel = buildModel 250 
  [ Link ((0, 0), (30, 10)) chem1
  , Link ((1000, 30), (-50, 10)) chem1
  ]

twoBallModelInner :: Model
twoBallModelInner = buildModel 250
  [ Link ((0, 0), (30, 10)) chem1
  , Link ((10, 30), (-50, 10)) chem1
  ]

threeBallModel :: Model
threeBallModel = buildModel 250
  [ Link ((0, -100), (60, 20)) chem1
  , Link ((1000, -150), (-50, 10)) chem1
  , Link ((500, 1000), (0, -30)) chem1
  ]

fourBallModel :: Model
fourBallModel = buildModel 250
  [ Link ((0, -100), (60, 20)) chem1
  , Link ((1000, -150), (-50, 10)) chem1
  , Link ((500, 1000), (0, -30)) chem1
  , Link ((700, 1000), (0, -20)) chem1
  ]
  