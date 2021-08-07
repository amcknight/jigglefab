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
import Link
import Point
import Chem
import Vector
import Vectors
import Space
import Wall

randomModel :: StdGen -> Float -> Int -> Model
randomModel seed size num = buildModel 20 [] $ randomModel' seed size num
randomModel' :: StdGen -> Float -> Int -> [Link]
randomModel' _ _ 0 = []
randomModel' seed size num = Link (Point pos vel) (buildChem valence) : randomModel' newSeed size (num-1)
  where
    (valence, pSeed) = randomR (1, 3) seed :: (Int, StdGen)
    (pos, vSeed) = randomVIn pSeed size
    (vel, newSeed) = randomV vSeed 50

chainModel :: StdGen -> Radius -> Position -> Position -> Model
chainModel seed rad from to = randomLinearModel seed rad from to $ round $ dist (from,to) / (rad-0.001) + 1

randomLinearModel :: StdGen -> Radius -> Position -> Position -> Int -> Model
randomLinearModel seed rad from to num = buildModel rad [Wall Vertical (-300), Wall Vertical 1000, Wall Horizontal (-1000), Wall Horizontal 500] $ randomLinearModel' seed from to num
randomLinearModel' :: StdGen -> Position -> Position -> Int -> [Link]
randomLinearModel' seed from to n = zipWith toLink poss (vels seed n 150.0)
  where
    poss :: [Position]
    poss = fromTo from to n
    vels :: StdGen -> Int -> Float -> [Velocity]
    vels seed n speed = fmap (speed |*) $ take n $ randoms seed
    toLink :: Position -> Velocity -> Link
    toLink p v = Link (Point p v) chem2

ballWall :: Model 
ballWall = buildModel 200
  [ Wall Vertical (-500)
  , Wall Vertical 500
  , Wall Horizontal (-500)
  , Wall Horizontal 500
  ]
  [Link (Point zeroV (V 120 40)) chem1]

twoBallModel :: Model
twoBallModel = buildModel 250 [Wall Vertical 300]
  [ Link (Point zeroV (V 120 40)) chem1
  , Link (Point (V 1000 30) (V (-200) 40)) chem1
  ]

twoBallModelInner :: Model
twoBallModelInner = buildModel 250 [Wall Vertical 0]
  [ Link (Point zeroV (V 90 30)) chem1
  , Link (Point (V 10 30) (V (-150) 30)) chem1
  ]

threeBallModel :: Model
threeBallModel = buildModel 250 [Wall Vertical 1200]
  [ Link (Point (V 0 (-100)) (V 180 60)) chem1
  , Link (Point (V 1000 (-150)) (V (-150) 30)) chem1
  , Link (Point (V 500 1000) (V 0 (-90))) chem1
  ]

fourBallModel :: Model
fourBallModel = buildModel 250 []
  [ Link (Point (V 0 (-100)) (V 60 20)) chem1
  , Link (Point (V 1000 (-150)) (V (-50) 10)) chem1
  , Link (Point (V 500 1000) (V 0 (-30))) chem1
  , Link (Point (V 700 1000) (V 0 (-20))) chem1
  ]
