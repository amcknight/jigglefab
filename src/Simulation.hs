module Simulation
( run
, twoBallModel
, twoBallModelInner
, threeBallModel
, fourBallModel
) where

import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Data.ViewPort (ViewPort)
import System.Random ( getStdGen, Random(randomR), StdGen )
import Data.Maybe (fromMaybe)
import Data.Array (elems)
import Data.Map (keys)
import Vector
import Space
import Time
import Chem
import Point
import Link
import Links
import Pair
import Model

run :: IO ()
run = do
  seed <- getStdGen
  simulate
    FullScreen
    black
    60
    (randomLinearModel seed 100)
    draw
    update

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

draw :: Model -> Picture
draw m = Pictures $ bodies ++ centres ++ bonds
  where
    (bodies, centres) = unzip $ fmap (drawLink (rad m) (time m)) (elems (links m))
    bonds = fmap (drawBond m) (innerIps m)

update :: ViewPort -> Duration -> Model -> Model
update vp = step

drawLink :: Radius -> Time -> Link -> (Picture, Picture)
drawLink rad t (Link point chem) = bimap (translate x y) (body bodyColor rad, innerPoint centerColor)
  where
    (x, y) = posAt t point
    (bodyColor, centerColor) = chemColors chem

drawBond :: Model -> IP -> Picture
drawBond m ip = Color white $ line [p1, p2]
  where
    (p1, p2) = bimap (posAt (time m)) $ points $ linksByI m ip

body :: Color -> Radius -> Picture
body color rad = Color color $ circleSolid rad

innerPoint :: Color -> Picture
innerPoint color = Color color $ circleSolid 1

chemColors :: Chem -> (Color, Color)
chemColors chem = case desire chem of
  EQ -> (greyN 0.5, white)
  GT -> (red, white)
  LT -> (green, white)
