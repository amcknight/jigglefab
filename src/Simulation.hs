module Simulation
( run
) where

import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Data.ViewPort (ViewPort)
import System.Random ( getStdGen, Random(randomR), StdGen )
import Data.Maybe (fromMaybe)
import Vector
import Space
import Chem
import Link
import Model

run :: IO ()
run = do
  seed <- getStdGen
  simulate
    FullScreen
    black
    60
    (initialModel seed 300)
    draw
    update

initialModel :: StdGen -> Int -> Model
initialModel seed n = Model 20 $ randomLinks seed 1000 n

-- gridLinks :: StdGen -> Int -> [Link] 
-- gridLinks _ 0 = []
-- gridLinks seed n = Link ((x, y), (vx, vy)) (Chem 1 0)

randomLinks :: StdGen -> Float -> Int -> [Link]
randomLinks _ _ 0 = []
randomLinks seed size num = Link (pos, vel) (Chem valence 0) : randomLinks newSeed size (num-1)
  where
    (valence, pSeed) = randomR (1, 3) seed :: (Int, StdGen)
    (pos, vSeed) = randomVIn pSeed size
    (vel, newSeed) = randomV vSeed 50

randomLinearLinks :: StdGen -> Int -> [Link]
randomLinearLinks _ 0 = []
randomLinearLinks seed n = Link ((x, 0), v) (Chem valence 0) : randomLinearLinks newSeed (n-1)
  where
    (valence, vSeed) = randomR (1, 3) seed :: (Int, StdGen)
    x = fromIntegral n * 18.0
    (v, newSeed) = randomV vSeed 50


twoLinkModel :: Model
twoLinkModel = Model 250
  [ Link ((0, 0), (30, 10))      (Chem 1 0)
  , Link ((1000, 30), (-50, 10)) (Chem 1 0)
  ]
threeLinkModel :: Model
threeLinkModel = Model 250
  [ Link ((0, -100), (60, 20))     (Chem 1 0)
  , Link ((1000, -150), (-50, 10)) (Chem 1 0)
  , Link ((500, 1000), (0, -30))   (Chem 1 0)
  ]
fiveLinks = Model 200
  [ Link ((0,0), (0,0))         (Chem 1 0)
  , Link ((400,400), (-20,-22)) (Chem 1 0)
  , Link ((-400,500), (30,-28)) (Chem 1 0)
  , Link ((-400,-380), (40,40)) (Chem 1 0)
  , Link ((400,-400), (-24,24)) (Chem 1 0)
  ]

update :: ViewPort -> Duration -> Model -> Model
update vp = step

draw :: Model -> Picture
draw (Model rad links) = Pictures $ bodies ++ centres
  where
    (bodies, centres) = unzip $ fmap (drawLink rad) links

drawLink :: Radius -> Link -> (Picture, Picture)
drawLink rad (Link ((x, y), _) chem) = (translate x y (body bodyColor rad), translate x y (innerPoint centerColor))
  where
    (bodyColor, centerColor) = chemColors chem

body :: Color -> Radius -> Picture
body color rad = Color color $ circleSolid rad
innerPoint :: Color -> Picture
innerPoint color = Color color $ circleSolid 1

chemColors :: Chem -> (Color, Color)
chemColors chem = case desire chem of
  EQ -> (greyN 0.5, white)
  GT -> (red, white)
  LT -> (green, white)