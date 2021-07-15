module Simulation
( run
, twoBallEnv
, threeBallEnv
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
import Env

run :: IO ()
run = do
  seed <- getStdGen
  simulate
    FullScreen
    black
    60
    threeBallEnv -- (model (initialModel seed 300))
    draw
    update

initialEnv :: StdGen -> Int -> Env
initialEnv seed n = Env 20 $ randomModel seed 1000 n

randomModel :: StdGen -> Float -> Int -> Model
randomModel _ _ 0 = []
randomModel seed size num = Link (pos, vel) (Chem valence 0) : randomModel newSeed size (num-1)
  where
    (valence, pSeed) = randomR (1, 3) seed :: (Int, StdGen)
    (pos, vSeed) = randomVIn pSeed size
    (vel, newSeed) = randomV vSeed 50

randomLinearModel :: StdGen -> Int -> Model
randomLinearModel _ 0 = []
randomLinearModel seed n = Link ((x, 0), v) (Chem valence 0) : randomLinearModel newSeed (n-1)
  where
    (valence, vSeed) = randomR (1, 3) seed :: (Int, StdGen)
    x = fromIntegral n * 18.0
    (v, newSeed) = randomV vSeed 50


twoBallEnv :: Env
twoBallEnv = Env 250
  [ Link ((0, 0), (30, 10))      (Chem 1 0)
  , Link ((1000, 30), (-50, 10)) (Chem 1 0)
  ]
threeBallEnv :: Env
threeBallEnv = Env 250
  [ Link ((0, -100), (60, 20))     (Chem 1 0)
  , Link ((1000, -150), (-50, 10)) (Chem 1 0)
  , Link ((500, 1000), (0, -30))   (Chem 1 0)
  ]

draw :: Env -> Picture
draw (Env rad m) = Pictures $ bodies ++ centres
  where
    (bodies, centres) = unzip $ fmap (drawLink rad) m

update :: ViewPort -> Duration -> Env -> Env
update vp dt (Env rad m) = Env rad $ step dt rad m

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