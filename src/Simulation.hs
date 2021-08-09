module Simulation
( run
) where

import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Data.Vector (toList)
import System.Random (getStdGen)
import Space
import Time
import Vector
import Point
import Ball
import Balls
import Pair
import Model
import ModelLibrary
import Wall
import Form
import Control.Monad.State
import Chem
import Electro.Electro

run :: IO ()
run = do
  seed <- getStdGen
  let (model, _) = runState (wireModel 30 100 (V (-500) 500) (V 500 (-500)) Active) seed  --(chainModel seed 20 (V (-500) (-200)) (V 500 500))
  simulate
    FullScreen
    black
    30
    model
    draw
    update

draw :: Chem c => Model c -> Picture
draw m = Pictures $ drawForm (rad m) (form m) ++ fmap (drawBond m) (innerIps m)

drawForm :: Chem c => Radius -> Form c -> [Picture]
drawForm rad f = ws ++ bodies ++ centres
  where
    (bodies, centres) = unzip $ fmap (drawBall rad) (toList (balls f))
    ws = toList $ fmap drawWall (walls f)

update :: Chem c => ViewPort -> Duration -> Model c -> Model c
update vp = step

drawBall :: Chem c => Radius -> Ball c -> (Picture, Picture)
drawBall rad (Ball (Point (V x y) _) chem) = bimap (translate x y) (body (chemColor chem) rad, innerPoint)

drawWall :: Wall -> Picture 
drawWall (Wall Horizontal f) = Color yellow $ line [(-3000, f), (3000, f)]
drawWall (Wall Vertical f) = Color yellow $ line [(f, -3000), (f, 3000)]

drawBond :: Model c -> IP -> Picture
drawBond m ip = Color white $ line [p1, p2]
  where
    (p1, p2) = bimap (coords . pos) $ points $ ballsByI (form m) ip

body :: Color -> Radius -> Picture
body color rad = Color color $ circleSolid rad

innerPoint :: Picture
innerPoint = Color white $ circleSolid 1
