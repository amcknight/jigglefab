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
import Chem
import Point
import Ball
import Balls
import Pair
import Model
import ModelLibrary
import Wall

run :: IO ()
run = do
  seed <- getStdGen
  simulate
    FullScreen
    black
    30
    (chainModel seed 20 (V (-500) (-200)) (V 500 500))
    draw
    update

draw :: Model -> Picture
draw m = Pictures $ ws ++ bodies ++ centres ++ bonds
  where
    (bodies, centres) = unzip $ fmap (drawBall (rad m)) (toList (balls m))
    ws = toList $ fmap drawWall (walls m)
    bonds = fmap (drawBond m) (innerIps m)

update :: ViewPort -> Duration -> Model -> Model
update vp = step

drawBall :: Radius -> Ball -> (Picture, Picture)
drawBall rad (Ball (Point (V x y) _) chem) = bimap (translate x y) (body bodyColor rad, innerPoint centerColor)
  where (bodyColor, centerColor) = chemColors chem

drawWall :: Wall -> Picture 
drawWall (Wall Horizontal f) = Color yellow $ line [(-3000, f), (3000, f)]
drawWall (Wall Vertical f) = Color yellow $ line [(f, -3000), (f, 3000)]

drawBond :: Model -> IP -> Picture
drawBond m ip = Color white $ line [p1, p2]
  where
    (p1, p2) = bimap (coords . pos) $ points $ ballsByI m ip

body :: Color -> Radius -> Picture
body color rad = Color color $ circleSolid rad

innerPoint :: Color -> Picture
innerPoint color = Color color $ circleSolid 1

chemColors :: Chem -> (Color, Color)
chemColors chem = case desire chem of
  EQ -> (greyN 0.5, white)
  GT -> (red, white)
  LT -> (green, white)
