module Simulation
( run
) where

import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Data.Maybe (fromMaybe)
import Data.Vector (toList)
import Data.Map (keys)
import Space
import Time
import Chem
import Point
import Link
import Links
import Pair
import Model
import ModelLibrary
import System.Random ( getStdGen )

run :: IO ()
run = do
  seed <- getStdGen
  simulate
    FullScreen
    black
    30
    (randomLinearModel seed 300)
    draw
    update

draw :: Model -> Picture
draw m = Pictures $ bodies ++ centres ++ bonds
  where
    (bodies, centres) = unzip $ fmap (drawLink (rad m)) (toList (links m))
    bonds = fmap (drawBond m) (innerIps m)

update :: ViewPort -> Duration -> Model -> Model
update vp = step

drawLink :: Radius -> Link -> (Picture, Picture)
drawLink rad (Link ((x, y), _) chem) = bimap (translate x y) (body bodyColor rad, innerPoint centerColor)
  where
    (bodyColor, centerColor) = chemColors chem

drawBond :: Model -> IP -> Picture
drawBond m ip = Color white $ line [p1, p2]
  where
    (p1, p2) = bimap pos $ points $ linksByI m ip

body :: Color -> Radius -> Picture
body color rad = Color color $ circleSolid rad

innerPoint :: Color -> Picture
innerPoint color = Color color $ circleSolid 1

chemColors :: Chem -> (Color, Color)
chemColors chem = case desire chem of
  EQ -> (greyN 0.5, white)
  GT -> (red, white)
  LT -> (green, white)
