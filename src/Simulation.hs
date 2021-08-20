module Simulation
( run
) where

import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Data.Vector (toList)
import System.Random (getStdGen, StdGen)
import Space
import Time
import Point
import Ball
import Pair
import Model
import ModelLibrary
import Wall
import Form
import Control.Monad.State
import Chem
import Electro.ElectroModels
import Pallet
import Valence.Valence
import Core.CoreModels
import Valence.ValenceModels
import Core.Core
import Debug.Trace

pallet = nicePallet

run :: IO ()
run = runSeeded =<< getStdGen

runSeeded :: StdGen -> IO ()
runSeeded seed = do
  let (model, _) = runState (wireModel 50) seed
  trace (show seed) simulate
    FullScreen
    (greyN 0.2)
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
    ws = toList $ fmap (drawWall (getCold pallet)) (walls f) 

update :: Chem c => ViewPort -> Duration -> Model c -> Model c
update vp = step

drawBall :: Chem c => Radius -> Ball c -> P Picture
drawBall rad (Ball (Point (x,y) _) chem) = pmap (translate x y) (body (chemColor chem pallet) rad, innerPoint)

drawWall :: Color -> Wall -> Picture
drawWall color (HLine y) = Color color $ line [(-3000, y), (3000, y)]
drawWall color (VLine x) = Color color $ line [(x, -3000), (x, 3000)]
drawWall color (Wall.Circle (x,y) rad) = Color color $ translate x y $ circle rad

drawBond :: Model c -> P Int -> Picture
drawBond m ip = Color white $ line [p1, p2]
  where (p1, p2) = pmap (pos . point . ballI (form m)) ip

body :: Color -> Radius -> Picture
body color rad = Color color $ circleSolid rad

innerPoint :: Picture
innerPoint = Color white $ circleSolid 1
