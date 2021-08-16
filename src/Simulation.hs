module Simulation
( run
) where

import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Data.Vector (toList)
import System.Random (getStdGen)
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

pallet = nicePallet

run :: IO ()
run = do
  seed <- getStdGen
  let (model, _) = runState (genModel 20) seed
  simulate
    FullScreen
    (greyN 0.2)
    30
    (innerBumpModel 200 Active Creator)--model
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
drawWall color (Wall Horizontal f) = Color color $ line [(-3000, f), (3000, f)]
drawWall color (Wall Vertical   f) = Color color $ line [(f, -3000), (f, 3000)]

drawBond :: Model c -> P Int -> Picture
drawBond m ip = Color white $ line [p1, p2]
  where (p1, p2) = pmap (pos . point . ballByI (form m)) ip

body :: Color -> Radius -> Picture
body color rad = Color color $ circleSolid rad

innerPoint :: Picture
innerPoint = Color white $ circleSolid 1
