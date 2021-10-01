module Simulation
( run
) where

import Graphics.Gloss.Data.ViewPort (ViewPort)
import Data.Vector (toList)
import System.Random (getStdGen, StdGen)
import Geometry.Space
import Time
import Point
import Ball
import Pair
import Model
import Wall
import Form
import Control.Monad.State
import Chem
import Debug.Trace
import Geometry.Angle
import qualified Color as C
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import StructLibrary
import View
import Geometry.Vector
import Struct
import Orb
import Chem.Sem

run :: IO ()
run = runSeeded =<< getStdGen

runSeeded :: StdGen -> IO ()
runSeeded seed = do
  let struct = movingTool
  let (model, _) = runState (buildModel 3 struct) seed
  let view = View (Right model) zeroV 50
  trace (show seed) play
    FullScreen
    (greyN 0.2)
    30
    view
    draw
    event
    update

draw :: Chem c => View c -> Picture
draw v = translate x y $ scale z z $ case sm of
  Left s -> drawStruct s
  Right m -> drawModel m
  where
    sm = structOrModel v
    z = zoom v
    (x, y) = center v

event :: Event -> View Sem -> View Sem
event e v = trace (show e) $ case e of
  EventKey (MouseButton LeftButton) Down _ pos -> v
  EventKey {} -> v
  EventMotion pos -> v
  EventResize _ -> v

update :: Duration -> View Sem -> View Sem
update dt v = v { structOrModel = case m of
   Left s -> Left s
   Right s -> Right $ step dt s }
  where m = structOrModel v

drawStruct :: Chem c => Struct c -> Picture
drawStruct (Struct ws os) = Pictures $ fmap (drawWall yellow) ws ++ fmap drawOrb os

drawModel :: Chem c => Model c -> Picture
drawModel m = Pictures (drawForm (form m)) <> Pictures (fmap (drawBond (form m)) (innerIps m))

drawForm :: Chem c => Form c -> [Picture]
drawForm f = ws ++ bs
  where
    ws = toList $ fmap (drawWall yellow) (walls f)
    bs = fmap drawBall (toList (balls f))

drawBall :: Chem c => Ball c -> Picture
drawBall (Ball (Point p _) c) = drawOrb $ Orb p c

drawOrb :: Chem c => Orb c -> Picture
drawOrb (Orb (x,y) chem) = translate x y $ body (C.toGlossColor (chemColor chem)) 1

drawWall :: Color -> Wall -> Picture
drawWall color (HLine y) = Color color $ line [(-1000000, y), (1000000, y)]
drawWall color (VLine x) = Color color $ line [(x, -1000000), (x, 1000000)]
drawWall color (Wall.Circle (x,y) rad) = Color color $ translate x y $ circle rad

drawBond :: Form c -> P Int -> Picture
drawBond f ip = Color white $ line [p1, p2]
  where (p1, p2) = pmap (pos . point . ballI f) ip

body :: Color -> Radius -> Picture
body color rad = Color color $ circleSolid rad
