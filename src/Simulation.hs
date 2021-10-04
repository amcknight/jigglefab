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
import Chem.Valence
import Debug.Trace
import qualified Color as C
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import View
import Geometry.Vector
import Struct
import Orb
import Chem.Sem
import Geometry.Angle
import Data.Fixed (mod')
import Geometry.Tiling
import Data.List (sortBy)

run :: IO ()
run = runSeeded =<< getStdGen

runSeeded :: StdGen -> IO ()
runSeeded seed = do
  let struct = twoBallInner
  let (model, _) = runState (buildModel 3 struct) seed
  let view = View (Left struct) zeroV 250
  let frameRate = 30
  play
    FullScreen
    (greyN 0.2)
    frameRate
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

-- draw v = Pictures $ zipWith Color colors arcs <> zipWith Color colors tris
--   where
--     rad = 400
--     p = (0, 100)
--     q = (0,-100)
--     r = (50,  0)
--     squashPQ = squashTurn rad p q
--     squashPR = squashTurn rad p r
--     squashQR = squashTurn rad q r
--     dirPQ = direction (q |- p)
--     dirPR = direction (r |- p)
--     dirQR = direction (r |- q)
--     turnPQ1 = dirPQ + squashPQ
--     turnPQ2 = dirPQ - squashPQ
--     turnQP1 = pole dirPQ + squashPQ
--     turnQP2 = pole dirPQ - squashPQ
--     turnPR1 = dirPR + squashPQ
--     turnPR2 = dirPR - squashPQ
--     turnRP1 = pole dirPR + squashPR
--     turnRP2 = pole dirPR - squashPR
--     turnQR1 = dirPR + squashPR
--     turnQR2 = dirPR - squashPR
--     turnRQ1 = pole dirQR + squashQR
--     turnRQ2 = pole dirQR - squashQR
--     radianPQ1 = toRadian turnPQ1
--     radianPQ2 = toRadian turnPQ2
--     radianQP1 = toRadian turnQP1
--     radianQP2 = toRadian turnQP2
--     radianPR1 = toRadian turnPR1
--     radianPR2 = toRadian turnPR2
--     radianRP1 = toRadian turnRP1
--     radianRP2 = toRadian turnRP2
--     radianQR1 = toRadian turnQR1
--     radianQR2 = toRadian turnQR2
--     radianRQ1 = toRadian turnRQ1
--     radianRQ2 = toRadian turnRQ2
--     posPQ1 = (rad |* toUnit radianPQ1) |+ p
--     posPQ2 = (rad |* toUnit radianPQ2) |+ p
--     posQP1 = (rad |* toUnit radianQP1) |+ q
--     posQP2 = (rad |* toUnit radianQP2) |+ q
--     posPR1 = (rad |* toUnit radianPR1) |+ p
--     posPR2 = (rad |* toUnit radianPR2) |+ p
--     posRP1 = (rad |* toUnit radianRP1) |+ r
--     posRP2 = (rad |* toUnit radianRP2) |+ r
--     posQR1 = (rad |* toUnit radianQR1) |+ q
--     posQR2 = (rad |* toUnit radianQR2) |+ q
--     posRQ1 = (rad |* toUnit radianRQ1) |+ r
--     posRQ2 = (rad |* toUnit radianRQ2) |+ r
--     colors = [red, green, blue]
--     [pa, pt] = drawBlob rad [(simple turnPQ1, simple turnPQ2)]
--     [qa, qt] = drawBlob rad [(simple turnQP1, simple turnQP2)]
--     arcs = [uncurry translate p pa, uncurry translate q qa]
--     tris = [uncurry translate p pt, uncurry translate q qt]

-- drawBlob :: Radius -> [(Turn,Turn)] -> [Picture]
-- drawBlob rad fromTos = [
--   case compare from to of
--     GT -> arcSolid (degrees from) 360 rad <> arcSolid 0 (degrees to) rad
--     _ -> arcSolid (degrees from) (degrees to) rad
--   , polygon [zeroV, rad |* toUnit (toRadian from), rad |* toUnit (toRadian to)]
--   ]

event :: Event -> View c -> View c
event e v = case e of
  EventKey (MouseButton LeftButton) Down _ pos -> v
  EventKey {} -> v
  EventMotion pos -> v
  EventResize _ -> v

update :: Chem c => Duration -> View c -> View c
update dt v = v { structOrModel = case m of
   Left s -> Left s
   Right s -> Right $ step dt s }
  where m = structOrModel v

drawStruct :: Chem c => Struct c -> Picture
drawStruct (Struct ws os) = Pictures (fmap (drawWall yellow) ws) <> drawVoronoi (voronoi ps)
  where
    -- Sorted High Y to Low Y
    ps = sortBy (\((_,y1),_) ((_,y2),_) -> compare y2 y1) $ fmap (\(Orb p c) -> (p, chemColor c)) os

drawVoronoi :: Voronoi -> Picture
drawVoronoi v = Pictures $ fmap drawArc (arcs v) <> fmap drawTri (tris v)

drawArc :: Arc -> Picture
drawArc (Geometry.Tiling.Arc p from to c) = uncurry translate p $ Color (C.toGlossColor c) $ circleSolid 1

drawTri :: Tri -> Picture
drawTri (Tri (a,b,c) i) = polygon [a, b, c]

-- drawBlob :: Radius -> [(Turn,Turn)] -> [Picture]
-- drawBlob rad fromTos = [
--   case compare from to of
--     GT -> arcSolid (degrees from) 360 rad <> arcSolid 0 (degrees to) rad
--     _ -> arcSolid (degrees from) (degrees to) rad
--   , polygon [zeroV, rad |* toUnit (toRadian from), rad |* toUnit (toRadian to)]
--   ]

drawModel :: Chem c => Model c -> Picture
drawModel m = drawForm (form m) <> Pictures (fmap (drawBond (form m)) (innerIps m))

drawForm :: Chem c => Form c -> Picture
drawForm f = Pictures $ ws ++ bs
  where
    ws = toList $ fmap (drawWall yellow) (walls f)
    bs = fmap drawBall (toList (balls f))

drawBall :: Chem c => Ball c -> Picture
drawBall (Ball (Point p _) c) = drawOrb $ Orb p c

drawOrb :: Chem c => Orb c -> Picture
drawOrb (Orb (x,y) chem) = translate x y $ drawCircle (C.toGlossColor (chemColor chem)) 1

drawWall :: Color -> Wall -> Picture
drawWall color (HLine y) = Color color $ line [(-1000000, y), (1000000, y)]
drawWall color (VLine x) = Color color $ line [(x, -1000000), (x, 1000000)]
drawWall color (Wall.Circle (x,y) rad) = Color color $ translate x y $ circle rad

drawBond :: Form c -> P Int -> Picture
drawBond f ip = Color white $ line [p1, p2]
  where (p1, p2) = pmap (pos . point . ballI f) ip

drawCircle :: Color -> Radius -> Picture
drawCircle color = Color color . circleSolid
