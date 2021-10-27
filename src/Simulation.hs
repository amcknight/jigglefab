{-# LANGUAGE LambdaCase #-}
module Simulation
( run
) where

import Graphics.Gloss.Data.ViewPort (ViewPort)
import Data.Vector (toList)
import System.Random (getStdGen, StdGen)
import Data.List (sortBy)
import Data.Fixed (mod')
import qualified Data.Vector as V
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
import Geometry.Tiling
import Geometry.Voronoi
import Geometry.Line
import Geometry.Parabola
import Geometry.CrossPoint
import Geometry.Beach
import Geometry.Bound
import Geometry.Angle
import Geometry.Pie
import Geometry.Tri
import Geometry.Sweep
import Chem.Buckle

run :: IO ()
run = runSeeded =<< getStdGen

runSeeded :: StdGen -> IO ()
runSeeded seed = do
  let struct = threeBallInner   
  let (model, _) = runState (buildModel 3 struct) seed
  let view = View (Left struct) zeroV 200
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

event :: Graphics.Gloss.Interface.IO.Interact.Event -> View c -> View c
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
drawStruct (Struct walls os) = Pictures $
  fmap (drawWall yellow) walls <>
  fmap drawEdge es <>
  fmap drawOrb os <>
  fmap (drawWedge vos) ws
  -- [drawBeach vos (processBeach (initialBeach ps) 2)]
  where
    es = voronoi ps
    ws = tileVoronoi vos es
    ps = fmap orbPos os
    vos = V.fromList os

drawEdge :: Edge -> Picture
drawEdge (Edge (Seg p q) _) = Color white $ line [p, q]

drawBeach :: Chem c => V.Vector (Orb c) -> Beach -> Picture
drawBeach os (Beach sw _ es bs rs) = Pictures $
  fmap drawEvent es <>
  zipWith (drawBouy os sw) xBounds (V.toList bs) <>
  fmap drawEdge (edgesFromRays b rs) <>
  drawSweep sw
  where
    ops = V.toList $ fmap orbPos os
    bps = V.toList $ fmap bouyPos bs
    b = bufferedBound ops 1
    pcs = parabolaCrossXs sw bps
    xBounds = zip (minX b : pcs) (pcs ++ [maxX b])

drawEvent :: Geometry.Beach.Event -> Picture 
drawEvent (BouyEvent b) = drawPosAt (bouyPos b) cyan
drawEvent (CrossEvent (Cross pos rad i)) = Color g (uncurry translate pos (circle rad)) <> drawPosAt pos g
  where g = greyN 0.5

drawBouy :: Chem c => V.Vector (Orb c) -> Float -> (Float, Float) -> Bouy -> Picture
drawBouy cs sweep xBound (Bouy pos@(x,y) i) = drawPosAt pos c <> Color c p
  where
    p = drawParabola pos sweep xBound
    c = C.toGlossColor $ chemColor $ orbChem $ cs V.! i

drawParabola :: Position -> Float -> (Float, Float) -> Picture
drawParabola focal sweep (mnX,mxX) = case parabolaFromFocus sweep focal of
  Nothing -> blank
  Just p -> line $ fmap (atX p) [mnX,(mnX+0.01)..mxX]

drawParabCrosses :: Float -> V.Vector Bouy -> [Picture]
drawParabCrosses sw bs =
  fmap drawCrossPoints (zipWith (crossPointsFromFoci sw) ps (tail ps)) <> 
  fmap drawLiveCrossPoint (zipWith (parabolaCross sw) ps (tail ps))
  where ps = V.toList $ fmap bouyPos bs

drawCrossPoints :: CrossPoints -> Picture
drawCrossPoints (OneCross p) = drawPosAt p blue
drawCrossPoints (TwoCross p q) = drawPosAt p blue <> drawPosAt p blue
drawCrossPoints NoCross = error "Drawing non-points"
drawCrossPoints AllCross = error "Drawing infinite-points"

drawLiveCrossPoint :: Position -> Picture
drawLiveCrossPoint p = drawPosAt p magenta

drawSweep :: Float -> [Picture]
drawSweep h = [Color black $ line [(-100000, h), (100000, h)]]

drawPosAt :: Position -> Color -> Picture
drawPosAt pos c = Color c $ uncurry translate pos $ circleSolid 0.05

drawWedge :: Chem c => V.Vector (Orb c) -> Wedge -> Picture
drawWedge os (PieWedge i p) = Color (colorFromOrbI os i) $ drawPie p
drawWedge os (TriWedge i t) = Color (colorFromOrbI os i) $ drawTri t

drawPie :: Pie -> Picture
drawPie (Pie o (Sweep from to)) = drawArcAt o from to
drawPie (Pie o FullSweep) = drawArcAt o 0 1

drawTri :: Tri -> Picture
drawTri (Tri o (Seg p q)) = polygon [o, p, q]

colorFromOrbI :: Chem c => V.Vector (Orb c) -> Int -> Color
colorFromOrbI os i = C.toGlossColor $ chemColor $ orbChem $ os V.! i

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
drawOrb (Orb (x,y) chem) = translate x y $ drawCircle c 0.01 <> Color c (circle 1)
  where c = C.toGlossColor $ chemColor chem

drawWall :: Color -> Wall -> Picture
drawWall color (HLine y) = Color color $ line [(-10000, y), (10000, y)]
drawWall color (VLine x) = Color color $ line [(x, -10000), (x, 10000)]
drawWall color (Wall.Circle (x,y) rad) = Color color $ translate x y $ circle rad

drawBond :: Form c -> P Int -> Picture
drawBond f ip = Color white $ line [p1, p2]
  where (p1, p2) = pmap (pos . point . ballI f) ip

drawArcAt :: Position -> Turn -> Turn -> Picture
drawArcAt p from to = uncurry translate p $ case compare f t of
   LT -> arcSolid f t 1
   EQ -> error "Exactly equal from to shouldn't happen?"
   GT -> arcSolid f 360 1 <> arcSolid 0 t 1
   where
     f = degrees from
     t = degrees to

drawCircle :: Color -> Radius -> Picture
drawCircle color = Color color . circleSolid
