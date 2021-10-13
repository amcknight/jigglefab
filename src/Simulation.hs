module Simulation
( run
) where

import Graphics.Gloss.Data.ViewPort (ViewPort)
import Data.Vector (toList)
import System.Random (getStdGen, StdGen)
import Data.List (sortBy)
import Data.Fixed (mod')
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
import Geometry.Tiling
import Geometry.Voronoi
import qualified Data.Vector as V
import Geometry.Line

run :: IO ()
run = runSeeded =<< getStdGen

runSeeded :: StdGen -> IO ()
runSeeded seed = do
  let struct = threeBallInner
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
drawStruct (Struct ws os) = Pictures $
  fmap (drawWall yellow) ws <>
  -- fmap drawEdge (voronoi ps) <>
  fmap drawOrb os <>
  -- fmap (drawWedge vos) (tileVoronoi vos (voronoi ps))
  [drawBeach (processBeach (initialBeach (fmap orbPos os)) 3)]
  where
    ps = fmap orbPos os
    vos = V.fromList os

drawEdge :: Edge -> Picture
drawEdge (Edge (Seg p q) _) = Color white $ line [p, q]

drawBeach :: Beach -> Picture
drawBeach (Beach sw es bs rs) = Pictures $ fmap drawEvent es <> fmap (drawBouy sw) bs <> fmap drawRay rs <> drawSweep sw

drawEvent :: Geometry.Voronoi.Event -> Picture 
drawEvent (BouyEvent (pos, i)) = drawPosAt pos cyan
drawEvent (CrossEvent (Cross pos rad i)) = Color (greyN 0.5) $ uncurry translate pos $ circle rad

drawBouy :: Float -> Bouy -> Picture
drawBouy sweep (pos@(x,y), i) = drawPosAt pos green <> Color green (uncurry translate (x,sweep) (drawParabola y sweep))

drawParabola :: Float -> Float -> Picture
drawParabola focalH sweep = line $ fmap (\xi -> (xi, parabY (focalH-sweep) xi)) [-100,-99.99..100]
  where parabY h x = (x^2 + h^2)/(2*h)

drawRay :: Ray -> Picture
drawRay (Ray pos dir i j) = drawPosAt pos yellow

drawSweep :: Float -> [Picture]
drawSweep h = [Color red $ line [(-100000, h), (100000, h)]]

drawPosAt :: Position -> Color -> Picture
drawPosAt pos c = Color c $ uncurry translate pos $ circleSolid 0.02

drawWedge :: Chem c => V.Vector (Orb c) -> Wedge -> Picture
drawWedge os (Pie p from to c) = blank --uncurry translate p $ Color (C.toGlossColor c) (arcSolid 1 from to)
drawWedge os (Tri p q r c) = Color (C.toGlossColor c) (polygon [p, q, r])

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
drawOrb (Orb (x,y) chem) = translate x y $ drawCircle (C.toGlossColor (chemColor chem)) 0.01 <> Color (C.toGlossColor (chemColor chem)) (circle 1)

drawWall :: Color -> Wall -> Picture
drawWall color (HLine y) = Color color $ line [(-1000000, y), (1000000, y)]
drawWall color (VLine x) = Color color $ line [(x, -1000000), (x, 1000000)]
drawWall color (Wall.Circle (x,y) rad) = Color color $ translate x y $ circle rad

drawBond :: Form c -> P Int -> Picture
drawBond f ip = Color white $ line [p1, p2]
  where (p1, p2) = pmap (pos . point . ballI f) ip

drawCircle :: Color -> Radius -> Picture
drawCircle color = Color color . circleSolid
