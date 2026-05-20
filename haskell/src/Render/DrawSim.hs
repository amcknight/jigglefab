module Render.DrawSim
( draw
) where

import Data.Vector (toList)
import qualified Data.Vector as V
import qualified Color as C
import Geometry.Circle
import Motion.Point
import Motion.Ball
import Util.Pair
import Motion.Model
import State.Wall
import Motion.Form
import State.Chem
import Graphics.Gloss
import State.Struct
import State.Orb
import Voronoi.Tiling
import Voronoi.Fortune
import Geometry.Line
import Geometry.Angle
import Voronoi.Pie
import Voronoi.Tri
import Voronoi.Sweep
import Render.Draw
import Pane.View
import Pane.Frame
import Util.Enumer
import Render.DrawDebug
import Geometry.Bound

draw :: forall c . (Chem c, Enumer c) => View c -> Picture
draw v = Pictures $ case mode v of
  Add ->    [dVoronoi, dStruct, dStatusBar, dSideBar]
  Delete -> [dVoronoi, dStruct, dStatusBar, dOrbHover]
  Edit ->   [dVoronoi, dStruct, dStatusBar, dOrbHover, dSideBar]
  Move ->   [dVoronoi, dStruct, dStatusBar, dOrbHover]
  Run -> [dModel]
  where
    f = frame v
    s = struct v
    dStruct = toFrame f $ drawStruct s
    dModel =  toFrame f $ drawModel $ model v
    dStatusBar = drawStatusBar v
    dSideBar = drawSidebar (vals @c) (tip v) (menuHover v)
    dOrbHover = drawOrbHover f (orbHover v) s
    dVoronoi = toFrame f $ drawVoronoi v

drawVoronoi :: View c -> Picture
drawVoronoi v = Pictures $ drawVoronoiEdges $ voronoi $ fmap pos os
  -- <> [drawBeach f os 4]
  where os = viewOrbs v

drawStruct :: Chem c => Struct c -> Picture
drawStruct (Struct walls os) = Pictures $
  fmap (drawWall yellow) walls
  <> fmap drawOrb os
  <> fmap (drawOrbWedge (V.fromList os)) ws
  where
    ps = fmap pos os
    ws = tileVoronoi (V.fromList ps) (voronoi ps)

drawOrbWedge :: Chem c => V.Vector (Orb c) -> Wedge -> Picture
drawOrbWedge os (PieWedge i p) = Color (colorFromOrbI os i) $ drawPie p
drawOrbWedge os (TriWedge i t) = Color (colorFromOrbI os i) $ drawTri t

drawBallWedge :: Chem c => V.Vector (Ball c) -> Wedge -> Picture
drawBallWedge bs (PieWedge i p) = Color (colorFromBallI bs i) $ drawPie p
drawBallWedge bs (TriWedge i t) = Color (colorFromBallI bs i) $ drawTri t

drawPie :: Pie -> Picture
drawPie (Pie o (Sweep from to)) = drawArcAt o from to
drawPie (Pie o FullSweep) = drawArcAt o 0 1

drawTri :: Tri -> Picture
drawTri (Tri o (Seg p q)) = toPolygon [o, p, q]

colorFromOrbI :: Chem c => V.Vector (Orb c) -> Int -> Color
colorFromOrbI os i = C.toGlossColor $ chemColor $ orbChem $ os V.! i

colorFromBallI :: Chem c => V.Vector (Ball c) -> Int -> Color
colorFromBallI bs i = C.toGlossColor $ chemColor $ chem $ bs V.! i

drawModel :: Chem c => Model c -> Picture
drawModel m = drawForm f <> Pictures (fmap (drawBond f) (innerIps m))
  where f = form m

drawForm :: Chem c => Form c -> Picture
drawForm (Form ws bs) = Pictures $ wPics ++ bPics
  where
    wPics = toList $ fmap (drawWall yellow) ws
    es = voronoi $ toList $ fmap pos bs
    bPics = drawBallWedge bs <$> tileVoronoi (fmap pos bs) es

drawOrb :: Chem c => Orb c -> Picture
drawOrb (Orb p chem) = toTranslate p . Color c $ toCircleSolid 1 <> circle 1
  where c = C.toGlossColor $ chemColor chem

drawWall :: Color -> Wall -> Picture
drawWall color (HLine y) = Color color $ toLine [(-10000, y), (10000, y)]
drawWall color (VLine x) = Color color $ toLine [(x, -10000), (x, 10000)]
drawWall color (Rock (Geometry.Circle.Circle p rad)) = Color color $ toTranslate p $ toCircle rad

drawBond :: Form c -> P Int -> Picture
drawBond f ip = Color white $ toLine [p1, p2]
  where (p1, p2) = pmap (pos . point . ballI f) ip

drawArcAt :: Position -> Turn -> Turn -> Picture
drawArcAt p from to = toTranslate p $ case compare f t of
   LT -> toArcSolid f t 1
   EQ -> error "Exactly equal from to shouldn't happen?"
   GT -> toArcSolid f 360 1 <> toArcSolid 0 t 1
   where
     f = degrees from
     t = degrees to

drawOrbHover :: Frame -> Maybe (Orb c) -> Struct c -> Picture
drawOrbHover _ Nothing _ = blank
drawOrbHover f (Just (Orb p _)) _ = toFrame f $ toTranslate p $ toCircleSolid $ 20 / zoom f

drawStatusBar :: View c -> Picture
drawStatusBar v = toTranslate scrPos $ scale textScale textScale $ color green $ text $ "Mode: " ++ show (mode v)
  where
    textScale = 0.3
    scrPos = pmap (realToFrac . (+ menuBlockSize)) (maxX b, maxY b)
    b = screen v

-- TODO: Remove magic numbers. Merge menu and screen values into single type for passing around?
drawSidebar :: Chem c => [c] -> c -> Maybe c -> Picture
drawSidebar chs sel hov = translate (-1850) 1000 $ Pictures pics
  where
    pics = spreadSelections $ zipWith drawTokenSelector chs flairs
    flairs = map (buildFlair hov sel) chs
    spreadSelections :: [Picture] -> [Picture]
    spreadSelections ps = zipWith (\i p -> translate 0 (-40*fromIntegral i) p) [0..length chs] ps

buildFlair :: Chem c => Maybe c -> c -> c -> Picture
buildFlair hov sel c
  | sel == c = color white pointer
  | hov == Just c = color black pointer
  | otherwise = blank
  where pointer = toRectSolid 20 20

drawTokenSelector :: Chem c => c -> Picture -> Picture
drawTokenSelector ch flair = Pictures
  [ translate 0 0 flair
  , translate 40 0 $ tokenColor ch $ toCircleSolid 20
  , translate 100 (-10) $ scale 0.2 0.2 $ color white $ text $ show ch
  ]

tokenColor :: Chem c => c -> Picture -> Picture
tokenColor = color . C.toGlossColor . chemColor
