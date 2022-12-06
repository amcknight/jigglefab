module DrawDebug
( drawBeach
, drawVoronoiEdges
) where

import Graphics.Gloss
import Chem
import Model.Orb
import Voronoi.Beach
import Voronoi.Event
import Pair
import Geometry.Vector
import Geometry.Circle
import qualified Data.Vector as V
import qualified Color as C
import Voronoi.Edge
import Debug.Trace (trace)
import Draw
import Geometry.Parabola
import Geometry.Bound
import Pane.Frame
import Geometry.Line

drawBeach :: Chem c => Frame -> [Orb c] -> Int -> Picture
drawBeach f os i = drawBeach' (zoom f) (V.fromList os) (processBeach (initialBeach (fmap pos os)) i)

drawBeach' :: Chem c => Double -> V.Vector (Orb c) -> Beach -> Picture
drawBeach' z os (Beach sw _ es bs rs) = Pictures $
  fmap (drawEvent z) es <>
  zipWith (drawBouy z os sw) xBounds (V.toList bs) <>
  fmap drawEdge (edgesFromRays b rs) <>
  drawSweep sw
  where
    ops = V.toList $ fmap pos os
    bps = V.toList $ fmap pos bs
    b = bufferedBound ops 1
    pcs = filter (\x -> x > minX b && x < maxX b) $ parabolaCrossXs sw bps
    xBounds = zip (minX b : pcs) (pcs ++ [maxX b])

drawEvent :: Double -> Voronoi.Event.Event -> Picture
drawEvent z (BouyEvent b) = drawPosAt z (pos b) cyan
drawEvent z (CrossEvent (Cross (Geometry.Circle.Circle pos rad) _)) =
  Color g (toTranslate pos (toCircle rad)) <> drawPosAt z pos g
  where g = greyN 0.5

drawBouy :: Chem c => Double -> V.Vector (Orb c) -> Double -> P Double -> Bouy -> Picture
drawBouy z os sweep xBound (Bouy pos i) = drawPosAt z pos c <> Color c p
  where
    p = drawParabola pos sweep xBound
    c = C.toGlossColor $ chemColor $ orbChem $ os V.! i

drawParabola :: Position -> Double -> P Double -> Picture
drawParabola focal sweep xBound = case parabolaFromFocus sweep focal of
  Nothing -> trace "Warning: blank parabola" blank
  Just p -> toLine $ parabPoss p xBound 0.01 -- TODO: Should be sensitive to zoom to prevent jagged parabola lines

drawSweep :: Double -> [Picture]
drawSweep h = [Color black $ toLine [(-100000, h), (100000, h)]]

drawPosAt :: Double -> Position -> Color -> Picture
drawPosAt z pos c = Color c $ toTranslate pos $ toCircleSolid $ 5/z

drawVoronoiEdges :: [Edge] -> [Picture]
drawVoronoiEdges = fmap drawEdge

drawEdge :: Edge -> Picture
drawEdge = Color white . drawSeg . seg

drawSeg :: Seg -> Picture
drawSeg (Seg p q) = toLine [p, q]
