module Simulation
( run
) where

import Graphics.Gloss.Data.ViewPort (ViewPort)
import Data.Vector (toList)
import System.Random (getStdGen, StdGen)
import Data.List (sortBy)
import Data.Fixed (mod')
import qualified Data.Vector as V
import qualified Color as C
import Geometry.Circle
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
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import View
import Geometry.Vector
import Struct
import Orb
import Chem.Sem
import Tiling
import Voronoi.Fortune
import Geometry.Line
import Geometry.Parabola
import Geometry.CrossPoint
import Voronoi.Beach
import Geometry.Bound
import Geometry.Angle
import Voronoi.Pie
import Voronoi.Tri
import Voronoi.Sweep
import Chem.Buckle
import Voronoi.Edge
import Voronoi.Event
import Chem.Load
import Draw
import Chem.Stripe
import Chem.Peano
import Chem.Encode
import Overlay
import DataType

run :: IO ()
run = runSeeded =<< getStdGen

zooom :: Double
zooom = 50
speeed :: Double
speeed = 10

metaChem :: Con
metaChem = encodeMetaChem

runSeeded :: StdGen -> IO ()
runSeeded seed = do
  let struct = turnbuckle
  let (model, nextSeed) = runState (buildModel speeed struct) seed
  -- let view = View (Left struct) zeroV zooom
  let view = View (Right model) nextSeed NoOverlay zeroV zooom
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
draw v = Pictures [toTranslate (pos v) $ toScale z z drawBoard, drawMenu]
  where
    drawBoard = case structOrModel v of
      Left s -> drawStruct s
      Right m -> drawModel m
    drawMenu = drawOverlay v metaChem
    z = zoom v

event :: Chem c => Graphics.Gloss.Interface.IO.Interact.Event -> View c -> View c
event e v = case e of
  EventKey (MouseButton LeftButton) Down _ pos -> v
  EventKey (MouseButton RightButton) Down _ pos -> setOverlayOn (pmap realToFrac pos) v
  EventKey (Char '=') Down _ _ -> zoomHop Out v
  EventKey (Char '-') Down _ _ -> zoomHop In v
  EventKey (SpecialKey KeySpace) Down _ _ -> togglePlay speeed v
  EventKey (SpecialKey KeyLeft) Down _ _ ->  panHop leftV v
  EventKey (SpecialKey KeyRight) Down _ _ -> panHop rightV v
  EventKey (SpecialKey KeyUp) Down _ _ ->    panHop upV v
  EventKey (SpecialKey KeyDown) Down _ _ ->  panHop downV v
  EventKey {} -> v
  EventMotion mpos -> v {overlay = updateOverlay metaChem (pmap realToFrac mpos) (overlay v)}
  EventResize _ -> v

update :: Chem c => Float -> View c -> View c
update dt v = v { structOrModel = case structOrModel v of
   Left s -> Left s
   Right s -> Right $ step (realToFrac dt) s }

drawStruct :: Chem c => Struct c -> Picture
drawStruct (Struct walls os) = Pictures $
  fmap (drawWall yellow) walls
  <> fmap drawOrb os
  <> fmap drawEdge es
  <> fmap (drawOrbWedge (V.fromList os)) ws
  -- <> [drawBeach (V.fromList os) (processBeach (initialBeach ps) 4)]
  where
    es = voronoi ps
    ps = fmap pos os
    ws = tileVoronoi (V.fromList ps) es

drawBeach :: Chem c => V.Vector (Orb c) -> Beach -> Picture
drawBeach os (Beach sw _ es bs rs) = Pictures $
  fmap drawEvent es <>
  zipWith (drawBouy os sw) xBounds (V.toList bs) <>
  fmap drawEdge (edgesFromRays b rs) <>
  drawSweep sw
  where
    ops = V.toList $ fmap pos os
    bps = V.toList $ fmap pos bs
    b = bufferedBound ops 1
    pcs = filter (\x -> x > minX b && x < maxX b) $ parabolaCrossXs sw bps
    xBounds = zip (minX b : pcs) (pcs ++ [maxX b])

drawEvent :: Voronoi.Event.Event -> Picture
drawEvent (BouyEvent b) = drawPosAt (pos b) cyan
drawEvent (CrossEvent (Cross (Geometry.Circle.Circle pos rad) i)) =
  Color g (toTranslate pos (toCircle rad)) <> drawPosAt pos g
  where g = greyN 0.5

drawBouy :: Chem c => V.Vector (Orb c) -> Double -> (Double, Double) -> Bouy -> Picture
drawBouy cs sweep xBound (Bouy pos@(x,y) i) = drawPosAt pos c <> Color c p
  where
    p = drawParabola pos sweep xBound
    c = C.toGlossColor $ chemColor $ orbChem $ cs V.! i

drawParabola :: Position -> Double -> (Double, Double) -> Picture
drawParabola focal sweep xBound = case parabolaFromFocus sweep focal of
  Nothing -> trace "Warning: blank parabola" blank
  Just p -> toLine $ parabPoss p xBound 0.01 -- TODO: Should be sensitive to zooom

drawParabCrosses :: Double -> V.Vector Bouy -> [Picture]
drawParabCrosses sw bs =
  fmap drawCrossPoints (zipWith (crossPointsFromFoci sw) ps (tail ps)) <> 
  fmap drawLiveCrossPoint (zipWith (parabolaCross sw) ps (tail ps))
  where ps = V.toList $ fmap pos bs

drawCrossPoints :: CrossPoints -> Picture
drawCrossPoints (OneCross p) = drawPosAt p blue
drawCrossPoints (TwoCross p q) = drawPosAt p blue <> drawPosAt p blue
drawCrossPoints NoCross = error "Drawing non-points"
drawCrossPoints InfinteCross = error "Drawing infinite-points"

drawLiveCrossPoint :: Position -> Picture
drawLiveCrossPoint p = drawPosAt p magenta

drawSweep :: Double -> [Picture]
drawSweep h = [Color black $ toLine [(-100000, h), (100000, h)]]

drawPosAt :: Position -> Color -> Picture
drawPosAt pos c = Color c $ toTranslate pos $ toCircleSolid $ 5/zooom

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
drawModel m = drawForm (form m) <> Pictures (fmap (drawBond (form m)) (innerIps m))

drawForm :: Chem c => Form c -> Picture
drawForm (Form ws bs) = Pictures $ wPics ++ bPics -- <> fmap drawEdge es
  where
    wPics = toList $ fmap (drawWall yellow) ws
    es = voronoi $ toList $ fmap pos bs
    bPics = drawBallWedge bs <$> tileVoronoi (fmap pos bs) es

drawBall :: Chem c => Ball c -> Picture
drawBall (Ball (Point p _) c) = drawOrb $ Orb p c

drawOrb :: Chem c => Orb c -> Picture
drawOrb (Orb p chem) = toTranslate p . Color c $ toCircleSolid (5/zooom) <> circle 1
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

drawEdge :: Edge -> Picture
drawEdge = Color white . drawSeg . seg

drawSeg :: Seg -> Picture
drawSeg (Seg p q) = toLine [p, q]

drawOverlay :: View c -> Con -> Picture
drawOverlay v c = case overlay v of
  NoOverlay -> blank
  Overlay pos tk -> toTranslate pos $ scale s s $ Pictures $ drawSuboverlay [] tk c (0,1)
  where s = realToFrac overlayThinkness

-- Try this as a Zipper?
drawSuboverlay :: Token -> Token -> Con -> P Turn -> [Picture]
drawSuboverlay preTk tk c r = ( case tk of
  [] -> []
  (name:postTk) -> case con c name of
    Nothing -> error "Can't draw Overlay where Token not matching the Con"
    Just (nextI, nextC) -> drawSuboverlay (preTk ++ [name]) postTk nextC (rs!!nextI)
  ) ++ zipWith drawSlice subTks rs
  where
    subTks = fmap (\n -> preTk ++ [n]) names
    rs = ranges r $ length names
    names = conNames c

ranges :: P Turn -> Int -> [P Turn]
ranges (d0,d1) n = zip turns $ tail turns
  where
    turns = fmap (\i -> d0 + width * fromIntegral i / fn) [0..n]
    step = width / fromIntegral n
    width = d1-d0
    fn = fromIntegral n

drawSlice :: Token -> P Turn -> Picture
drawSlice tk (f, t) = Pictures
  [ color (C.toGlossColor (metaChemColor tk)) (toArcSolid d0 d1 rad)
  , color black $ toSectorWire d0 d1 rad
  ]
  where
    rad = fromIntegral $ length tk
    d0 = degrees f
    d1 = degrees t
