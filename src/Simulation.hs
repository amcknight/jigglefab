{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Simulation
( run
) where

import Data.Vector (toList)
import System.Random (getStdGen, StdGen)
import qualified Data.Vector as V
import qualified Color as C
import Geometry.Circle
import Geometry.Space
import Point
import Ball
import Pair
import Model
import Wall
import Form
import Control.Monad.State
import Chem
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Geometry.Vector
import Struct
import Orb
import Tiling
import Voronoi.Fortune
import Geometry.Line
import Geometry.Parabola
import Voronoi.Beach
import Geometry.Bound
import Geometry.Angle
import Voronoi.Pie
import Voronoi.Tri
import Voronoi.Sweep
import Chem.Buckle
import Voronoi.Edge
import Voronoi.Event
import Draw
import Pane.View
import Pane.EditView
import Pane.RunView
import Pane.Frame
import Enumer

run :: IO ()
run = runSeeded =<< getStdGen

speeed :: Double
speeed = 10

runSeeded :: StdGen -> IO ()
runSeeded seed = do
  let struct = turnbuckle
  let (model, nextSeed) = runState (buildModel speeed struct) seed
  let initialFrame = Frame zeroV 50
  let view = View Run (EditView 0 Nothing Nothing struct) (RunView nextSeed model) initialFrame
  let frameRate = 30
  play
    FullScreen
    (greyN 0.2)
    frameRate
    view
    draw
    event
    update

event :: Chem c => Graphics.Gloss.Interface.IO.Interact.Event -> View c -> View c
event e v = case e of
  EventKey (MouseButton LeftButton) Down _ mpos -> lClick (pmap realToFrac mpos) v
  EventKey (MouseButton RightButton) Down _ mpos -> rClick (pmap realToFrac mpos) v
  EventKey (Char '=') Down _ _ -> v {frame = zoomHop Out $ frame v}
  EventKey (Char '-') Down _ _ -> v {frame = zoomHop In $ frame v}
  EventKey (SpecialKey KeySpace) Down _ _ -> togglePlay speeed v
  EventKey (SpecialKey KeyLeft) Down _ _ ->  v {frame = panHop leftV $ frame v}
  EventKey (SpecialKey KeyRight) Down _ _ -> v {frame = panHop rightV $ frame v}
  EventKey (SpecialKey KeyUp) Down _ _ ->    v {frame = panHop upV $ frame v}
  EventKey (SpecialKey KeyDown) Down _ _ ->  v {frame = panHop downV $ frame v}
  EventKey {} -> v
  EventMotion mpos -> mMove (pmap realToFrac mpos) v
  EventResize _ -> v

update :: Chem c => Float -> View c -> View c
update dt v = case mode v of
  Edit -> v
  Run -> v {runView = (runView v) {model = step (realToFrac dt) (model (runView v))}}

draw :: (Chem c, Enumer c) => View c -> Picture
draw v = case mode v of
  Edit -> drawEditView (frame v) (editView v)
  Run -> drawRunView (frame v) (runView v)

drawEditView :: forall c . (Chem c, Enumer c) => Frame -> EditView c -> Picture
drawEditView f ev = Pictures
  [ drawStruct f $ struct ev
  , drawOrbHover f (orbHover ev) (struct ev)
  , drawSidebar (vals @c) (tip ev) (menuHover ev)
  ]

drawRunView :: Chem c => Frame -> RunView c -> Picture
drawRunView f rv = toFrame f $ drawModel $ model rv

drawStruct :: Chem c => Frame -> Struct c -> Picture
drawStruct f (Struct walls os) = toFrame f $ Pictures $
  fmap (drawWall yellow) walls
  <> fmap drawOrb os
  <> fmap drawEdge es
  <> fmap (drawOrbWedge (V.fromList os)) ws
  <> beach
  where
    es = voronoi ps
    ps = fmap pos os
    ws = tileVoronoi (V.fromList ps) es
    beach = [drawBeach (zoom f) (V.fromList os) (processBeach (initialBeach ps) 4)]

drawBeach :: Chem c => Double -> V.Vector (Orb c) -> Beach -> Picture
drawBeach z os (Beach sw _ es bs rs) = Pictures $
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
  Just p -> toLine $ parabPoss p xBound 0.01 -- TODO: Should be sensitive to zooom

drawSweep :: Double -> [Picture]
drawSweep h = [Color black $ toLine [(-100000, h), (100000, h)]]

drawPosAt :: Double -> Position -> Color -> Picture
drawPosAt z pos c = Color c $ toTranslate pos $ toCircleSolid $ 5/z

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

drawEdge :: Edge -> Picture
drawEdge = Color white . drawSeg . seg

drawSeg :: Seg -> Picture
drawSeg (Seg p q) = toLine [p, q]

drawOrbHover :: Frame -> Maybe (Orb c) -> Struct c -> Picture
drawOrbHover _ Nothing _ = blank
drawOrbHover f (Just (Orb p _)) _ = toFrame f $ toTranslate p $ toCircleSolid $ 20 / zoom f

-- TODO: Remove all these magic numbers
drawSidebar :: Chem c => [c] -> Int -> Maybe Int -> Picture
drawSidebar chs selI hovI = translate (-1850) 1000 $ Pictures pics
  where
    pics = spreadSelections $ zipWith drawTokenSelector chs flairs
    flairs = map (buildFlair hovI selI) is
    is = [0..length chs]
    spreadSelections :: [Picture] -> [Picture]
    spreadSelections ps = zipWith (\i p -> translate 0 (-40*fromIntegral i) p) is ps

buildFlair :: Maybe Int -> Int -> Int -> Picture
buildFlair hovI selI i
  | selI == i = color white pointer
  | hovI == Just i = color black pointer
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
