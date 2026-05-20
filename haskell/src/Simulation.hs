module Simulation
( run
) where

import System.Random (getStdGen, StdGen)
import Chem.Buckle (turnbuckle)
import Util.Pair
import Motion.Model
import Control.Monad.State
import State.Chem
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.Environment
import Geometry.Vector
import Pane.View
import Pane.Frame
import Util.Enumer
import Pane.MousePos
import Geometry.Bound
import Util.Side
import Render.DrawSim

run :: IO ()
run = runSeeded =<< getStdGen

runSeeded :: StdGen -> IO ()
runSeeded seed = do
  ss <- getScreenSize
  let speed = 10
  let zoom = 50
  let frameRate = 30
  let bgColor = greyN 0.2
  let struct = turnbuckle
  let (model, nextSeed) = runState (buildModel speed struct) seed
  let initialFrame = Frame zeroV zoom
  let sp = 0.5 |* pmap fromIntegral ss
  let screen = Bound (zeroV |- sp) sp
  let view = initialView nextSeed screen initialFrame model struct
  
  play
    FullScreen
    bgColor
    frameRate
    view
    draw
    event
    update

event :: (Chem c, Enumer c) => Graphics.Gloss.Interface.IO.Interact.Event -> View c -> View c
event e v = case e of
  EventKey (MouseButton LeftButton) Down _ mpos -> leftDown (buildMousePos mpos) v
  EventKey (MouseButton LeftButton) Up _ mpos -> leftUp (buildMousePos mpos) v
  EventKey (MouseButton RightButton) Down _ mpos -> rightDown (buildMousePos mpos) v
  EventKey (MouseButton RightButton) Up _ mpos -> rightUp (buildMousePos mpos) v
  EventMotion mpos -> mouseMove (buildMousePos mpos) v
  EventKey (Char ch) Down _ _ -> keyDownEvent ch v
  EventKey (SpecialKey KeySpace) Down _ _ -> togglePlay v
  EventKey (SpecialKey KeyLeft) Down _ _ ->  v {frame = panHop leftV f}
  EventKey (SpecialKey KeyRight) Down _ _ -> v {frame = panHop rightV f}
  EventKey (SpecialKey KeyUp) Down _ _ ->    v {frame = panHop upV f}
  EventKey (SpecialKey KeyDown) Down _ _ ->  v {frame = panHop downV f}
  EventKey {} -> v
  EventResize _ -> v
  where f = frame v

keyDownEvent :: Chem c => Char -> View c -> View c
keyDownEvent ch v = case ch of
  '=' -> v {frame = zoomHop Out $ frame v}
  '-' -> v {frame = zoomHop In  $ frame v}
  's' -> v --saveModel $ model v
  'a' -> setMode Add v
  'd' -> setMode Delete v
  'e' -> setMode Edit v
  'm' -> setMode Move v
  _ -> v

update :: Chem c => Float -> View c -> View c
update dt v = case mode v of
  Run -> v {model = step (realToFrac dt) (model v)}
  _ -> v
