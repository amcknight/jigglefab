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
import ModelLibrary
import Wall
import Form
import Control.Monad.State
import Chem
import Electro.Electro
import Valence.Valence
import Core.CoreModels
import Gate.Gate
import Valence.ValenceModels
import Core.Core
import Debug.Trace
import Geometry.Angle
import qualified Color as C
import Graphics.Gloss
import Buckle.Buckle
import Peano.Peano
import Stripe.Stripe
import Sem.Sem
import Encode.Encode
import Load.Load
import Graphics.Gloss.Interface.IO.Interact
import StructLibrary
import View
import Geometry.Vector

run :: IO ()
run = runSeeded =<< getStdGen

runSeeded :: StdGen -> IO ()
runSeeded seed = do
  let (model, _) = runState movingToolModel seed
  let view = View model zeroV 50
  trace (show seed) play
    FullScreen
    (greyN 0.2)
    30
    view
    draw
    event
    update

draw :: Chem c => View c -> Picture
draw v = scale z z (Pictures (drawForm (form m)) <> Pictures (fmap (drawBond (form m)) (innerIps m)))
  where
    m = model v
    z = zoom v

drawForm :: Chem c => Form c -> [Picture]
drawForm f = ws ++ bs
  where
    ws = toList $ fmap (drawWall yellow) (walls f)
    bs = fmap drawBall (toList (balls f))

event :: Event -> View Sem -> View Sem
event e v = trace (show e) $ case e of
  EventKey key ks mod pos -> case (key, ks) of
    (MouseButton LeftButton, Down) -> v { model = add (Ball (Point pos (1,1)) (Sem.Sem.Wire [])) (model v) } -- TODO Should be random velocity and work with Orbs
    _ -> v
  EventMotion pos -> v
  EventResize _ -> v

update :: Duration -> View Sem -> View Sem 
update dt v = v { model = step dt (model v) }    

drawBall :: Chem c => Ball c -> Picture
drawBall (Ball (Point (x,y) _) chem) = translate x y $ body (C.toGlossColor (chemColor chem)) 1

drawWall :: Color -> Wall -> Picture
drawWall color (HLine y) = Color color $ line [(-1000000, y), (1000000, y)]
drawWall color (VLine x) = Color color $ line [(x, -1000000), (x, 1000000)]
drawWall color (Wall.Circle (x,y) rad) = Color color $ translate x y $ circle rad

drawBond :: Form c -> P Int -> Picture
drawBond f ip = Color white $ line [p1, p2]
  where (p1, p2) = pmap (pos . point . ballI f) ip

body :: Color -> Radius -> Picture
body color rad = Color color $ circleSolid rad
