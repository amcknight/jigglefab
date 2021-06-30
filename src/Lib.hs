module Lib
    ( someFunc
    ) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort ( ViewPort )
import System.Random

type Position = (Float, Float)
type Momentum = (Float, Float)
type Link = (Position, Momentum)
type Model = [Link]

-- windowDisplay :: Display
-- windowDisplay = InWindow "Window" (1000, 1000) (10, 10)

someFunc :: IO ()
someFunc = simulate
  FullScreen
  cyan
  simulationRate
  initialModel
  drawingFunc
  updateFunc
  where
    simulationRate :: Int
    simulationRate = 20

    initialModel :: Model
    initialModel =  [((x,y), (x/1000,y/1000)) | x <- [-1000,-960..1000], y <- [-1000,-960..1000]]

      -- [((0,0), (3,3)), ((-5,-5), (-2, 1))]
    

    drawingFunc :: Model -> Picture
    drawingFunc links = Pictures $ fmap drawLink links

    drawLink :: Link -> Picture
    drawLink ((x, y), _) = Pictures $ fmap (translate x y) [Color blue (circleSolid 10), Color white (circleSolid 1)]

    updateFunc :: ViewPort -> Float -> Model -> Model
    updateFunc _ dt links = fmap (moveLink dt) links

    moveLink :: Float -> Link -> Link
    moveLink dt ((x, y), (vx, vy)) = ((x + vx, y + vy), (vx, vy))
