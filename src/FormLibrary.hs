module FormLibrary
( randomForm
, randomLinearForm
, chainForm
, box
) where

import Point
import Vector
import Space
import System.Random
import Ball
import Chem
import Vectors
import Form
import Wall

box :: Vectors -> Form 
box (V x1 y1, V x2 y2) =
  wallForm (wallV x1) <> 
  wallForm (wallV x2) <> 
  wallForm (wallH y1) <> 
  wallForm (wallH y2)

randomForm :: StdGen -> Float -> Float -> Int -> Form
randomForm _ _ _ 0 = mempty
randomForm seed speed size num = ballForm (Ball (Point pos vel) (buildChem valence)) <> randomForm newSeed size speed (num-1)
  where
    (valence, pSeed) = randomR (1, 3) seed :: (Int, StdGen)
    (pos, vSeed) = randomVIn pSeed size
    (vel, newSeed) = randomV vSeed speed

randomLinearForm :: StdGen -> Float -> Position -> Position -> Int -> Form
randomLinearForm seed speed from to num = mconcat $ zipWith toBallForm poss vels
  where
    vels = fmap (speed |*) $ take num $ randoms seed
    poss = fromTo from to num
    toBallForm :: Position -> Velocity -> Form
    toBallForm p v = ballForm $ Ball (Point p v) chem2

chainForm :: StdGen -> Radius -> Float -> Position -> Position -> Form
chainForm seed rad speed from to = randomLinearForm seed speed from to num
 where num = round $ dist (from,to) / (rad-0.001) + 1
