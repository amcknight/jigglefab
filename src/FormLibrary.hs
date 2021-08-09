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

randomForm :: StdGen -> Float -> Float -> Int -> (Form, StdGen)
randomForm seed _ _ 0 = (mempty, seed)
randomForm seed speed size num = (headForm <> nextForms, newSeed)
  where
    headForm = ballForm (Ball (Point pos vel) (buildChem valence))
    (nextForms, newSeed) = randomForm tailSeed size speed (num-1)
    (valence, pSeed) = randomR (1, 3) seed :: (Int, StdGen)
    (pos, vSeed) = randomVIn pSeed size
    (vel, tailSeed) = randomV vSeed speed

randomLinearForm :: StdGen -> Float -> Position -> Position -> Int -> (Form, StdGen)
randomLinearForm seed speed from to num = (mconcat (zipWith toBallForm poss vels), newSeed)
  where
    (vels, newSeed) = randomVs seed speed num
    poss = fromTo from to num
    toBallForm :: Position -> Velocity -> Form
    toBallForm p v = ballForm $ Ball (Point p v) chem2

chainForm :: StdGen -> Radius -> Float -> Position -> Position -> (Form, StdGen)
chainForm seed rad speed from to = randomLinearForm seed speed from to num
 where num = round $ dist (from,to) / (rad-0.001) + 1
