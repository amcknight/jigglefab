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

randomForm :: Float -> Float -> Int -> StdGen -> (StdGen, Form)
randomForm _ _ 0 seed = (seed, mempty)
randomForm speed size num seed = let
    (valence, pSeed) = randomR (1, 3) seed
    (vSeed, pos) = randomVIn size pSeed
    (tailSeed, vel) = randomV speed vSeed
    headForm = ballForm (Ball (Point pos vel) (buildChem valence))
    (newSeed, nextForms) = randomForm size speed (num-1) tailSeed
  in (newSeed, headForm <> nextForms)

chainForm :: Radius -> Float -> Position -> Position -> StdGen -> (StdGen, Form)
chainForm rad speed from to = randomLinearForm speed from to num
 where num = round $ dist (from,to) / (rad-0.001) + 1

randomLinearForm :: Float -> Position -> Position -> Int -> StdGen -> (StdGen, Form)
randomLinearForm speed from to num seed = let
    (newSeed, vels) = randomVs speed num seed
    poss = fromTo from to num
  in (newSeed, mconcat (zipWith toBallForm poss vels))
  where
    toBallForm :: Position -> Velocity -> Form
    toBallForm p v = ballForm $ Ball (Point p v) chem2
