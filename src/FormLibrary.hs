module FormLibrary
( randomForm
, randomLinearForm
, chainForm
, box
) where

import Point
import Vector
import Space
import Ball
import Chem
import Form
import Wall
import Utils
import System.Random
import Control.Monad.State

box :: Vector -> Vector -> Form 
box (V x1 y1) (V x2 y2) =
  wallForm (wallV x1) <> 
  wallForm (wallV x2) <> 
  wallForm (wallH y1) <> 
  wallForm (wallH y2)

randomForm :: Float -> Float -> Int -> R Form
randomForm _ _ 0 = do pure mempty
randomForm speed size num = do
  seed <- get
  let (valence, pSeed) = randomR (1, 3) seed
  put pSeed
  pos <- randomVIn size
  vel <- randomV speed
  let headForm = ballForm (Ball (Point pos vel) (buildChem valence))
  nextForms <- randomForm size speed (num-1)
  pure $ headForm <> nextForms

chainForm :: Radius -> Float -> Int -> Position -> Position -> R Form
chainForm rad speed slack from to = randomLinearForm speed from to num
 where num = (ceiling (dist from to / rad) + 1) + slack

randomLinearForm :: Float -> Position -> Position -> Int -> R Form
randomLinearForm speed from to num = do
  vels <- randomVs speed num
  let poss = fromTo from to num
  pure $ mconcat (zipWith toBallForm poss vels)
  where
    toBallForm :: Position -> Velocity -> Form
    toBallForm p v = ballForm $ Ball (Point p v) chem2
