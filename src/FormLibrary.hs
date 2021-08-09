module FormLibrary
( randomLinearForm
, chainForm
, box
) where

import Point
import Vector
import Space
import Ball
import Form
import Wall
import Utils
import System.Random
import Control.Monad.State

box :: Vector -> Vector -> Form c
box (V x1 y1) (V x2 y2) =
  wallForm (wallV x1) <> 
  wallForm (wallV x2) <> 
  wallForm (wallH y1) <> 
  wallForm (wallH y2)

chainForm :: Radius -> Float -> Int -> Position -> Position -> c -> R (Form c)
chainForm rad speed slack from to = randomLinearForm speed from to num
 where num = (ceiling (dist from to / rad) + 1) + slack

randomLinearForm :: Float -> Position -> Position -> Int -> c -> R (Form c)
randomLinearForm speed from to num ch = do
  vels <- randomVs speed num
  let poss = fromTo from to num
  pure $ mconcat (zipWith (toBallForm ch) poss vels)
  where
    toBallForm :: c -> Position -> Velocity -> Form c
    toBallForm ch p v = ballForm $ Ball (Point p v) ch
