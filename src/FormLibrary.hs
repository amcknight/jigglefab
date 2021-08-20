module FormLibrary
( ballFormAt
, randomLinearFormIncl, randomLinearFormExcl
, chainFormIncl, chainFormExcl
, box
) where

import Point
import Vector
import Space
import Ball
import Form
import Wall
import Utils

box :: Vector -> Vector -> Form c
box (x1,y1) (x2,y2) =
  wallForm (VLine x1) <> 
  wallForm (VLine x2) <> 
  wallForm (HLine y1) <> 
  wallForm (HLine y2)

ballFormAt :: Float -> Position -> c -> R (Form c)
ballFormAt speed p c = do
  v <- randomV speed
  pure $ ballForm $ Ball (Point p v) c

chainFormIncl :: Radius -> Float -> Int -> Position -> Position -> c -> R (Form c)
chainFormIncl rad speed slack from to = randomLinearFormIncl speed from to num
 where num = (ceiling (dist from to / rad) + 1) + slack

chainFormExcl :: Radius -> Float -> Int -> Position -> Position -> c -> R (Form c)
chainFormExcl rad speed slack from to = randomLinearFormExcl speed from to num
 where num = (ceiling (dist from to / rad) + 1) + slack

randomLinearFormIncl :: Float -> Position -> Position -> Int -> c -> R (Form c)
randomLinearFormIncl speed from to num ch = do
  vels <- randomVs speed num
  let poss = fromTo from to num
  pure $ mconcat $ zipWith (toBallForm ch) poss vels
  where
    toBallForm :: c -> Position -> Velocity -> Form c
    toBallForm ch p v = ballForm $ Ball (Point p v) ch

randomLinearFormExcl :: Float -> Position -> Position -> Int -> c -> R (Form c)
randomLinearFormExcl speed from to num ch = do
  vels <- randomVs speed num
  let poss = fromTo from to num
  let _:ballForms = take (num-1) $ zipWith (toBallForm ch) poss vels
  pure $ mconcat ballForms
  where
    toBallForm :: c -> Position -> Velocity -> Form c
    toBallForm ch p v = ballForm $ Ball (Point p v) ch

