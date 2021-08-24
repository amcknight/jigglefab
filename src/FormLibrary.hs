module FormLibrary
( ballFormAt
, linearFormIncl, linearFormExcl
, chainFormIncl, chainFormExcl
, arcFormIncl
, box
) where

import Point
import Geometry.Angle
import Geometry.Vector
import Geometry.Space
import Ball
import Form
import Wall
import Utils
import Debug.Trace

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
chainFormIncl rad speed slack from to = linearFormIncl speed from to num
 where num = (ceiling (dist from to / rad) + 1) + slack

chainFormExcl :: Radius -> Float -> Int -> Position -> Position -> c -> R (Form c)
chainFormExcl rad speed slack from to = linearFormExcl speed from to num
 where num = (ceiling (dist from to / rad) + 1) + slack

linearFormIncl :: Float -> Position -> Position -> Int -> c -> R (Form c)
linearFormIncl speed from to num ch = do
  let poss = fromTo from to num
  vels <- randomVs speed num
  pure $ mconcat $ zipWith (toBallForm ch) poss vels
  where
    toBallForm :: c -> Position -> Velocity -> Form c
    toBallForm ch p v = ballForm $ Ball (Point p v) ch

linearFormExcl :: Float -> Position -> Position -> Int -> c -> R (Form c)
linearFormExcl speed from to num ch = do
  let poss = fromTo from to num
  vels <- randomVs speed num
  let _:ballForms = take (num-1) $ zipWith (toBallForm ch) poss vels
  pure $ mconcat ballForms
  where
    toBallForm :: c -> Position -> Velocity -> Form c
    toBallForm ch p v = ballForm $ Ball (Point p v) ch

arcFormIncl :: Float -> Angle -> Position -> Position -> Int -> c -> R (Form c)
arcFormIncl speed angle from to num ch = do
  let poss = trace (show (arcFromTo angle from to num)) arcFromTo angle from to num
  vels <- randomVs speed num
  pure $ mconcat $ zipWith (toBallForm ch) poss vels
  where
    toBallForm :: c -> Position -> Velocity -> Form c
    toBallForm ch p v = ballForm $ Ball (Point p v) ch
