module FormLibrary
( ballFormAt
, linFormIncl, linFormExcl
, linChainFormIncl, linChainFormExcl
, arcFormIncl, arcFormExcl
, arcChainFormIncl, arcChainFormExcl
, sigForm
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

sigForm :: Float -> Position -> Position -> [c] -> R (Form c)
sigForm _ _ _ [] = do pure mempty 
sigForm sp _ to [ch] = ballFormAt sp to ch
sigForm sp from to (ch:cs) = do
  form <- ballFormAt sp from ch
  let gap = (1/ fromIntegral (length cs)) |* (to |- from)
  forms <- sigForm sp (from |+ gap) to cs
  pure $ form <> forms

linFormIncl :: Float -> Position -> Position -> Int -> c -> R (Form c)
linFormIncl speed from to num ch = do
  let poss = fromTo from to num
  vels <- randomVs speed num
  pure $ mconcat $ zipWith (toBallForm ch) poss vels
  where
    toBallForm :: c -> Position -> Velocity -> Form c
    toBallForm ch p v = ballForm $ Ball (Point p v) ch

linFormExcl :: Float -> Position -> Position -> Int -> c -> R (Form c)
linFormExcl speed from to num ch = do
  let poss = fromTo from to num
  vels <- randomVs speed num
  let _:ballForms = take (num-1) $ zipWith (toBallForm ch) poss vels
  pure $ mconcat ballForms
  where
    toBallForm :: c -> Position -> Velocity -> Form c
    toBallForm ch p v = ballForm $ Ball (Point p v) ch

arcFormIncl :: Float -> Angle -> Position -> Position -> Int -> c -> R (Form c)
arcFormIncl speed angle from to num ch = do
  let poss = arcFromTo angle from to num
  vels <- randomVs speed num
  pure $ mconcat $ zipWith (toBallForm ch) poss vels
  where
    toBallForm :: c -> Position -> Velocity -> Form c
    toBallForm ch p v = ballForm $ Ball (Point p v) ch

arcFormExcl :: Float -> Angle -> Position -> Position -> Int -> c -> R (Form c)
arcFormExcl speed angle from to num ch = do
  let poss = arcFromTo angle from to num
  vels <- randomVs speed num
  let _:ballForms = take (num-1) $ zipWith (toBallForm ch) poss vels
  pure $ mconcat ballForms
  where
    toBallForm :: c -> Position -> Velocity -> Form c
    toBallForm ch p v = ballForm $ Ball (Point p v) ch

linChainFormIncl :: Radius -> Float -> Int -> Position -> Position -> c -> R (Form c)
linChainFormIncl rad speed slack from to = linFormIncl speed from to num
  where num = (ceiling (dist from to / rad) + 1) + slack

linChainFormExcl :: Radius -> Float -> Int -> Position -> Position -> c -> R (Form c)
linChainFormExcl rad speed slack from to = linFormExcl speed from to num
  where num = (ceiling (dist from to / rad) + 1) + slack

arcChainFormIncl :: Radius -> Float -> Angle -> Int -> Position -> Position -> c -> R (Form c)
arcChainFormIncl rad speed a slack from to = arcFormIncl speed a from to num
  where num = (ceiling (arcDist a from to / rad) + 1) + slack

arcChainFormExcl :: Radius -> Float -> Angle -> Int -> Position -> Position -> c -> R (Form c)
arcChainFormExcl rad speed a slack from to = arcFormExcl speed a from to num
  where num = (ceiling (arcDist a from to / rad) + 1) + slack