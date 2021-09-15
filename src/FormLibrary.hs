module FormLibrary
( ballFormAt
, cappedLinFormIncl, cappedLinFormExcl
, cappedArcFormIncl
, linFormIncl, linFormExcl
, cappedLinChainFormIncl, cappedLinChainFormExcl
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
import Time

box :: Vector -> Vector -> Form c
box (x1,y1) (x2,y2) =
  wallForm (VLine x1) <> 
  wallForm (VLine x2) <> 
  wallForm (HLine y1) <> 
  wallForm (HLine y2)

ballFormAt :: Speed -> Position -> c -> R (Form c)
ballFormAt speed p c = do
  v <- randomV speed
  pure $ ballForm $ Ball (Point p v) c

sigForm :: Speed -> Position -> Position -> [c] -> R (Form c)
sigForm _ _ _ [] = do pure mempty 
sigForm sp _ to [ch] = ballFormAt sp to ch
sigForm sp from to (ch:cs) = do
  form <- ballFormAt sp from ch
  let gap = (1 / fromIntegral (length cs)) |* (to |- from)
  forms <- sigForm sp (from |+ gap) to cs
  pure $ form <> forms

cappedLinFormIncl :: Speed -> Position -> Position -> Int -> [c] -> c -> [c] -> R (Form c)
cappedLinFormIncl speed from to num fromCh ch toCh = do
  let poss = fromTo from to num
  vels <- randomVs speed num
  let chems = fromCh ++ replicate (num - length fromCh - length toCh) ch ++ reverse toCh
  pure $ mconcat $ zipWith3 toBallForm chems poss vels
  where
    toBallForm :: c -> Position -> Velocity -> Form c
    toBallForm ch p v = ballForm $ Ball (Point p v) ch

cappedLinFormExcl :: Speed -> Position -> Position -> Int -> [c] -> c -> [c] -> R (Form c)
cappedLinFormExcl speed from to num = cappedLinFormIncl speed (from |+ step) (to |- step) (num-2)
  where
    numHops = fromIntegral $ num - 1
    step = (1/numHops) |* (to |- from)

cappedArcFormIncl :: Speed -> Angle -> Position -> Position -> Int -> [c] -> c -> [c] -> R (Form c)
cappedArcFormIncl speed angle from to num fromCh ch toCh = do
  let poss = arcFromTo angle from to num
  vels <- randomVs speed num
  let chems = fromCh ++ replicate (num - length fromCh - length toCh) ch ++ reverse toCh
  pure $ mconcat $ zipWith3 toBallForm chems poss vels
  where
    toBallForm :: c -> Position -> Velocity -> Form c
    toBallForm ch p v = ballForm $ Ball (Point p v) ch

linFormIncl :: Speed -> Position -> Position -> Int -> c -> R (Form c)
linFormIncl speed from to num ch = cappedLinFormIncl speed from to num [] ch []

linFormExcl :: Speed -> Position -> Position -> Int -> c -> R (Form c)
linFormExcl speed from to num = linFormIncl speed (from |+ step) (to |- step) (num-2)
  where step = (1/fromIntegral (num-1)) |* to |- from

arcFormIncl :: Speed -> Angle -> Position -> Position -> Int -> c -> R (Form c)
arcFormIncl speed angle from to num ch = cappedArcFormIncl speed angle from to num [] ch []

arcFormExcl :: Speed -> Angle -> Position -> Position -> Int -> c -> R (Form c)
arcFormExcl speed angle from to num ch = do
  let poss = arcFromTo angle from to num
  vels <- randomVs speed num
  let _:ballForms = take (num-1) $ zipWith (toBallForm ch) poss vels
  pure $ mconcat ballForms
  where
    toBallForm :: c -> Position -> Velocity -> Form c
    toBallForm ch p v = ballForm $ Ball (Point p v) ch

cappedLinChainFormIncl :: Radius -> Speed -> Int -> Position -> Position -> [c] -> c -> [c] -> R (Form c)
cappedLinChainFormIncl rad speed slack from to = cappedLinFormIncl speed from to num
  where num = (ceiling (dist from to / rad) + 1) + slack

linChainFormIncl :: Radius -> Speed -> Int -> Position -> Position -> c -> R (Form c)
linChainFormIncl rad speed slack from to ch = cappedLinChainFormIncl rad speed slack from to [] ch []

cappedLinChainFormExcl :: Radius -> Speed -> Int -> Position -> Position -> [c] -> c -> [c] -> R (Form c)
cappedLinChainFormExcl rad speed slack from to = cappedLinFormExcl speed from to num
  where num = (ceiling (dist from to / rad) + 1) + slack

linChainFormExcl :: Radius -> Speed -> Int -> Position -> Position -> c -> R (Form c)
linChainFormExcl rad speed slack from to = linFormExcl speed from to num
  where num = (ceiling (dist from to / rad) + 1) + slack

arcChainFormIncl :: Radius -> Speed -> Angle -> Int -> Position -> Position -> c -> R (Form c)
arcChainFormIncl rad speed a slack from to = arcFormIncl speed a from to num
  where num = (ceiling (arcDist a from to / rad) + 1) + slack

arcChainFormExcl :: Radius -> Speed -> Angle -> Int -> Position -> Position -> c -> R (Form c)
arcChainFormExcl rad speed a slack from to = arcFormExcl speed a from to num
  where num = (ceiling (arcDist a from to / rad) + 1) + slack
