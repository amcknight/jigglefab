module Form
( Form (Form)
, walls, balls
, wallForm, ballForm
, wallByI, ballByI
, wbSide, bbSide
, replaceBall
, replaceBalls
, addBall
, removeBall
, bonkIndices, bounceIndices
, toBonk
) where

import qualified Data.Vector as V
import Ball
import Wall
import Pair
import Time
import Hit
import HitTime
import Space
import Point
import Vector
import GHC.Base (Maybe(Nothing))

data Form c = Form
  { walls :: V.Vector Wall
  , balls :: V.Vector (Ball c)
  } deriving Show

instance Mover (Form c) where 
  move dt (Form ws bs) = Form ws $ fmap (move dt) bs

instance Semigroup (Form c) where
  (<>) (Form w1 b1) (Form w2 b2) = Form (w1 <> w2) (b1 <> b2)
instance Monoid (Form c) where
  mempty = Form V.empty V.empty 

wallForm :: Wall -> Form c
wallForm w = Form (V.fromList [w]) V.empty 

ballForm :: Ball c -> Form c
ballForm b = Form V.empty (V.fromList [b])

wallByI :: Form c -> Int -> Wall
wallByI f = (walls f V.!)

ballByI :: Form c -> Int -> Ball c
ballByI f = (balls f V.!)

addBall :: Form c -> Ball c -> Form c
addBall (Form ws bs) b = Form ws $ V.snoc bs b

removeBall :: Form c -> Int -> Form c
removeBall (Form ws bs) i = Form ws (V.take i bs V.++ V.drop (i+1) bs)

wbSide :: Form c -> P Int -> Sided Int
wbSide f wbi@(wi, bi) = (wbi, wSide (wallByI f wi) (pos (point (ballByI f bi))))

bbSide :: Radius -> Form c -> P Int -> Sided Int 
bbSide rad f bbi = (bbi, side rad (pmap (point . ballByI f) bbi))

replaceBall :: Int -> Ball c -> Form c -> Form c
replaceBall i b (Form ws bs) = Form ws $ bs V.// [(i, b)]

replaceBalls :: P Int -> P (Ball c) -> Form c -> Form c
replaceBalls (i1, i2) (b1, b2) (Form ws oldBs) = Form ws $ oldBs V.// [(i1, b1), (i2, b2)]

bonkIndices :: Form c -> [P Int]
bonkIndices (Form ws bs) = prodTo (length ws) (length bs)

bounceIndices :: Form c -> [P Int]
bounceIndices (Form _ bs) = pairsTo (length bs)

toBonk :: Form c -> Side -> P Int -> Maybe Hit
toBonk f s ip = case mt of
  Nothing -> Nothing
  Just t -> Just $ Hit t s ip
  where
    (wi, li) = ip
    w = wallByI f wi
    Ball p _ = ballByI f li
    mt = intersectTime s w p

intersectTime :: Side -> Wall -> Point -> Maybe Time
intersectTime s (VLine wx) (Point (x,_) (xv,_)) = case compare t 0 of
  GT -> Just t
  _ -> Nothing
  where t = -(x-wx)/xv
intersectTime s (HLine wy) (Point (_,y) (_,yv)) = case compare t 0 of
  GT -> Just t
  _ -> Nothing
  where t = -(y-wy)/yv
intersectTime s (Circle pl r) p = case hitTimes r (Point pl zeroV, p) of
  NoHit -> Nothing
  InHit t -> if s == In then Just t else Nothing
  OutAndInHit t1 t2 -> case s of
    Out -> Just t1
    In -> Just t2
