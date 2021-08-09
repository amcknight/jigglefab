module Form
( Form (Form)
, walls, balls
, wallForm, ballForm
, wallByI, ballByI
, ballsByI
, replaceBall
, replaceBalls
, bonkIndices, bounceIndices
, toBonk
, moveForm
) where

import qualified Data.Vector as V
import Ball
import Balls
import Wall
import Pair
import Time
import Hit
import Space
import Point
import Vector

data Form c = Form
  { walls :: V.Vector Wall
  , balls :: V.Vector (Ball c)
  } deriving Show

instance Semigroup (Form c) where
  (<>) (Form w1 b1) (Form w2 b2) = Form (w1 <> w2) (b1 <> b2)
instance Monoid (Form c) where
  mempty = Form V.empty V.empty 

wallForm :: Wall -> Form c
wallForm w = Form (V.fromList [w]) V.empty 

ballForm :: Ball c -> Form c
ballForm b = Form V.empty (V.fromList [b])

wallByI :: Form c -> Int -> Wall
wallByI m = (walls m V.!)

ballByI :: Form c -> Int -> Ball c
ballByI m = (balls m V.!)

ballsByI :: Form c -> IP -> Balls c
ballsByI = bimap . ballByI

replaceBall :: Int -> Ball c -> Form c -> Form c
replaceBall i b f = Form (walls f) (balls f V.// [(i, b)])

replaceBalls :: IP -> Balls c -> Form c -> Form c
replaceBalls ip bs f = Form ws (oldBs V.// [(i1, b1), (i2, b2)])
  where
    (i1, i2) = ip
    (b1, b2) = bs
    (Form ws oldBs) = f

bonkIndices :: Form c -> [IP]
bonkIndices (Form ws bs) = prodTo (length ws) (length bs)

bounceIndices :: Form c -> [IP]
bounceIndices (Form _ bs) = pairsTo (length bs)

toBonk :: Form c -> Side -> IP -> Maybe Hit
toBonk f s ip = case compare t 0 of
  GT -> Just $ Hit t s ip
  _ -> Nothing
  where
    (wi, li) = ip
    w = wallByI f wi
    Ball p _ = ballByI f li
    t = intersectTime w p

intersectTime :: Wall -> Point -> Time
intersectTime (Wall o p) (Point (V x y) (V xv yv)) = case o of
  Vertical -> -(x-p)/xv
  Horizontal -> -(y-p)/yv

moveForm :: Duration -> Form c -> Form c
moveForm dt (Form ws bs) = Form ws $ fmap (moveBall dt) bs
