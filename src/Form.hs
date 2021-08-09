module Form
( Form (Form)
, walls, balls
, wallForm, ballForm
, wallByI, ballByI
, ballsByI
, replaceBall
, replaceBalls
, bonkIndices, bounceIndices
, moveForm
) where

import Ball
import Balls
import Wall
import Pair
import qualified Data.Vector as V
import Space
import Time

type BallVector = V.Vector Ball
type WallVector = V.Vector Wall

data Form = Form
  { walls :: WallVector
  , balls :: BallVector
  } deriving Show

instance Semigroup Form where
  (<>) (Form w1 b1) (Form w2 b2) = Form (w1 <> w2) (b1 <> b2)
instance Monoid Form where
  mempty = Form V.empty V.empty 

wallForm :: Wall -> Form
wallForm w = Form (V.fromList [w]) V.empty 

ballForm :: Ball -> Form
ballForm b = Form V.empty (V.fromList [b])

wallByI :: Form -> Int -> Wall
wallByI m = (walls m V.!)

ballByI :: Form -> Int -> Ball
ballByI m = (balls m V.!)

ballsByI :: Form -> IP -> Balls
ballsByI = bimap . ballByI

replaceBall :: Int -> Ball -> Form -> Form
replaceBall i b f = Form (walls f) (balls f V.// [(i, b)])

replaceBalls :: IP -> Balls -> Form -> Form
replaceBalls ip bs f = Form ws (oldBs V.// [(i1, b1), (i2, b2)])
  where
    (i1, i2) = ip
    (b1, b2) = bs
    (Form ws oldBs) = f

bonkIndices :: Form -> [IP]
bonkIndices (Form ws bs) = prodTo (length ws) (length bs)

bounceIndices :: Form -> [IP]
bounceIndices (Form _ bs) = pairsTo (length bs)

moveForm :: Duration -> Form -> Form
moveForm dt (Form ws bs) = Form ws (fmap (moveBall dt) bs)