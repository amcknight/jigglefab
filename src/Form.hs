module Form
( Form (..)
, buildForm
-- , wallForm, ballForm
, wallI, ballI
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
import Geometry.Space
import Point
import Geometry.Vector
import Struct
import Utils
import Orb

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

buildForm :: Speed -> Struct c -> R (Form c)
buildForm sp (Struct ws os) = do
  bs <- buildBallsForm sp os
  pure $ Form (V.fromList ws) (V.fromList []) <> bs
  where
    buildBallsForm :: Speed -> [Orb c] -> R (Form c)
    buildBallsForm _ [] = do pure mempty
    buildBallsForm sp (o:os) = do
      b <- buildBall sp o
      rest <- buildBallsForm sp os
      pure $ ballForm b <> rest

wallForm :: Wall -> Form c
wallForm w = Form (V.fromList [w]) V.empty 

ballForm :: Ball c -> Form c
ballForm b = Form V.empty (V.fromList [b])

wallI :: Form c -> Int -> Wall
wallI f = (walls f V.!)

ballI :: Form c -> Int -> Ball c
ballI f = (balls f V.!)

addBall :: Form c -> Ball c -> Form c
addBall (Form ws bs) b = Form ws $ V.snoc bs b

removeBall :: Form c -> Int -> Form c
removeBall (Form ws bs) i = Form ws (V.take i bs V.++ V.drop (i+1) bs)

wbSide :: Form c -> P Int -> Sided Int
wbSide f wbi@(wi, bi) = (wbi, wSide (wallI f wi) (pos (point (ballI f bi))))

bbSide :: Form c -> P Int -> Sided Int 
bbSide f bbi = (bbi, side (pmap (point . ballI f) bbi))

replaceBall :: Int -> Ball c -> Form c -> Form c
replaceBall i b (Form ws bs) = Form ws $ bs V.// [(i, b)]

replaceBalls :: P Int -> P (Ball c) -> Form c -> Form c
replaceBalls (i1, i2) (b1, b2) (Form ws bs) = Form ws $ bs V.// [(i1, b1), (i2, b2)]

bonkIndices :: Form c -> [P Int]
bonkIndices (Form ws bs) = prodTo (length ws) (length bs)

bounceIndices :: Form c -> [P Int]
bounceIndices (Form _ bs) = pairsTo (length bs)

toBonk :: Form c -> Side -> P Int -> Maybe Hit
toBonk f s ip@(wi, li) = fmap (\t -> Hit t s ip) (bonkTime s w p)
  where
    w = wallI f wi
    Ball p _ = ballI f li

bonkTime :: Side -> Wall -> Point -> Maybe Time
bonkTime s (VLine w) (Point (p,_) (v,_)) = bonkTime1d w p v
bonkTime s (HLine w) (Point (_,p) (_,v)) = bonkTime1d w p v
bonkTime s (Circle c r) p = case hitTimes r (Point c zeroV, p) of
  NoHit -> Nothing
  InHit t -> if s == In then Just t else Nothing
  OutAndInHit t1 t2 -> case s of
    Out -> Just t1
    In -> Just t2

bonkTime1d :: Float -> Float -> Float -> Maybe Time
bonkTime1d st dyn v = case compare t 0 of
  GT -> Just t
  _ -> Nothing
  where t = -(dyn-st)/v
