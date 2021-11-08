{-# LANGUAGE FlexibleInstances #-}

module Geometry.Vector
( Vector
, Position, Velocity
, AnchorPos(..)
, zeroV
, unit, direction, radians
, unitV
, upV, downV, rightV, leftV
, upRightV, upLeftV, downRightV, downLeftV
, toUnit
, rotate, rotateAround
, magnitudeSq, magnitude
, randomV, randomVs, randomVIn
, (|*), (|+), (|-)
, (|.)
-- , colinear
, fromTo, arcFromTo
, distSq, dist
, arcDist
, reflect
, squashTurn
, turnDirection
, mid
, colinear
, show2
, fromBy
) where

import Control.Monad.State
import System.Random as R
import Geometry.Angle
import Geometry.Space
import Utils
import Pair
import Data.List (sort, nub)
import Debug.Trace
import Data.Maybe (mapMaybe)

type Vector = P Float
type Position = Vector
type Velocity = Vector

instance Near Vector where
  near dec v1 v2 = magnitude (v2 |- v1) < 1/10^dec

instance Random Vector where
  randomR ((x1,y1),(x2,y2)) g = ((x,y), g3)
    where
      (x, g2) = randomR (x1, x2) g
      (y, g3) = randomR (y1, y2) g2
  random g = (toUnit theta, g2)
    where
      (theta, g2) = randomR (-pi, pi) g

class AnchorPos a where
  pos :: a -> Position
  
instance AnchorPos Position where
  pos = id


show2 :: Position -> String
show2 (x, y) = show (truncF x 2, truncF y 2)

toUnit:: Radian -> Vector
toUnit a = (cos a, sin a)

unit :: Vector -> Maybe Vector
unit (0,0) = Nothing
unit v = Just $ (1 / magnitude v) |* v

unitV :: Turn -> Vector
unitV = toUnit . (*tau)

zeroV :: Vector
zeroV = (0,0)
upV :: Vector
upV = (0,1)
downV :: Vector
downV = (0,-1)
rightV :: Vector
rightV = (1,0)
leftV :: Vector
leftV = (-1,0)

upRightV :: Vector
upRightV = upV |+ rightV
upLeftV :: Vector
upLeftV = upV |+ leftV
downRightV :: Vector
downRightV = downV |+ rightV
downLeftV :: Vector
downLeftV = downV |+ leftV

direction :: Vector -> Maybe Turn 
direction v = case radians v of
  Nothing -> Nothing
  Just r -> Just $ toTurn r

radians :: Vector -> Maybe Radian
radians (0,0) = Nothing
radians (x,y) = Just $ atan2 y x

rotate :: Vector -> Turn -> Vector
rotate v t = case direction v of
  Nothing -> (0,0)
  Just dv -> magnitude v |* unitV (simple (dv + simple t))

rotateAround :: Vector -> Turn -> Vector -> Vector
rotateAround c t v = rotate (v |- c) t |+ c

magnitudeSq :: Vector -> Float
magnitudeSq v = v |. v

magnitude :: Vector -> Float 
magnitude = sqrt . magnitudeSq

randomV :: Float -> R Vector
randomV len = do
  seed <- get
  let (unit, newSeed) = random seed
  put newSeed
  pure $ len |* unit

randomVs :: Float -> Int -> R [Vector]
randomVs _ 0 = do pure mempty
randomVs len num = do
  vel <- randomV len
  vels <- randomVs len $ num - 1
  pure $ vel : vels

randomVIn :: Float -> R Vector
randomVIn maxLen = do
  seed <- get
  let (lenFactor, vSeed) = randomR (0.0, 1.0) seed
  put vSeed
  randomV $ maxLen * sqrt lenFactor

reflect :: Ortho -> Vector -> Vector
reflect Vertical (x,y) = (-x,y)
reflect Horizontal (x,y) = (x,-y)

(|*) :: Float -> Vector -> Vector
(|*) scale = pmap (scale *)

(|-) :: Vector -> Vector -> Vector
(|-) = pairwise (-)

(|+) :: Vector -> Vector -> Vector
(|+) = pairwise (+)

(|.) :: Vector -> Vector -> Float
(|.) (x1,y1) (x2,y2) = x1*x2 + y1*y2

pairwise :: (Float -> Float -> Float) -> Vector -> Vector -> Vector
pairwise f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

fromTo :: Vector -> Vector -> Int -> [Vector]
fromTo _ _ 0 = []
fromTo v1 _ 1 = [v1]
fromTo v1 v2 n = v1 : fromTo (v1 |+ hop) v2 (n-1)
  where
    numHops = fromIntegral n - 1
    hop = (1/numHops) |* (v2 |- v1)

fromBy :: Vector -> Vector -> Int -> [Vector]
fromBy _ _ 0 = []
fromBy v gap n = fmap ((|+ v) . (|* gap) . fromIntegral) [0..n-1]

arcFromTo :: Radian -> Vector -> Vector -> Int -> [Vector]
arcFromTo _ _ _ 0 = []
arcFromTo _ _ v2 1 = [v2]
arcFromTo _ v1 v2 2 = [v1, v2]
arcFromTo a v1 v2 n = fmap (\i -> rotateAround (i*gap) c v1) [0..numHops]
  where
    numHops = fromIntegral $ n - 1
    m = mid v1 v2
    v1m = 0.5 |* (v2 |- v1)
    v1mMagSq = magnitudeSq v1m
    cmMagSq = radSqFromArc a v1 v2 - v1mMagSq
    leftCM = (sqrt cmMagSq / sqrt v1mMagSq) |* negOpp v1m
    c = (if a < toRadian 0.5 then (m |+) else (m |-)) leftCM
    gap = a / numHops

    rotateAround :: Radian -> Vector -> Vector -> Vector
    rotateAround a c v = let cv = v |- c
      in case radians cv of
        Nothing -> c
        Just rad -> c |+ (magnitude cv |* toUnit (a + rad))

distSq :: Vector -> Vector -> Float
distSq v1 v2 = magnitudeSq $ v2 |- v1

dist :: Vector -> Vector -> Float
dist v1 v2 = magnitude $ v2 |- v1

arcDist :: Radian -> Vector -> Vector -> Float 
arcDist a v1 v2 = a * sqrt (radSqFromArc a v1 v2)

negOpp :: Vector -> Vector
negOpp (x,y) = (-y, x)

radSqFromArc :: Radian -> Vector -> Vector -> Float
radSqFromArc a v1 v2 = distSq v1 v2 / chord a ^ 2

squashTurn :: Radius -> Vector -> Vector -> Turn
squashTurn rad v1 v2 = if 2*rad <= d then 0 else toTurn $ acos $ (d/2)/rad
  where d = dist v1 v2

colinear :: Int -> [Position] -> Bool
colinear dec ps = colinear' dec $ nub ps
colinear' :: Int -> [Position] -> Bool
colinear' _ [] = True
colinear' _ [p] = True
colinear' _ [p,q] = True
colinear' dec aps@(a:b:ps) = all (withinAngle acc dirB) (mapMaybe dirFromA ps)
  where
    Just dirB = dirFromA b
    dirFromA = direction . (|- a)
    acc = 1/10^dec

turnDirection :: Position -> Position -> Position -> Maybe TurnDirection
turnDirection (x1,y1) (x2,y2) (x3,y3) = case compare det 0 of
  LT -> Just Clockwise
  EQ -> Nothing
  GT -> Just CounterClockwise 
  where det = x1*(y2 - y3) - y1*(x2 - x3) + (x2*y3 - y2*x3)

mid :: Vector -> Vector -> Vector
mid v1 = (0.5 |*) . (v1 |+)
