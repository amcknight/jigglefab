module Geometry.Bound
( Bound
, maxX, maxY, minX, minY
, overlap
, boundOne
, bound, bufferedBound
, rayCrossBound
, isIn
) where
      
import Pair
import Geometry.Vector
import Geometry.Angle
import Geometry.CrossPoint
import Debug.Trace
import Data.Maybe (isJust)

type Bound = (P Position)

maxX :: Bound -> Float
maxX = fst . fst
maxY :: Bound -> Float
maxY = snd . fst
minX :: Bound -> Float
minX = fst . snd
minY :: Bound -> Float
minY = snd . snd

overlap :: Bound -> Bound -> Maybe Bound
overlap b1 b2
  | minX b1 > maxX b2 || minY b1 > maxY b2 || minX b2 > maxX b1 || minY b2 > maxY b1 = Nothing 
  | otherwise = Just ((min (maxX b1) (maxX b2), min (maxY b1) (maxY b2)), (max (minX b1) (minX b2), max (minY b1) (minY b2)))

boundOne :: Position -> Bound
boundOne p = (p,p)

bound :: [Position] -> Bound
bound [] = error "Can't bound nothing"
bound [v] = boundOne v
bound ((x,y):vs) = ((max maxX x, max maxY y), (min minX x, min minY y))
  where ((maxX, maxY), (minX, minY)) = bound vs

bufferedBound :: [Position] -> Float -> Bound
bufferedBound vs b = ((maxX+b, maxY+b), (minX-b, minY-b))
  where ((maxX, maxY), (minX, minY)) = bound vs

rayCrossBound :: Bound -> Position -> Turn -> CrossPoints
rayCrossBound bnd@((mxX,mxY),(mnX,mnY)) p@(x,y) dir = case overlap bnd (boundOne p) of
  Nothing -> if length onBound < 2 then NoCross
    else if length onBound > 2 then error "TODO: Three crosspoints on the bound. Possible only in very edgy cases"
    else
      let [b1, b2] = onBound 
      in case separation dir (direction (b1 |- p)) of
        Zero -> TwoCross b1 b2
        Acute -> TwoCross b1 b2
        _ -> NoCross 
  Just _ -> OneCross $ case compass dir of
    East -> (mxX, y)
    North -> (x, mxY)
    West -> (mnX, y)
    South -> (x, mnY)
    NorthEast -> closer pForMaxX pForMaxY
    NorthWest -> closer pForMinX pForMaxY
    SouthWest -> closer pForMinX pForMinY
    SouthEast -> closer pForMaxX pForMinY
  where
    s = slope dir -- breaks on vertical or horizontal
    b = y - s * x
    mnXY = s * mnX + b
    mxXY = s * mxX + b
    mnYX = (mnY-b) / s
    mxYX = (mxY-b) / s
    pForMinX = (mnX, mnXY)
    pForMinY = (mnYX, mnY)
    pForMaxX = (mxX, mxXY)
    pForMaxY = (mxYX, mxY)
    mnXYIn = mnXY < mxY && mnXY > mnY
    mxXYIn = mxXY < mxY && mxXY > mnY
    mnYXIn = mnYX < mxX && mnYX > mnX
    mxYXIn = mxYX < mxX && mxYX > mnX
    onBound = fmap snd $ filter fst $ zip [mnXYIn, mxXYIn, mnYXIn, mxYXIn] [pForMinX, pForMaxX, pForMinY, pForMaxY]

    closer :: Position -> Position -> Position
    closer a b = if distSq p a < distSq p b then a else b

isIn :: Bound -> Position -> Bool
isIn b = isJust . overlap b . boundOne
