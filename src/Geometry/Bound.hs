module Geometry.Bound
( Bound(..)
, maxX, maxY, minX, minY
, overlap
, boundOne
, bound, bufferedBound
, isIn
) where
      
import Data.Maybe (isJust)
import Geometry.Vector

data Bound = Bound 
  { maxB :: Position
  , minB :: Position
  } deriving Show

instance Semigroup Bound where
  (<>) (Bound (mxX1, mxY1) (mnX1, mnY1)) (Bound (mxX2, mxY2) (mnX2, mnY2)) = Bound (max mxX1 mxX2, max mxY1 mxY2) (min mnX1 mnX2, min mnY1 mnY2)

maxX :: Bound -> Float
maxX = fst . maxB
maxY :: Bound -> Float
maxY = snd . maxB
minX :: Bound -> Float
minX = fst . minB
minY :: Bound -> Float
minY = snd . minB

overlap :: Bound -> Bound -> Maybe Bound
overlap b1 b2
  | minX b1 > maxX b2 || minY b1 > maxY b2 || minX b2 > maxX b1 || minY b2 > maxY b1 = Nothing 
  | otherwise = Just $ Bound (min (maxX b1) (maxX b2), min (maxY b1) (maxY b2)) (max (minX b1) (minX b2), max (minY b1) (minY b2))

boundOne :: Position -> Bound
boundOne p = Bound p p

bound :: [Position] -> Bound
bound [] = error "Can't bound nothing"
bound [v] = boundOne v
bound ((x,y):vs) = Bound (max mxX x, max mxY y) (min mnX x, min mnY y)
  where (Bound (mxX, mxY) (mnX, mnY)) = bound vs

bufferedBound :: [Position] -> Float -> Bound
bufferedBound vs b = Bound (maxB bnd |+ buff) (minB bnd |- buff)
  where
    bnd = bound vs
    buff = (b, b)

isIn :: Bound -> Position -> Bool
isIn b = isJust . overlap b . boundOne
