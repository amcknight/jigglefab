module Geometry.Bound
( Bound
, maxX, maxY, minX, minY
, overlap
, boundOne
, bound, bufferedBound
) where
      
import Pair
import Geometry.Vector

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
