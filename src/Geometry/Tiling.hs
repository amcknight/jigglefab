module Geometry.Tiling
  ( Voronoi(..)
  , Arc(Arc)
  , Tri(Tri)
  , voronoi
  ) where
import Graphics.Gloss (Picture)
import Data.List (sortBy)
import Data.Maybe (catMaybes)
import Geometry.Vector
import Geometry.Angle
import Debug.Trace
import Color

type IxPos = (Position, Int)
type ColorPos = (Position, Color)

data Arc = Arc
  { cenPos :: Position
  , from :: Turn
  , to :: Turn
  , arcColor :: Color
  } deriving (Show, Eq)

data Tri = Tri
  { trip :: (Position, Position, Position)
  , triColor :: Color 
  } deriving (Show, Eq)

data Bouy = Bouy
  { pos :: Position
  , leftPos :: Maybe Position
  , rightPos :: Maybe Position
  , bouyColor :: Color
  } deriving (Show, Eq)

data Voronoi = Voronoi
  { arcs :: [Arc]
  , tris :: [Tri]
  } deriving (Show, Eq)

data Beach = Beach
  { verts :: [ColorPos] -- (Assumes sorted by Y)
  , circs :: [IxPos] -- Bouy Index
  , bouys :: [Bouy]
  , voron :: Voronoi
  }

voronoi :: [ColorPos] -> Voronoi
voronoi ps = voronoi' $ Beach ps [] [] $ Voronoi [] []
voronoi' :: Beach -> Voronoi
voronoi' (Beach [] [] bs v) = v { arcs = arcs v ++ fmap completeArc bs }
voronoi' b = voronoi' $ updateBeach b

completeArc :: Bouy -> Arc
completeArc (Bouy p Nothing _ c) = Arc p 0 1 c
completeArc (Bouy p _ Nothing c) = Arc p 0 1 c
completeArc (Bouy p (Just lp) (Just rp) i) = Arc p (direction (lp |- p)) (direction (rp |- p)) i

updateBeach :: Beach -> Beach
updateBeach b@(Beach [] [] _ v) = undefined -- shouldn't happen
updateBeach b@(Beach [] (c:cs) _ _) = removeBouy c $ b { circs = cs }
updateBeach b@(Beach (p:ps) [] _ _) = addBouy p $ b { verts = ps }
updateBeach b@(Beach (p@((_,py),_):ps) (c@((_,cy),_):cs) _ _) = case compare py cy of
  LT -> addBouy p $ b { verts = ps }
  _ -> removeBouy c $ b { circs = cs }

removeBouy :: IxPos -> Beach -> Beach
removeBouy c (Beach ps cs bs v) = case bs of
  [] -> undefined
  [_] -> undefined
  [_,_] -> undefined
  bs -> case (bs, c) of
    (_, (_,0)) -> undefined 
    ([_,_,_], (_,2)) -> undefined
    (lb:(b:(rb:bs)), (p,1)) -> Beach ps cs (lb{ rightPos = Just p } : rb{ leftPos = Just p } : bs) (v{ tris = tris v ++ newTris lb b rb p})
    (bs, (p,i)) -> let
      newB = removeBouy c $ Beach ps cs (drop (i-1) bs) v
      in newB { bouys = take (i-1) bs ++ bouys newB }

newTris :: Bouy -> Bouy -> Bouy -> Position -> [Tri]
newTris lb b rb p = catMaybes
  [ fmap (\p2 -> Tri (pos lb, p2, p) (bouyColor lb)) (rightPos lb)
  , fmap (\p2 -> Tri (pos  b, p2, p) (bouyColor  b)) (leftPos   b)
  , fmap (\p2 -> Tri (pos  b, p2, p) (bouyColor  b)) (rightPos  b)
  , fmap (\p2 -> Tri (pos rb, p2, p) (bouyColor rb)) (leftPos  rb)
  ]

addBouy :: ColorPos -> Beach -> Beach
addBouy (p,c) (Beach ps cs [] v) = Beach ps cs [Bouy p Nothing Nothing c] v
addBouy (p@(x,y),c) (Beach ps cs bs v) = Beach ps newCs newBs v
  where
    bi = findBouyI p bs
    newBs = case getBouy bi bs of
      Nothing -> undefined -- Shouldn't happen
      Just splitB -> take bi bs ++ [splitB, Bouy p Nothing Nothing c, splitB] ++ drop (bi+1) bs
    newCs = circleEvents y newBs

circleEvents :: Float -> [Bouy] -> [IxPos]
circleEvents sweep bs = sortBy (\((_,y1),_) ((_,y2),_) -> compare y1 y2) $ filter (\((_,y),_) -> y-1 < sweep) (circleEvents' bs 0)
circleEvents' :: [Bouy] -> Int -> [IxPos]
circleEvents' [] _ = []
circleEvents' [_] _ = []
circleEvents' [_,_] _ = []
circleEvents' [b1,b2,b3] si = [(centerFrom3 (pos b1) (pos b2) (pos b3), si+1) | b1 /= b3]
circleEvents' (b1:b2:b3:bs) si = (centerFrom3 (pos b1) (pos b2) (pos b3), si+1) : circleEvents' (b2:b3:bs) (si+1)

findBouyI :: Position -> [Bouy] -> Int
findBouyI _ [] = undefined -- Shouldn't happen
findBouyI _ [b] = 0
findBouyI p@(x,_) (b1:b2:bs) = case compare x x1 of
  GT -> case compare x x2 of
    LT -> case bouySide p p1 p2 of
      GT -> 1
      _ -> 0
    _ -> findBouyI p (b2:bs)
  _ -> 0
  where
    p1@(x1,_) = pos b1
    p2@(x2,_) = pos b2

bouySide :: Position -> Position -> Position -> Ordering
bouySide (x,y) (px,py) (qx,qy) = compare beachLeft beachRight
  where
    beachLeft  = ((x-px)^2 + py^2 - y^2) / (py-y)
    beachRight = ((x-qx)^2 + qy^2 - y^2) / (qy-y)

getBouy :: Int -> [Bouy] -> Maybe Bouy
getBouy _ [] = Nothing
getBouy i (b:bs) = case compare i 0 of
  LT -> Nothing
  EQ -> Just b
  GT -> getBouy (i-1) bs

centerFrom3 :: Position -> Position -> Position -> Position
centerFrom3 (x1,y1) (x2,y2) (x3,y3) = (-1/(2*a)) |* (b, c)
  where
    a = x1*(y2-y3) - y1*(x2-x3) + x2*y3 - x3*y2

    b =   (x1^2 + y1^2) * (y3-y2) 
        + (x2^2 + y2^2) * (y1-y3)
        + (x3^2 + y3^2) * (y2-y1)
 
    c =   (x1^2 + y1^2) * (x2-x3) 
        + (x2^2 + y2^2) * (x3-x1) 
        + (x3^2 + y3^2) * (x1-x2)

