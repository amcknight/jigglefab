module Geometry.Tiling
  ( Voronoi(..)
  , Arc(Arc)
  , Tri(Tri)
  , voronoi
  ) where
import Graphics.Gloss (Picture)
import Data.List (sortBy, sort)
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
  } deriving Show

data Tri = Tri
  { trip :: (Position, Position, Position)
  , triColor :: Color 
  } deriving Show

data Bouy = Bouy
  { pos :: Position
  , leftPos :: Maybe Position
  , rightPos :: Maybe Position
  , bouyColor :: Color
  } deriving (Show, Eq)

data Voronoi = Voronoi
  { arcs :: [Arc]
  , tris :: [Tri]
  } deriving Show

data Beach = Beach
  { events :: [Event]
  , bouys :: [Bouy]
  , voron :: Voronoi
  } deriving Show

data Event = NewBouy ColorPos | Cross IxPos | CloseBouy IxPos deriving (Show, Eq)
height :: Event -> Float 
height (NewBouy ((_,y), _)) = y
height (Cross ((_,y), _)) = y
height (CloseBouy ((_,y), _)) = y

instance Ord Event where
  compare e1 e2 = compare (height e2) (height e1)

voronoi :: [ColorPos] -> Voronoi
voronoi ps = voronoi' $ Beach (sort (fmap NewBouy ps)) [] $ Voronoi [] []
voronoi' :: Beach -> Voronoi
voronoi' (Beach [] bs v) = v
voronoi' b = voronoi' $ updateBeach b

completeArc :: Bouy -> Arc
completeArc (Bouy p Nothing _ c) = Arc p 0 1 c
completeArc (Bouy p _ Nothing c) = Arc p 0 1 c
completeArc (Bouy p (Just lp) (Just rp) i) = Arc p (direction (lp |- p)) (direction (rp |- p)) i

updateBeach :: Beach -> Beach
updateBeach (Beach [] _ _) = undefined -- shouldn't happen
updateBeach beach@(Beach (e:es) _ _) = case e of
  NewBouy p -> addBouy p newBeach
  Cross c -> removeBouy c newBeach
  CloseBouy c -> closeBouy c newBeach
  where newBeach = beach { events = es }

closeBouy :: IxPos -> Beach -> Beach
closeBouy (p,i) (Beach es bs v) = case getBouy i bs of
  Nothing -> undefined -- Shouldn't happen
  Just b -> Beach es (take i bs ++ drop (i+1) bs) (Voronoi (arcs v ++ [completeArc b]) (tris v))

removeBouy :: IxPos -> Beach -> Beach
removeBouy c (Beach es bs v) = case bs of
  [] -> undefined
  [_] -> undefined
  [_,_] -> undefined
  bs -> case (bs, c) of
    (_, (_,0)) -> undefined 
    ([_,_,_], (_,2)) -> undefined
    (lb:(b:(rb:bs)), (p,1)) -> Beach es (lb{ rightPos = Just p } : rb{ leftPos = Just p } : bs) (v{ tris = tris v ++ newTris lb b rb p})
    (bs, (p,i)) -> let
      newB = removeBouy c $ Beach es (drop (i-1) bs) v
      in newB { bouys = take (i-1) bs ++ bouys newB }

newTris :: Bouy -> Bouy -> Bouy -> Position -> [Tri]
newTris lb b rb p = catMaybes
  [ fmap (\p2 -> Tri (pos lb, p2, p) (bouyColor lb)) (rightPos lb)
  , fmap (\p2 -> Tri (pos  b, p2, p) (bouyColor  b)) (leftPos   b)
  , fmap (\p2 -> Tri (pos  b, p2, p) (bouyColor  b)) (rightPos  b)
  , fmap (\p2 -> Tri (pos rb, p2, p) (bouyColor rb)) (leftPos  rb)
  ]

addBouy :: ColorPos -> Beach -> Beach
addBouy (p,c) (Beach es [] v) = Beach (sort (es ++ [CloseBouy (p |+ downV, 0)])) [Bouy p Nothing Nothing c] v
addBouy (p@(x,y),c) (Beach es bs v) = Beach newEs newBs v
  where
    bi = findBouyI p bs
    newBs = case getBouy bi bs of
      Nothing -> undefined -- Shouldn't happen
      Just splitB -> take bi bs ++ [splitB, Bouy p Nothing Nothing c, splitB] ++ drop (bi+1) bs
    newEs = sort $ es ++ circleEvents y newBs ++ [CloseBouy (p |+ downV, 0)]

circleEvents :: Float -> [Bouy] -> [Event]
circleEvents sweep bs = Cross <$> filter (\((_,y),_) -> y-1 < sweep) (circleEvents' bs 0)
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

