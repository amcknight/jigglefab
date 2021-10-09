{-# LANGUAGE LambdaCase #-}
module Geometry.Voronoi
  ( Edge(..)
  , voronoi
  ) where
import Graphics.Gloss (Picture)
import Data.List (sortBy, sort)
import Data.Maybe (catMaybes)
import Geometry.Vector
import Debug.Trace
import Geometry.Line
import Pair
import Geometry.Bound
import Geometry.Space
import Geometry.Angle
import Data.Fixed (mod')

type IxPos = (Position, Int)
type Bouy = IxPos
data Cross = Cross Position Radius Int deriving (Show, Eq)
data Ray = Ray Position Turn Int Int deriving Show

pos :: IxPos -> Position
pos = fst

data Edge = Edge
  { seg :: Seg
  , edgeI :: P Int
  } deriving Show

data Beach = Beach
  { events :: [Event]
  , bouys :: [Bouy]
  , rays :: [Ray]
  } deriving Show

data Event = BouyEvent Bouy | CrossEvent Cross deriving (Show, Eq)
height :: Event -> Float 
height (BouyEvent ((_,y), _)) = y
height (CrossEvent (Cross (_,y) r _)) = y-r

instance Ord Event where
  compare e1 e2 = compare (height e2) (height e1)

voronoi :: [Position] -> [Edge]
voronoi ps = edgesFromRays (bufferedBound ps 1) $ voronoi' $ Beach (sort (fmap BouyEvent (zip ps [0..]))) [] []
voronoi' :: Beach -> [Ray]
voronoi' (Beach [] _ rs) = rs
voronoi' b = voronoi' $ updateBeach b

edgesFromRays :: Bound -> [Ray] -> [Edge]
edgesFromRays bnd = fmap (edgeFromRay bnd)

edgeFromRay :: Bound -> Ray -> Edge
edgeFromRay b (Ray p dir i j) = Edge (Seg p (rayCrossBound b p (simple dir))) (i,j)

rayCrossBound :: Bound -> Position -> Turn -> Position
rayCrossBound ((mxX,mxY),(mnX,mnY)) p@(x,y) dir
  | dir == 0 || dir == 1 = (mxX, y)
  | dir == 0.25 = (x, mxY)
  | dir == 0.50 = (mnX, y)
  | dir == 0.75 = (x, mnY)
  | dir > 0.75 = closer (mxX, yForMaxX) (xForMinY, mnY)
  | dir > 0.50 = closer (mnX, yForMinX) (xForMinY, mnY)
  | dir > 0.25 = closer (mnX, yForMinX) (xForMaxY, mxY)
  | dir > 0.00 = closer (mxX, yForMaxX) (xForMaxY, mxY)
  | otherwise = error "Invalid direction"
  where
    s = slope dir
    b = y - s * x
    yForMinX = s * mnX + b
    xForMinY = (mnY-b) / s
    yForMaxX = s * mxX + b
    xForMaxY = (mxY-b) / s

    closer :: Position -> Position -> Position
    closer a b = if distSq p a < distSq p b then a else b


updateBeach :: Beach -> Beach
updateBeach (Beach [] _ _) = error "Update beach should not be called with no events"
updateBeach beach@(Beach (e:es) _ _) = case e of
  BouyEvent p -> processBouy p newBeach
  CrossEvent c -> processCross c newBeach
  where newBeach = beach { events = es }

processCross :: Cross -> Beach -> Beach
processCross c@(Cross p r i) (Beach es bs rs) = case bs of
  [] -> error "Crosspoint event with 0 bouys is impossible"
  [_] -> error "Crosspoint event with 1 bouy is impossible"
  [_,_] -> error "Crosspoint event with 2 bouys is impossible"
  bs -> case (bs, i) of
    (_, 0) -> error "Bouy index should never be the left-most bouy"
    ([_,_,_], 2) -> error "Bouy index should never be the right-most bouy"
    (lb:(b:(rb:bs)), 1) -> Beach es (lb:rb:bs) (rs ++ newRays p lb b rb)
    (bs, _) -> let
      newB = processCross (Cross p r 1) $ Beach es (drop (i-1) bs) rs
      in newB { bouys = take (i-1) bs ++ bouys newB }

newRays :: Position -> Bouy -> Bouy -> Bouy -> [Ray]
newRays pos (p1,i1) (p2,i2) (p3,i3) =
  [ Ray pos (awayRay pos p1 p2 p3) i2 i3
  , Ray pos (awayRay pos p2 p1 p3) i1 i3
  , Ray pos (awayRay pos p3 p1 p2) i1 i2
  ]

awayRay :: Position -> Position -> Position -> Position -> Turn
awayRay o away p q = case separation dir adir of 
  Opposite -> dir
  Obtuse -> dir
  _ -> pole dir
  where
    dir = direction (mid |- o)
    adir = direction (away |- o)
    mid = 0.5 |* (p |+ q)

processBouy :: Bouy -> Beach -> Beach
processBouy b (Beach es [] rs) = Beach es [b] rs
processBouy (p@(x,y),i) (Beach es bs rs) = Beach newEs newBs rs
  where
    bi = findBouyI p bs
    newBs = case getBouy bi bs of
      Nothing -> error "Couldn't find the bouy we just found!?"
      Just splitB -> take bi bs ++ [splitB, (p, i), splitB] ++ drop (bi+1) bs
    newEs = sort $ filter (\case {BouyEvent{} -> True; _ -> False}) es ++ circleEvents y newBs

circleEvents :: Float -> [Bouy] -> [Event]
circleEvents sweep bs = CrossEvent <$> filter (\(Cross (_,y) r _) -> y-r < sweep) (circleEvents' bs 0)
circleEvents' :: [Bouy] -> Int -> [Cross]
circleEvents' [] _ = []
circleEvents' [_] _ = []
circleEvents' [_,_] _ = []
circleEvents' [b1,b2,b3] bi = case crossFrom3 b1 b2 b3 (bi+1) of { Nothing -> []; Just cr -> [cr] }
circleEvents' (b1:b2:b3:bs) bi = case crossFrom3 b1 b2 b3 (bi+1) of { Nothing -> []; Just cr -> [cr] } ++ circleEvents' (b2:b3:bs) (bi+1)

crossFrom3 :: Bouy -> Bouy -> Bouy -> Int -> Maybe Cross
crossFrom3 (p1,i1) (p2,i2) (p3,i3) bi =
  if i1 == i2 || i1 == i3 || i2 == i3
  then Nothing --not 3 different bouys
  else case circleFrom3 p1 p2 p3 of
    Nothing -> Nothing --colinear points
    Just (center, rad) -> Just $ Cross center rad bi

findBouyI :: Position -> [Bouy] -> Int
findBouyI _ [] = error "Searching for bouy in empty bouy list"
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
bouySide p a b = compare (bouyY p a) (bouyY p b)

bouyY :: Position -> Position -> Float
bouyY (sx,sy) (bx,by) = ((sx-bx)^2 + by^2 - sy^2) / (by-sy)

getBouy :: Int -> [Bouy] -> Maybe Bouy
getBouy _ [] = Nothing
getBouy i (b:bs) = case compare i 0 of
  LT -> Nothing
  EQ -> Just b
  GT -> getBouy (i-1) bs
