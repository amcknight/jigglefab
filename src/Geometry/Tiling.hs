module Geometry.Tiling
  ( Edge(..)
  , voronoi
  ) where
import Graphics.Gloss (Picture)
import Data.List (sortBy, sort)
import Data.Maybe (catMaybes)
import Geometry.Vector
import Geometry.Angle
import Debug.Trace
import Geometry.Line
import Pair
import Geometry.Bound
import Geometry.Space

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
voronoi' (Beach [] bs rs) = rs
voronoi' b = voronoi' $ updateBeach b

edgesFromRays :: Bound -> [Ray] -> [Edge]
edgesFromRays bnd rs = undefined

updateBeach :: Beach -> Beach
updateBeach (Beach [] _ _) = undefined -- shouldn't happen
updateBeach beach@(Beach (e:es) _ _) = case e of
  BouyEvent p -> addBouy p newBeach
  CrossEvent c -> processCross c newBeach
  where newBeach = beach { events = es }

processCross :: Cross -> Beach -> Beach
processCross c (Beach es bs v) = case bs of
  [] -> error "Crosspoint event with bouys is impossible"
  [_] -> error "Crosspoint event with 1 bouy is impossible"
  [_,_] -> error "Crosspoint event with 2 bouys is impossible"
  bs -> case (bs, c) of
    (_, Cross _ _ 0) -> undefined 
    ([_,_,_], Cross _ _ 2) -> undefined
    (lb:(b:(rb:bs)), Cross p _ 1) -> Beach es (lb:rb:bs) (v ++ newRays p lb b rb)
    (bs, Cross _ _ i) -> let
      newB = processCross c $ Beach es (drop (i-1) bs) v
      in newB { bouys = take (i-1) bs ++ bouys newB }

newRays :: Position -> Bouy -> Bouy -> Bouy -> [Ray]
newRays pos b1 b2 b3 = undefined

addBouy :: Bouy -> Beach -> Beach
addBouy b (Beach es [] rs) = Beach es [b] rs
addBouy (p@(x,y),i) (Beach es bs rs) = Beach newEs newBs rs
  where
    bi = findBouyI p bs
    newBs = case getBouy bi bs of
      Nothing -> error "Couldn't find the bouy we just found!?"
      Just splitB -> take bi bs ++ [splitB, (p, i), splitB] ++ drop (bi+1) bs
    newEs = sort $ es ++ circleEvents y newBs

circleEvents :: Float -> [Bouy] -> [Event]
circleEvents sweep bs = CrossEvent <$> filter (\(Cross (_,y) r _) -> y-r < sweep) (circleEvents' bs 0)
circleEvents' :: [Bouy] -> Int -> [Cross]
circleEvents' [] _ = []
circleEvents' [_] _ = []
circleEvents' [_,_] _ = []
circleEvents' [b1,b2,b3] si = [crossFrom3 b1 b2 b3 (si+1) | b1 /= b3]
circleEvents' (b1:b2:b3:bs) si = crossFrom3 b1 b2 b3 (si+1) : circleEvents' (b2:b3:bs) (si+1)

crossFrom3 :: Bouy -> Bouy -> Bouy -> Int -> Cross
crossFrom3 (p1,_) (p2,_) (p3,_) = Cross center rad
  where
    (center, rad) = circleFrom3 p1 p2 p3

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
