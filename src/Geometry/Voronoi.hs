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
edgesFromRays bnd rs = trace (show rs) undefined

updateBeach :: Beach -> Beach
updateBeach (Beach [] _ _) = error "Update beach should not be called with no events"
updateBeach beach@(Beach (e:es) _ _) = trace (show beach) $ case e of
  BouyEvent p -> processBouy p newBeach
  CrossEvent c -> processCross c newBeach
  where newBeach = beach { events = es }

processCross :: Cross -> Beach -> Beach
processCross c (Beach es bs rs) = case bs of
  [] -> error "Crosspoint event with 0 bouys is impossible"
  [_] -> error "Crosspoint event with 1 bouy is impossible"
  [_,_] -> error "Crosspoint event with 2 bouys is impossible"
  bs -> case (bs, c) of
    (_, Cross p _ 0) -> error "Bouy index should never be the left-most bouy"
    ([_,_,_], Cross p _ 2) -> error "Bouy index should never be the right-most bouy"
    (lb:(b:(rb:bs)), Cross p _ 1) -> Beach es (lb:rb:bs) (rs ++ newRays p lb b rb)
    (bs, Cross _ _ i) -> let
      newB = processCross c $ Beach es (drop (i-1) bs) rs
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
    newEs = sort $ es ++ circleEvents y newBs

circleEvents :: Float -> [Bouy] -> [Event]
circleEvents sweep bs = CrossEvent <$> filter (\(Cross (_,y) r _) -> y-r < sweep) (circleEvents' bs 0)
circleEvents' :: [Bouy] -> Int -> [Cross]
circleEvents' [] _ = []
circleEvents' [_] _ = []
circleEvents' [_,_] _ = []
circleEvents' [b1,b2,b3] bi = case crossFrom3 b1 b2 b3 (bi+1) of { Nothing -> []; Just cr -> [cr] }
circleEvents' (b1:b2:b3:bs) bi = case crossFrom3 b1 b2 b3 (bi+1) of { Nothing -> []; Just cr -> [cr] } ++ circleEvents' (b2:b3:bs) (bi+1)

crossFrom3 :: Bouy -> Bouy -> Bouy -> Int -> Maybe Cross
crossFrom3 (p1,_) (p2,_) (p3,_) bi = case circleFrom3 p1 p2 p3 of 
  Nothing -> Nothing
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
