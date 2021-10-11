{-# LANGUAGE LambdaCase #-}
module Geometry.Voronoi
  ( Edge(..)
  , voronoi
  , parabolaCrossX
  , parabolaParams
  , parabolaCrossXs
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
import Data.Either (partitionEithers)

type IxPos = (Position, Int)
type Bouy = IxPos
data Cross = Cross Position Radius Int deriving (Show, Eq)
data Ray = Ray Position Turn Int Int deriving (Show, Eq)

instance Ord Ray where
  compare (Ray _ _ i1 j1) (Ray _ _ i2 j2) = case compare i1 i2 of
    EQ -> compare j1 j2
    o -> o

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
voronoi ps = edgesFromRays (bufferedBound ps 10) $ voronoi' $ Beach (sort (fmap BouyEvent (zip ps [0..]))) [] []
voronoi' :: Beach -> [Ray]
voronoi' b@(Beach [] _ rs) = rs
voronoi' b = voronoi' $ updateBeach b

edgesFromRays :: Bound -> [Ray] -> [Edge]
edgesFromRays bnd rs = pairs ++ fmap (edgeFromRay bnd) strays
  where (pairs, strays) = rayDups rs

rayDups :: [Ray] -> ([Edge], [Ray])
rayDups rs = partitionEithers $ addRayDups rs []

addRayDups :: [Ray] -> [Either Edge Ray] -> [Either Edge Ray]
addRayDups [] ers = ers
addRayDups [r] ers = Right r : ers
addRayDups (r1:r2:rs) ers = case edgeRay r1 r2 of
  Left e -> Left e : addRayDups rs ers
  Right r -> Right r : addRayDups (r2:rs) ers
  where
    edgeRay :: Ray -> Ray -> Either Edge Ray
    edgeRay r1@(Ray p1 _ i1 j1) (Ray p2 _ i2 j2) = if i1 == i2 && j1 == j2
      then Left $ Edge (Seg p1 p2) (i1, j1)
      else Right r1

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
updateBeach (Beach [] _ _) = error "updateBeach: No events"
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
    (lb:(b:(rb:bs)), 1) -> processCross' c (Beach es (lb:b:rb:bs) rs)
    (bs, _) -> let
      newB = processCross (Cross p r 1) $ Beach es (drop (i-1) bs) rs
      in newB { bouys = take (i-1) bs ++ bouys newB }

processCross' :: Cross -> Beach -> Beach
processCross' c@(Cross p _ _) (Beach es (lb@(lp,li):b:rb@(rp,ri):bs) rs)
  | crossContainsBouy c bs = Beach es (lb:b:rb:bs) rs
  | li == ri = Beach es (lb:bs) (rs ++ newRays p lb b rb)
  | otherwise = Beach es (lb:rb:bs) (rs ++ newRays p lb b rb)
processCross' _ _ = error "Previous conditions should have made this impossible"

crossContainsBouy :: Cross -> [Bouy] -> Bool
crossContainsBouy _ [] = False
crossContainsBouy c@(Cross cp rad _) ((bp,_):bs) = (distSq cp bp < rad^2) || crossContainsBouy c bs

newRays :: Position -> Bouy -> Bouy -> Bouy -> [Ray]
newRays pos (p1,i1) (p2,i2) (p3,i3) =
  [ if i2 < i3 then Ray pos (awayRay pos p1 p2 p3) i2 i3 else Ray pos (awayRay pos p1 p2 p3) i3 i2
  , if i1 < i3 then Ray pos (awayRay pos p2 p1 p3) i1 i3 else Ray pos (awayRay pos p2 p1 p3) i3 i1
  , if i1 < i2 then Ray pos (awayRay pos p3 p1 p2) i1 i2 else Ray pos (awayRay pos p3 p1 p2) i2 i1
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
findBouyI (px,sw) bs = findBouyI' px $ parabolaCrossXs sw $ fmap fst bs
findBouyI' :: Float -> [Float] -> Int
findBouyI' x [] = 0
findBouyI' x (cx:xs) = case compare x cx of
  LT -> 0
  EQ -> 0
  GT -> 1 + findBouyI' x xs

-- bouyY :: Position -> Position -> Float
-- bouyY (sx,sy) (bx,by) = ((sx-bx)^2 + by^2 - sy^2) / (by-sy)

getBouy :: Int -> [Bouy] -> Maybe Bouy
getBouy _ [] = Nothing
getBouy i (b:bs) = case compare i 0 of
  LT -> Nothing
  EQ -> Just b
  GT -> getBouy (i-1) bs

parabolaCrossXs :: Float -> [Position] -> [Float]
parabolaCrossXs _ [] = []
parabolaCrossXs _ [b] = []
parabolaCrossXs sw bs = zipWith (parabolaCrossX sw) bs (tail bs)

parabolaCrossX :: Float -> Position -> Position -> Float
parabolaCrossX sw p1@(px1,_) p2@(px2,_)
  | aDiff == 0 = 0.5*(px1+px2)
  | px1 < px2 = innerX
  | otherwise = outerX
  where
    (a1,b1,c1) = parabolaParams sw p1
    (a2,b2,c2) = parabolaParams sw p2
    aDiff = a2 - a1
    bDiff = b2 - b1
    cDiff = c2 - c1
    part = 0.5*bDiff/aDiff
    root = sqrt(part^2 - cDiff/aDiff)
    x1 = root - part
    x2 = - root - part
    -- y1 = a1*x1^2 + b1*x1 + c1
    -- y2 = a1*x2^2 + b1*x2 + c1
    isInner1 = between x1 px1 px2
    isInner2 = between x2 px1 px2
    (innerX, outerX) = case (isInner1, isInner2) of
      (True, True) -> error "Both are inner"
      (False, False) -> error "Both are outer"
      (True, False) -> (x1, x2)
      (False, True) -> (x2, x1)

between :: Float -> Float -> Float -> Bool 
between x a b
  | b < a = between x b a
  | otherwise = a <= x && x <= b

parabolaParams :: Float -> Position -> (Float, Float, Float)
parabolaParams sw (px,py) = (a,b,c)
  where
    a = 0.5/(py - sw)
    b = -2*px * a
    c = (px^2 + py^2 - sw^2) * a
