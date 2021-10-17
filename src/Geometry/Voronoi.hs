{-# LANGUAGE LambdaCase #-}
module Geometry.Voronoi
  ( Edge(..)
  , Beach(..)
  , Event(..)
  , Cross(..), Bouy(..)
  , voronoi
  , initialBeach
  , processBeach
  , parabolaCrossXs
  , parabolaCross
  , height
  ) where
import Graphics.Gloss (Picture)
import Data.List (sortBy, sort)
import Data.Maybe (catMaybes)
import Data.Fixed (mod')
import Data.Either (partitionEithers)
import Geometry.Vector
import Debug.Trace
import Geometry.Line
import Pair
import Geometry.Bound
import Geometry.Space
import Geometry.Angle
import Geometry.Parabola
import Geometry.CrossPoint
import qualified Data.Vector as V

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
  { sweep :: Float 
  , events :: [Event]
  , bouys :: V.Vector Bouy
  , rays :: [Ray]
  } deriving Show

data Event = BouyEvent Bouy | CrossEvent Cross deriving (Show, Eq)
height :: Event -> Float
height (BouyEvent ((_,y), _)) = y
height (CrossEvent (Cross (_,y) r _)) = y - r

instance Ord Event where
  compare e1 e2 = compare (height e2) (height e1)

voronoi :: [Position] -> [Edge]
voronoi ps = edgesFromRays (bufferedBound ps 1) $ voronoi' $ initialBeach ps
voronoi' :: Beach -> [Ray]
voronoi' b@(Beach _ [] _ rs) = rs
voronoi' b = voronoi' $ updateBeach b

initialBeach :: [Position] -> Beach
initialBeach ps = Beach sw es V.empty []
  where
    sw = height (head es) + 1
    es = sort $ BouyEvent <$> zip ps [0..]

processBeach :: Beach -> Int -> Beach
processBeach b = (iterate updateBeach b !!)

edgesFromRays :: Bound -> [Ray] -> [Edge]
edgesFromRays bnd rs = pairs ++ fmap (edgeFromRay bnd) strays
  where (pairs, strays) = rayDups rs

rayDups :: [Ray] -> ([Edge], [Ray])
rayDups rs = partitionEithers $ addRayDups (sort rs) []

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
updateBeach (Beach _ [] _ _) = error "updateBeach: No events"
updateBeach beach@(Beach sw (e:es) _ _) = if height e > sw
  then error "Somehow the event is occurring above the sweep line"
  else case e of
    BouyEvent p -> trace (show p ++ ", " ++ show newBeach) $ processBouy p newBeach
    CrossEvent c -> trace (show c ++ ", " ++ show newBeach) $ processCross c newBeach
  where newBeach = beach { sweep = height e, events = es }

processCross :: Cross -> Beach -> Beach
processCross c@(Cross p r i) b@(Beach sw es bs rs)
  | numBs < 3 = error ("Crosspoint event with "++show numBs++" bouys is impossible")
  | i == 0 = error "Bouy index should never be the left-most bouy"
  | i == numBs - 1 = error "Bouy index should never be the right-most bouy"
  | i >= numBs = error "Index out of bounds in processCross"
  | snd (bs V.! (i-1)) == snd (bs V.! (i+1)) = error "A circle event had left and right indices equal. Impossible"
  | crossContainsBouy c bs = b { events = newEs, bouys = newBs } -- Don't add Rays. Just drop event.
  | otherwise = b { events = newEs, bouys = newBs, rays = rs ++ newRays p (bs V.! (i-1)) (bs V.! i) (bs V.! (i+1)) }
  where
    newBs = remove bs i
    newEs = replenishCircleEvents sw newBs es
    numBs = length bs
    remove :: V.Vector Bouy -> Int -> V.Vector Bouy
    remove bs i = V.take i bs <> V.drop (i+1) bs

crossContainsBouy :: Cross -> V.Vector Bouy -> Bool
crossContainsBouy c@(Cross cp rad i) bs = any (\(p, j) -> j /= li && j /= mi && j /= ri && distSq cp p < rad^2) bs
  where
    (_, li) = bs V.! (i-1)
    (_, mi) = bs V.! i
    (_, ri) = bs V.! (i+1)

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
processBouy b@(p@(x,y),i) bch@(Beach sw es bs rs)
  | V.null bs = Beach sw es (V.fromList [b]) rs
  | otherwise = Beach sw newEs newBs rs
  where
    bi = findBouyI p bs
    splitB = bs V.! bi
    newBs = V.take bi bs <> V.fromList [splitB, (p, i), splitB] <> V.drop (bi+1) bs
    newEs = replenishCircleEvents y newBs es

replenishCircleEvents :: Float -> V.Vector Bouy -> [Event] -> [Event]
replenishCircleEvents sw bs es = sort $ filter (\case {BouyEvent{} -> True; _ -> False}) es ++ circleEvents sw bs

circleEvents :: Float -> V.Vector Bouy -> [Event]
circleEvents sweep bs = CrossEvent <$> filter (\(Cross (_,y) r _) -> y-r <= sweep) (circleEvents' (V.toList bs) 0)
circleEvents' :: [Bouy] -> Int -> [Cross]
circleEvents' [] _ = []
circleEvents' [_] _ = []
circleEvents' [_,_] _ = []
circleEvents' [b1,b2,b3] bi = case clockwiseCrossFrom3 b1 b2 b3 (bi+1) of { Nothing -> []; Just cr -> [cr] }
circleEvents' (b1:b2:b3:bs) bi = case clockwiseCrossFrom3 b1 b2 b3 (bi+1) of { Nothing -> []; Just cr -> [cr] } ++ circleEvents' (b2:b3:bs) (bi+1)

clockwiseCrossFrom3 :: Bouy -> Bouy -> Bouy -> Int -> Maybe Cross
clockwiseCrossFrom3 (p1,i1) (p2,i2) (p3,i3) bi =
  if i1 == i2 || i1 == i3 || i2 == i3
  then Nothing --not 3 different bouys
  else case turnDirection p1 p2 p3 of
    Nothing -> Nothing -- Colinear
    Just Clockwise -> case circleFrom3 p1 p2 p3 of
      Nothing -> Nothing --colinear points
      Just (center, rad) -> Just $ Cross center rad bi
    Just CounterClockwise -> case circleFrom3 p1 p2 p3 of
        Nothing -> Nothing --colinear points
        Just (center, rad) -> Just $ Cross center rad bi
      
findBouyI :: Position -> V.Vector Bouy -> Int
findBouyI (px,py) bs
  | V.null bs = error "Searching for bouy in empty bouy list"
  | otherwise = findBouyI' px $ parabolaCrossXs py $ V.toList $ fmap fst bs
findBouyI' :: Float -> [Float] -> Int
findBouyI' x [] = 0
findBouyI' x (cx:xs) = case compare x cx of
  LT -> 0
  EQ -> 0
  GT -> 1 + findBouyI' x xs

parabolaCrossXs :: Float -> [Position] -> [Float]
parabolaCrossXs _ [] = []
parabolaCrossXs _ [b] = []
parabolaCrossXs sw bs = zipWith (parabolaCrossX sw) bs (tail bs)

parabolaCrossX :: Float -> Position -> Position -> Float
parabolaCrossX sw p q = fst $ parabolaCross sw p q

parabolaCross :: Float -> Position -> Position -> Position
parabolaCross sw p q = case crossPointsFromFoci sw p q of
  NoCross -> error "All Bouy parabolas should have at least one cross point"
  OneCross c -> c
  TwoCross lc rc -> case compare (snd p) (snd q) of
    LT -> rc
    EQ -> error "Shouldn't happen I guess"
    GT -> lc
  AllCross -> error "Identical Bouys should not be in a voronoi"

between :: Float -> Float -> Float -> Bool
between x a b
  | b < a = between x b a
  | otherwise = a <= x && x <= b

