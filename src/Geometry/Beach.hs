{-# LANGUAGE LambdaCase #-}
module Geometry.Beach
( Beach(..)
, Bouy(..)
, Cross(..)
, Event(..)
, Ray(..)
, initialBeach
, updateBeach
, processBeach
, parabolaCrossXs
, parabolaCross
, height
, newRays
, awayRay
) where

import qualified Data.Vector as V
import Data.List (sort, partition)
import Data.Maybe (mapMaybe)
import Geometry.Vector
import Geometry.Space
import Geometry.Angle
import Geometry.CrossPoint
import Geometry.Parabola
import Debug.Trace

data Bouy = Bouy
  { bouyPos :: Position
  , bouyI :: Int
  } deriving Eq

instance Show Bouy where
  show (Bouy pos i) = "Bouy "++show pos++" i"++show i

data Cross = Cross
  { crossPos :: Position
  , crossRad :: Radius
  , crossI :: Int
   } deriving Eq

instance Show Cross where
  show (Cross pos rad i) = "Cross "++show pos++" r"++show rad++" i"++show i

data Event = BouyEvent Bouy | CrossEvent Cross deriving Eq

instance Show Event where
  show (BouyEvent e) = "Event"++show e
  show (CrossEvent e) = "Event"++show e

height :: Event -> Float
height (BouyEvent (Bouy (_,y) _)) = y
height (CrossEvent (Cross (_,y) r _)) = y - r

instance Ord Event where
  compare e1 e2 = compare (height e2) (height e1)

data Ray = Ray Position Turn Int Int deriving (Show, Eq)

instance Ord Ray where
  compare (Ray _ _ i1 j1) (Ray _ _ i2 j2) = case compare i1 i2 of
    EQ -> compare j1 j2
    o -> o

data Beach = Beach
  { sweep :: Float 
  , crossStack :: [(Int, Int, Int)]
  , events :: [Event]
  , bouys :: V.Vector Bouy
  , rays :: [Ray]
  }

instance Show Beach where
  show (Beach sw cs es bs rs) = "Beach {s="++show sw++" stack="++show cs++" events="++show es++" bouys="++show bs++" rays="++show rs++"}"

isBouyEvent :: Event -> Bool
isBouyEvent (BouyEvent _) = True
isBouyEvent _ = False

bouyEvents :: Beach -> [Event]
bouyEvents (Beach _ _ es _ _) = filter isBouyEvent es
crossEvents :: Beach -> [Event]
crossEvents (Beach _ _ es _ _) = filter (not . isBouyEvent) es

initialBeach :: [Position] -> Beach
initialBeach ps = Beach sw [] es V.empty []
  where
    sw = height (head es) + 1
    es = sort $ BouyEvent <$> zipWith Bouy ps [0..]

updateBeach :: Beach -> Beach
updateBeach (Beach _ _ [] _ _) = error "updateBeach: No events"
updateBeach beach@(Beach sw cs (e:es) _ _)
  | h > sw = error "Somehow the event is occurring above the sweep line"
  | otherwise = trace (show beach) $ case e of
    BouyEvent p -> processBouy p newBeach
    CrossEvent c -> processCross c newBeach
  where
    h = height e
    newBeach = if h == sw
      then beach { events = es }
      else beach { sweep = h, crossStack = [], events = es }

processBeach :: Beach -> Int -> Beach
processBeach b = (iterate updateBeach b !!)

processCross :: Cross -> Beach -> Beach
processCross c@(Cross p r i) b@(Beach sw cs es bs rs)
  | numBs < 3 = error $ "Crosspoint event with "++show numBs++" bouys is impossible"
  | i < 1 = error "Bouy index should never be the left-most bouy (or out of bounds)"
  | i >= numBs - 1 = error "Bouy index should never be the right-most bouy (or out of bounds)"
  | bouyI (bs V.! (i-1)) == bouyI (bs V.! (i+1)) = error "A circle event had left and right indices equal. Impossible"
  | bis `elem` cs = trace "BIS" $ Beach sw cs newEs newBs rs -- No new rays. TODO: Should it not use the newBS?
  | crossContainsBouy c bs = trace "CONTAINS" $ Beach sw newCs newEs newBs rs -- Don't add Rays. Just drop event.
  | otherwise = trace "OTHERWISE" $ Beach sw newCs newEs newBs newRs
  where
    numBs = length bs
    leftBouy =  bs V.! (i-1)
    midBouy =   bs V.! i
    rightBouy = bs V.! (i+1)
    bis = sort3 (bouyI leftBouy) (bouyI midBouy) (bouyI rightBouy)
     -- 0123210
     -- 013210
    newCs = bis : cs
    newBs = V.take i bs <> V.drop (i+1) bs
    newEs = sort $ shiftCrosses i (-1) (removeBrokenCircleEvent (removeBrokenCircleEvent es (i+1)) (i-1)) ++ newCircleEventsAt newBs [i-1, i]
    newRs = rs ++ newRays p leftBouy midBouy rightBouy

shiftCrosses :: Int -> Int -> [Event] -> [Event]
shiftCrosses i by = fmap (\case CrossEvent c -> CrossEvent (shiftCross c); e -> e)
  where
    shiftCross :: Cross -> Cross
    shiftCross (Cross p r ci) = if ci >= i
      then Cross p r $ ci + by
      else Cross p r ci

crossContainsBouy :: Cross -> V.Vector Bouy -> Bool
crossContainsBouy c@(Cross cp rad i) bs = any (\(Bouy p j) -> j /= li && j /= mi && j /= ri && distSq cp p < rad^2) bs
  where
    li = bouyI $ bs V.! (i-1)
    mi = bouyI $ bs V.! i
    ri = bouyI $ bs V.! (i+1)

newRays :: Position -> Bouy -> Bouy -> Bouy -> [Ray]
newRays pos (Bouy p1 i1) (Bouy p2 i2) (Bouy p3 i3) =
  [ if i2 < i3 then Ray pos away1 i2 i3 else Ray pos away1 i3 i2
  , if i1 < i3 then Ray pos away2 i1 i3 else Ray pos away2 i3 i1
  , if i1 < i2 then Ray pos away3 i1 i2 else Ray pos away3 i2 i1
  ]
  where
    away1 = awayRay pos p1 p2 p3
    away2 = awayRay pos p2 p1 p3
    away3 = awayRay pos p3 p1 p2

awayRay :: Position -> Position -> Position -> Position -> Turn
awayRay o away p q = if turnDirection p q o == turnDirection p q away
  then dir
  else pole dir
-- awayRay o away p q = case separation dir adir of 
--   Opposite -> dir
--   Obtuse -> dir
--   _ -> pole dir
  where
    dir = direction $ mid p q |- o
--     adir = direction $ away |- o

processBouy :: Bouy -> Beach -> Beach
processBouy b bch@(Beach sw ss es bs rs)
  | V.null bs = Beach sw ss es (V.fromList [b]) rs
  | otherwise = Beach sw ss newEs newBs rs
  where
    bi = findBouyI (bouyPos b) bs
    dupB = bs V.! bi
    newBs = V.take bi bs <> V.fromList [dupB, b, dupB] <> V.drop (bi+1) bs
    newEs = sort $ shiftCrosses bi 2 (removeBrokenCircleEvent es bi) ++ newCircleEventsAt newBs [bi, bi+2] -- Could do this without re-sorting for better performance

removeBrokenCircleEvent :: [Event] -> Int -> [Event]
removeBrokenCircleEvent es bi = filter (\case CrossEvent (Cross _ _ ci) -> ci /= bi; _ -> True ) es

newCircleEventsAt :: V.Vector Bouy -> [Int] -> [Event]
newCircleEventsAt bs is = mapMaybe (fmap CrossEvent . crossFrom3 bs) (filter notOnEdge is)
  where notOnEdge i = i > 0 && i < length bs - 1

crossFrom3 :: V.Vector Bouy -> Int -> Maybe Cross
crossFrom3 bs bi =
  if i1 == i2 || i1 == i3 || i2 == i3
  then Nothing --not 3 different bouys
  else case turnDirection p1 p2 p3 of
    Nothing -> Nothing -- Colinear
    Just Clockwise -> case circleFrom3 p1 p2 p3 of
      Nothing -> Nothing --colinear points
      Just (center, rad) ->
        if crossContainsBouy (Cross center rad bi) bs
        then Nothing -- Contains other bouy
        else Just $ Cross center rad bi
    Just CounterClockwise -> Nothing
  where
    Bouy p1 i1 = bs V.! (bi-1)
    Bouy p2 i2 = bs V.! bi
    Bouy p3 i3 = bs V.! (bi+1)
      
findBouyI :: Position -> V.Vector Bouy -> Int
findBouyI (px,py) bs
  | V.null bs = error "Searching for bouy in empty bouy list"
  | otherwise = findBouyI' px $ parabolaCrossXs py $ V.toList $ fmap bouyPos bs
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
