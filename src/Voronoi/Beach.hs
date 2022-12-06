{-# LANGUAGE LambdaCase #-}
module Voronoi.Beach
( Beach(..)
, initialBeach
, updateBeach
, processBeach
, parabolaCrossXs
, parabolaCross
, newRays
) where

import qualified Data.Vector as V
import Data.List (sort, sortOn)
import Data.Maybe (mapMaybe)
import Geometry.Vector
import Geometry.Angle
import Geometry.CrossPoint
import Geometry.Parabola
import Debug.Trace
import Geometry.Circle
import Voronoi.Edge
import Util.Pair
import Util.Utils
import Voronoi.Event

data Beach = Beach
  { sweep :: Double 
  , crossStack :: [(Int, Int, Int)]
  , events :: [Event]
  , bouys :: V.Vector Bouy
  , rays :: [Ray]
  }

instance Show Beach where
  show (Beach sw cs es bs rs) = "Beach {s="++show sw++" stack="++show cs++" events="++show es++" bouys="++show bs++" rays="++show rs++"}"

initialBeach :: [Position] -> Beach
initialBeach ps = Beach sw [] es V.empty []
  where
    sw = height (head es) + 1
    es = sort $ BouyEvent <$> zipWith Bouy ps [0..]

processBeach :: Beach -> Int -> Beach
processBeach b = (iterate updateBeach b !!)

updateBeach :: Beach -> Beach
updateBeach (Beach _ _ [] _ _) = error "updateBeach: No events"
updateBeach beach@(Beach sw _ (e:es) _ _) = case compare h sw of
  LT -> processEvent e $ beach { sweep = h, crossStack = [], events = es }
  EQ -> processEvent e $ beach { events = es }
  GT -> trace ("Warning: updateBeach: Somehow the event is occurring "++show (h-sw)++" above the sweep line") $ processEvent e $ beach { events = es }
  where h = height e

processEvent :: Event -> Beach -> Beach
processEvent (BouyEvent p) = processBouy p
processEvent (CrossEvent c) = processCross c

processCross :: Cross -> Beach -> Beach
processCross cr@(Cross c i) (Beach sw cs es bs rs)
  | numBs < 3 = error $ "Crosspoint event with "++show numBs++" bouys is impossible"
  | i < 1 = error "Bouy index should never be the left-most bouy (or out of bounds)"
  | i >= numBs - 1 = error "Bouy index should never be the right-most bouy (or out of bounds)"
  | bouyI (bs V.! (i-1)) == bouyI (bs V.! (i+1)) = error "A circle event had left and right indices equal. Impossible"
  | bis `elem` cs = Beach sw cs newEs newBs rs -- No new rays
  | crossContainsBouy cr bs = Beach sw newCs newEs newBs rs -- No new rays
  | otherwise = Beach sw newCs newEs newBs newRs
  where
    numBs = length bs
    leftBouy =  bs V.! (i-1)
    midBouy =   bs V.! i
    rightBouy = bs V.! (i+1)
    bis = sort3 (bouyI leftBouy) (bouyI midBouy) (bouyI rightBouy)

    newCs = bis : cs
    newBs = V.take i bs <> V.drop (i+1) bs
    adjustedEs = shiftCrosses i (-1) . removeBrokenCircleEvent (i-1) . removeBrokenCircleEvent (i+1)
    newEs = sort $ adjustedEs es ++ newCircleEventsAt newBs [i-1, i]
    newRs = rs ++ newRays (circPos c) leftBouy midBouy rightBouy

shiftCrosses :: Int -> Int -> [Event] -> [Event]
shiftCrosses i by = fmap (\case CrossEvent c -> CrossEvent (shiftCross c); e -> e)
  where
    shiftCross :: Cross -> Cross
    shiftCross (Cross c ci) = Cross c $ if ci >= i then ci + by else ci

crossContainsBouy :: Cross -> V.Vector Bouy -> Bool
crossContainsBouy (Cross (Circle cp r) i) bs = any (\(Bouy p j) -> j /= li && j /= mi && j /= ri && distSq cp p < r*r) bs
  where
    li = bouyI $ bs V.! (i-1)
    mi = bouyI $ bs V.! i
    ri = bouyI $ bs V.! (i+1)

newRays :: Position -> Bouy -> Bouy -> Bouy -> [Ray]
newRays pos (Bouy p1 i1) (Bouy p2 i2) (Bouy p3 i3) =
  [ Ray pos (awayRay pos p1 p2 p3) (sortP (i2,i3))
  , Ray pos (awayRay pos p2 p1 p3) (sortP (i1,i3))
  , Ray pos (awayRay pos p3 p1 p2) (sortP (i1,i2))
  ]

processBouy :: Bouy -> Beach -> Beach
processBouy b (Beach sw ss es bs rs)
  | V.null bs = Beach sw ss es (V.fromList [b]) rs
  | sameH = case compare h bouysH of
    LT -> Beach sw ss newEsOnStart newBs rs
    EQ -> Beach sw ss es (V.fromList (sortOn (fst . pos) (V.toList bs ++ [b]))) rs
    GT -> error "processBouy: Height of new bouy is above the beach bouys"
  | otherwise = Beach sw ss newEs newBs rs
  where
    h = snd $ pos b
    sameH = allEq $ V.toList $ fmap (snd . pos) bs
    bouysH = snd $ pos $ V.head bs
    bi = findBouyI (bouyPos b) bs
    dupB = bs V.! bi
    newBs = V.take bi bs <> V.fromList [dupB, b, dupB] <> V.drop (bi+1) bs
    newEs = sort $ shiftCrosses bi 2 (removeBrokenCircleEvent bi es) ++ newCircleEventsAt newBs [bi, bi+2] -- Could do this without re-sorting for better performance
    newEsOnStart = sort $ es ++ newCircleEventsAt newBs [bi, bi+1, bi+2]

removeBrokenCircleEvent :: Int -> [Event] -> [Event]
removeBrokenCircleEvent bi = filter (\case CrossEvent (Cross _ ci) -> ci /= bi; _ -> True)

newCircleEventsAt :: V.Vector Bouy -> [Int] -> [Event]
newCircleEventsAt bs is = mapMaybe (fmap CrossEvent . crossFrom3 bs) (filter notOnEdge is)
  where notOnEdge i = i > 0 && i < length bs - 1

crossFrom3 :: V.Vector Bouy -> Int -> Maybe Cross
crossFrom3 bs bi
  | anyEq [i1, i2, i3] = Nothing --not 3 different bouys
  | otherwise = case turnDirection p1 p2 p3 of
    Just CounterClockwise -> Nothing
    Nothing -> Nothing -- Colinear
    _ -> case circleFrom3 p1 p2 p3 of
      Nothing -> Nothing --colinear points
      Just c ->
        if crossContainsBouy (Cross c bi) bs
        then Nothing -- Contains other bouy
        else Just $ Cross c bi
  where
    Bouy p1 i1 = bs V.! (bi-1)
    Bouy p2 i2 = bs V.! bi
    Bouy p3 i3 = bs V.! (bi+1)
      
findBouyI :: Position -> V.Vector Bouy -> Int
findBouyI (px,py) bs
  | V.null bs = error "Searching for bouy in empty bouy list"
  | otherwise = findBouyI' px $ parabolaCrossXs py $ V.toList $ fmap bouyPos bs
findBouyI' :: Double -> [Double] -> Int
findBouyI' _ [] = 0
findBouyI' x (cx:xs) = case compare x cx of
  LT -> 0
  EQ -> 0
  GT -> 1 + findBouyI' x xs

parabolaCrossXs :: Double -> [Position] -> [Double]
parabolaCrossXs _ [] = []
parabolaCrossXs _ [_] = []
parabolaCrossXs sw bs = zipWith (parabolaCrossX sw) bs (tail bs)

parabolaCrossX :: Double -> Position -> Position -> Double
parabolaCrossX sw p q = fst $ parabolaCross sw p q

parabolaCross :: Double -> Position -> Position -> Position
parabolaCross sw p q = case crossPointsFromFoci sw p q of
  NoCross -> error "All Bouy parabolas should have at least one cross point"
  OneCross c -> c
  TwoCross lc rc -> case compare (snd p) (snd q) of
    LT -> rc
    EQ -> error "Shouldn't happen I guess"
    GT -> lc
  InfinteCross -> error "Identical Bouys should not be in a voronoi"
